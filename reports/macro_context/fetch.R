# fetch.R — Yahoo data fetch + DB caching for macro tickers
#
# Fetches VIX complex, rates, dollar/commodities, and sector ETFs.
# Uses daily cache in DB table "macro_context_cache".

source(file.path(SCRIPT_DIR, "..", "shared", "cache.R"))

CACHE_TABLE <- "macro_context_cache"

#' Universe symbol -> Yahoo symbol (Tickers.YahooName when set, else the symbol).
#' Tdata::getYahooData maps only 3-letter names and returns their rows under the Yahoo
#' name (FXC -> FXC.SW), so the cache check below never found FXC and appended it again
#' on every run (four copies on 2026-10-08); EUR.CHF went to Yahoo unmapped and came
#' back empty. Fetching under the Yahoo name and renaming back keeps the universe symbol.
yahoo_names <- function(tickers) {
  conn <- Tdata::safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  t <- DBI::dbGetQuery(conn, sprintf("SELECT Name, YahooName FROM Tickers WHERE Name IN (%s)",
                                     paste(rep("?", length(tickers)), collapse = ",")), params = as.list(tickers))
  t <- t[!is.na(t$YahooName) & nzchar(t$YahooName) & !duplicated(t$Name), ]
  yh <- stats::setNames(tickers, tickers)
  yh[t$Name] <- t$YahooName
  yh
}

fetch_yahoo_as_universe <- function(tickers, from_date) {
  yh <- yahoo_names(tickers)
  yh <- yh[!duplicated(yh)]
  d <- Tdata::getYahooData(tickers = unname(yh), from_date = from_date, to_date = Sys.Date())
  if (is.null(d) || nrow(d) == 0) return(d)
  back <- stats::setNames(names(yh), yh)
  hit <- d$ticker %in% names(back)
  d$ticker[hit] <- back[d$ticker[hit]]
  d
}

#' Fetch macro data (cached daily)
#' @param tickers Character vector of tickers to fetch
#' @return data.frame with columns: ticker, date, Open, High, Low, Close, Volume
fetch_macro_data <- function(tickers) {
  today <- as.character(Sys.Date())

  # Try cache first
  cached <- cache_read(CACHE_TABLE, today)
  if (!is.null(cached)) {
    cached$cache_date <- NULL
    cached$date <- as.Date(cached$date)
    cached <- cached[!duplicated(cached[c("ticker", "date")]), ]
    # Tickers added since today's cache was written: fetch and append them
    add <- setdiff(tickers, unique(cached$ticker))
    if (length(add)) {
      extra <- tryCatch(fetch_yahoo_as_universe(add, Sys.Date() - 90), error = function(e) NULL)
      if (!is.null(extra) && nrow(extra) > 0) {
        cache_append(CACHE_TABLE, extra, today)
        cached <- rbind(cached, extra[, names(cached)])
      }
    }
    return(cached)
  }

  # Fetch from Yahoo
  message("Fetching market data...")
  raw <- tryCatch(
    fetch_yahoo_as_universe(tickers, Sys.Date() - 90),
    error = function(e) { message("ERROR: ", e$message); NULL })

  if (!is.null(raw) && nrow(raw) > 0) {
    cache_write(CACHE_TABLE, raw, today)
  }

  raw
}

#' Front and second monthly VIX futures (VX1, VX2) from CBOE daily settlements
#'
#' CBOE's settlement CSV lists the weekly VX contracts at the front monthly's
#' price, so only monthly contracts (VX/<month><year>) are read. Walks back from
#' today to the last `n` settlement dates; weekends and holidays have no file.
#' @param n Number of settlement dates to return (2 gives the 1d change)
#' @param max_back Calendar days to search back
#' @return data.frame(date, vx1_sym, vx1, vx2_sym, vx2), newest first; 0 rows if unavailable
fetch_vx_curve <- function(n = 2, max_back = 10) {
  out <- list()
  for (d in as.list(Sys.Date() - 0:max_back)) {
    u <- sprintf("https://www.cboe.com/us/futures/market_statistics/settlement/csv?dt=%s", format(d))
    s <- tryCatch(read.csv(url(u), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.null(s) || nrow(s) == 0) next
    m <- s[s$Product == "VX" & grepl("^VX/[A-Z][0-9]$", s$Symbol) & as.Date(s[["Expiration Date"]]) > d, ]
    m <- m[order(as.Date(m[["Expiration Date"]])), ]
    if (nrow(m) < 2) next
    out[[length(out) + 1]] <- data.frame(date = d, vx1_sym = m$Symbol[1], vx1 = m$Price[1],
                                         vx2_sym = m$Symbol[2], vx2 = m$Price[2])
    if (length(out) == n) break
  }
  if (length(out) == 0) message("VX futures: no CBOE settlement found in the last ", max_back, " days")
  do.call(rbind, c(list(data.frame(date = as.Date(character()), vx1_sym = character(), vx1 = numeric(),
                                   vx2_sym = character(), vx2 = numeric())), out))
}

# ── FRED: yield decomposition, 2-year yield, fed funds ─────────────────────
FRED_CACHE <- "macro_fred_cache"

#' FRED series used by analyze_rates(): series id -> label
FRED_SERIES <- c(DGS2 = "2Y yield", DGS10 = "10Y yield", DFII10 = "10Y real yield (TIPS)",
                 T10YIE = "10Y breakeven inflation", THREEFYTP10 = "10Y term premium (Kim-Wright)",
                 DFF = "Effective fed funds")

#' Daily FRED series from the public graph CSV (no API key), cached daily in the DB.
#' Rows are kept from the last business days of the previous year on, for the
#' year-to-date changes. Holidays come back empty and are dropped.
#' @return data.frame(ticker, date, Close) with ticker = FRED series id; NULL if nothing came back
fetch_fred <- function(ids = names(FRED_SERIES), from = as.Date(sprintf("%d-12-01", as.integer(format(Sys.Date(), "%Y")) - 1)),
                       cache = FRED_CACHE) {
  today <- as.character(Sys.Date())
  cached <- cache_read(cache, today)
  if (!is.null(cached)) {
    cached$cache_date <- NULL
    cached$date <- as.Date(cached$date)
    return(cached)
  }
  out <- lapply(ids, function(id) {
    u <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=%s", id, format(from))
    s <- tryCatch(read.csv(url(u), stringsAsFactors = FALSE, na.strings = c("", ".")), error = function(e) NULL)
    if (is.null(s) || ncol(s) < 2) { message("FRED ", id, ": no data"); return(NULL) }
    d <- data.frame(ticker = id, date = as.Date(s[[1]]), Close = suppressWarnings(as.numeric(s[[2]])))
    d[!is.na(d$Close), ]
  })
  d <- do.call(rbind, out)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  cache_write(cache, d, today)
  d
}

#' US high-yield spread (ICE BofA OAS) for the Risk badge: 20-session z on 252 sessions of history, so
#' about two years are fetched (FRED keeps three). Own cache table, so the rates cache keeps its short window.
fetch_hy_spread <- function() {
  d <- tryCatch(fetch_fred("BAMLH0A0HYM2", from = Sys.Date() - 760, cache = "macro_fred_hy_cache"),
                error = function(e) { message("FRED high-yield spread failed: ", conditionMessage(e)); NULL })
  if (is.null(d)) return(NULL)
  d[d$ticker == "BAMLH0A0HYM2", c("date", "Close")]
}
