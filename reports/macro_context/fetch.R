# fetch.R — Yahoo data fetch + DB caching for macro tickers
#
# Fetches VIX complex, rates, dollar/commodities, and sector ETFs.
# Uses daily cache in DB table "macro_context_cache".

source(file.path(SCRIPT_DIR, "..", "shared", "cache.R"))

CACHE_TABLE <- "macro_context_cache"

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
    return(cached)
  }

  # Fetch from Yahoo
  message("Fetching market data...")
  raw <- tryCatch(
    Tdata::getYahooData(tickers = tickers, from_date = Sys.Date() - 90, to_date = Sys.Date()),
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
