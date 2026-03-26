# fetch.R — Yahoo data fetch + DB caching for scanner universe

source(file.path(dirname(sys.frame(1)$ofile), "..", "shared", "cache.R"))

CACHE_TABLE <- "mrbreakouts_cache"

#' Resolve Symbol → YahooName for tickers where they differ
#' @param symbols Character vector of scanner symbols
#' @return Named character vector: names = YahooName, values = Symbol
resolve_yahoo_names <- function(symbols) {
  yahoo_names <- vapply(symbols, function(s) {
    yn <- tryCatch(Tdata::getYahooName(s), error = function(e) NA_character_)
    if (is.na(yn) || yn == "BASE_CURRENCY") s else yn
  }, character(1), USE.NAMES = FALSE)
  stats::setNames(yahoo_names, symbols)
}

#' Fetch from Yahoo using resolved names, return data with original symbols
#' @param symbols Character vector of scanner symbols
#' @param ... Additional arguments passed to getYahooData
#' @return data.frame with ticker column using original symbols
fetch_yahoo_resolved <- function(symbols, ...) {
  name_map <- resolve_yahoo_names(symbols)
  yahoo_tickers <- unname(name_map)
  # Deduplicate (multiple symbols could map to same Yahoo name)
  yahoo_tickers <- unique(yahoo_tickers)

  raw <- tryCatch(
    Tdata::getYahooData(tickers = yahoo_tickers, ...),
    error = function(e) { message("ERROR fetching tickers: ", e$message); NULL })

  if (!is.null(raw) && nrow(raw) > 0) {
    # Reverse map: YahooName → Symbol
    reverse_map <- stats::setNames(names(name_map), unname(name_map))
    raw$ticker <- ifelse(raw$ticker %in% names(reverse_map),
                         reverse_map[raw$ticker], raw$ticker)
  }
  raw
}

#' Fetch scanner data (cached daily)
#' @param tickers Character vector of tickers to fetch
#' @return data.frame with columns: ticker, date, Open, High, Low, Close, Volume
fetch_scanner_data <- function(tickers) {
  today <- as.character(Sys.Date())

  cached <- cache_read(CACHE_TABLE, today)
  if (!is.null(cached)) {
    cached$cache_date <- NULL
    cached$date <- as.Date(cached$date)

    # Check for tickers missing from cache (newly added to universe)
    cached_tickers <- unique(cached$ticker)
    missing <- setdiff(tickers, cached_tickers)
    if (length(missing) > 0) {
      message(sprintf("Fetching %d new tickers from Yahoo: %s", length(missing), paste(missing, collapse = ", ")))
      new_data <- fetch_yahoo_resolved(missing, from_date = Sys.Date() - 300, to_date = Sys.Date())
      if (!is.null(new_data) && nrow(new_data) > 0) {
        cache_append(CACHE_TABLE, new_data, today)
        cached <- rbind(cached, new_data)
      }
    }
    return(cached)
  }

  message("Fetching data from Yahoo...")
  raw <- fetch_yahoo_resolved(tickers, from_date = Sys.Date() - 300, to_date = Sys.Date())

  if (!is.null(raw) && nrow(raw) > 0) {
    cache_write(CACHE_TABLE, raw, today)
  }

  raw
}
