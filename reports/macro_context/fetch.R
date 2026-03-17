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
