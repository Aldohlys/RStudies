# fetch.R — Yahoo data fetch + DB caching for scanner universe

source(file.path(dirname(sys.frame(1)$ofile), "..", "shared", "cache.R"))

CACHE_TABLE <- "mrbreakouts_cache"

#' Fetch scanner data (cached daily)
#' @param tickers Character vector of tickers to fetch
#' @return data.frame with columns: ticker, date, Open, High, Low, Close, Volume
fetch_scanner_data <- function(tickers) {
  today <- as.character(Sys.Date())

  cached <- cache_read(CACHE_TABLE, today)
  if (!is.null(cached)) {
    cached$cache_date <- NULL
    cached$date <- as.Date(cached$date)
    return(cached)
  }

  message("Fetching data from Yahoo...")
  raw <- tryCatch(
    Tdata::getYahooData(tickers = tickers, from_date = Sys.Date() - 300, to_date = Sys.Date()),
    error = function(e) { message("ERROR: ", e$message); NULL })

  if (!is.null(raw) && nrow(raw) > 0) {
    cache_write(CACHE_TABLE, raw, today)
  }

  raw
}
