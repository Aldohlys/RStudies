# breadth.R — S&P 500 breadth calculation (% stocks > MA50)
#
# Parallel scrape of S&P 500 constituents + MA50 check.
# Uses daily cache in DB table "macro_breadth_cache".

source(file.path(SCRIPT_DIR, "..", "shared", "cache.R"))

BREADTH_TABLE <- "macro_breadth_cache"

#' Get S&P 500 constituent tickers from Wikipedia
.get_sp500_tickers <- function() {
  tryCatch({
    rvest::read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") |>
      rvest::html_element("#constituents") |>
      rvest::html_table() |>
      dplyr::pull(Symbol) |>
      gsub(pattern = "\\.", replacement = "-")
  }, error = function(e) {
    message("WARN: S&P500 list unavailable: ", e$message)
    NULL
  })
}

#' Check if a single ticker's last close is above its 50-day MA
.is_above_ma50 <- function(ticker, lookback_days = 80) {
  tryCatch({
    df <- Tdata::getYahooData(ticker,
      from_date = Sys.Date() - lookback_days, to_date = Sys.Date())
    closes <- na.omit(df$Close)
    if (length(closes) < 50) return(NA_real_)
    as.numeric(tail(closes, 1) > mean(tail(closes, 50)))
  }, error = function(e) NA_real_, warning = function(w) NA_real_)
}

#' Calculate breadth (cached daily)
#' @return list with pct, n_above, n_valid, elapsed
calculate_breadth <- function() {
  today <- as.character(Sys.Date())

  # Try cache
  cached <- cache_read(BREADTH_TABLE, today)
  if (!is.null(cached)) {
    return(list(
      pct = cached$pct[1], n_above = cached$n_above[1],
      n_valid = cached$n_valid[1], elapsed = cached$elapsed[1]))
  }

  # Compute
  tickers <- .get_sp500_tickers()
  if (is.null(tickers)) {
    return(list(pct = NA_real_, n_above = NA_integer_,
                n_valid = NA_integer_, elapsed = NA))
  }

  n_cores <- min(6, max(1, parallel::detectCores() - 1))
  message(sprintf("Breadth: %d tickers | %d cores...", length(tickers), n_cores))

  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  # Isolate TEMP per worker to prevent conda temp file race condition
  # (each worker's library(Tdata) loads reticulate which probes conda)
  parallel::clusterEvalQ(cl, {
    Sys.setenv(TEMP = tempdir(), TMP = tempdir())
    library(Tdata)
  })
  parallel::clusterExport(cl, ".is_above_ma50", envir = environment())

  t0 <- Sys.time()
  results <- parallel::parSapply(cl, tickers, .is_above_ma50, USE.NAMES = TRUE)
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)

  n_above <- sum(results == 1, na.rm = TRUE)
  n_valid <- n_above + sum(results == 0, na.rm = TRUE)
  pct <- if (n_valid > 0) round(n_above / n_valid * 100, 1) else NA_real_

  # Cache result
  cache_write(BREADTH_TABLE,
    data.frame(pct = pct, n_above = n_above, n_valid = n_valid, elapsed = elapsed))

  list(pct = pct, n_above = n_above, n_valid = n_valid, elapsed = elapsed)
}
