# shared/cache.R — Generic DB cache read/write helpers
#
# Daily cache pattern used by both macro_context and swing_scanner.
# Reads/writes timestamped data to avoid redundant Yahoo API calls.

#' Read cached data for today
#' @param table_name DB table name (e.g. "macro_context_cache")
#' @param date Character date string (default: today)
#' @return data.frame or NULL if no cache for today
cache_read <- function(table_name, date = as.character(Sys.Date())) {
  conn <- Tdata::safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  result <- tryCatch(
    DBI::dbGetQuery(conn,
      sprintf("SELECT * FROM %s WHERE cache_date = ?", table_name),
      params = list(date)),
    error = function(e) NULL)

  if (!is.null(result) && nrow(result) > 0) {
    message(sprintf("Cache hit: %s (%d rows)", table_name, nrow(result)))
    return(result)
  }
  NULL
}

#' Write data to cache, replacing old entries
#' @param table_name DB table name
#' @param data data.frame to cache (cache_date column will be added)
#' @param date Character date string (default: today)
cache_write <- function(table_name, data, date = as.character(Sys.Date())) {
  conn <- Tdata::safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  # Remove old cache entries
  tryCatch(
    DBI::dbExecute(conn,
      sprintf("DELETE FROM %s WHERE cache_date < ?", table_name),
      params = list(date)),
    error = function(e) NULL)

  # Add cache_date and write
  data$cache_date <- date
  DBI::dbWriteTable(conn, table_name, data, append = TRUE)
  message(sprintf("Cache written: %s (%d rows)", table_name, nrow(data)))
}

#' Replace the cached rows of the tickers in `data` from its first date on
#' (a re-fetch that filled in closes Yahoo had left empty)
#' @param table_name DB table name
#' @param data data.frame with ticker and date columns, same layout as the cache
#' @param date Character date string (default: today)
cache_replace_recent <- function(table_name, data, date = as.character(Sys.Date())) {
  conn <- Tdata::safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  from <- as.numeric(min(as.Date(data$date)))   # Date columns are stored as days since 1970
  for (tk in unique(data$ticker))
    DBI::dbExecute(conn, sprintf("DELETE FROM %s WHERE cache_date = ? AND ticker = ? AND date >= ?", table_name),
                   params = list(date, tk, from))
  data$cache_date <- date
  DBI::dbWriteTable(conn, table_name, data, append = TRUE)
  message(sprintf("Cache rows replaced: %s (%d tickers, %d rows)", table_name, length(unique(data$ticker)), nrow(data)))
}

#' Append new rows to existing cache (no delete)
#' @param table_name DB table name
#' @param data data.frame to append (cache_date column will be added)
#' @param date Character date string (default: today)
cache_append <- function(table_name, data, date = as.character(Sys.Date())) {
  conn <- Tdata::safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  data$cache_date <- date
  DBI::dbWriteTable(conn, table_name, data, append = TRUE)
  message(sprintf("Cache appended: %s (+%d rows)", table_name, nrow(data)))
}
