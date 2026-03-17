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
