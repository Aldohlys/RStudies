# reports/shared/freshness.R — staleness checks for cached scanner data.
#
# Policy: /analyze treats cached data (scanner CSV, DB Prices, DB
# option_skew_history, DB option_chain_oi_history) as authoritative *only* if
# its timestamp is within SCANNER_DATA_MAX_AGE_HOURS of now. Beyond that, the
# value is treated as if it were NA — every downstream consumer falls back to
# its existing live-fetch path (IBKR via tdata_py / yfinance).
#
# CLI overrides (resolve_freshness_policy):
#   --refresh             force-live everything (max_age = 0)
#   --max-age <hours>     numeric, overrides default
#   default               SCANNER_DATA_MAX_AGE_HOURS (12h)

SCANNER_DATA_MAX_AGE_HOURS <- 12L

#' Resolve the freshness policy from parsed CLI args.
#' @param argv character vector — typically commandArgs(trailingOnly=TRUE)
#' @return list(max_age_hours = numeric, force_refresh = logical)
resolve_freshness_policy <- function(argv) {
  if ("--refresh" %in% argv) {
    return(list(max_age_hours = 0, force_refresh = TRUE))
  }
  ix <- which(argv == "--max-age")
  if (length(ix) > 0 && length(argv) > ix[1]) {
    h <- suppressWarnings(as.numeric(argv[ix[1] + 1]))
    if (!is.na(h) && h >= 0) return(list(max_age_hours = h, force_refresh = h == 0))
  }
  list(max_age_hours = SCANNER_DATA_MAX_AGE_HOURS, force_refresh = FALSE)
}

#' Compute age in hours of a given POSIXct/Date timestamp relative to now.
#' Returns Inf when ts is NULL/NA/invalid. Tolerant of multiple string formats
#' commonly produced by SQLite TEXT columns ("YYYY-MM-DD HH:MM:SS",
#' "YYYY-MM-DD", ISO 8601 with T separator).
hours_since <- function(ts) {
  if (is.null(ts) || length(ts) == 0) return(Inf)
  ts <- ts[1]
  if (is.na(ts)) return(Inf)
  if (is.character(ts)) {
    fmts <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M",
              "%Y-%m-%d",
              "%Y%m%d %H:%M:%S", "%Y%m%d %H:%M", "%Y%m%d",
              "%Y/%m/%d %H:%M:%S", "%Y/%m/%d")
    parsed <- NA
    for (fmt in fmts) {
      p <- tryCatch(as.POSIXct(ts, format = fmt, tz = "UTC"),
                    error = function(e) NA, warning = function(w) NA)
      if (!is.na(p)) { parsed <- p; break }
    }
    ts <- parsed
  }
  if (inherits(ts, "Date")) ts <- as.POSIXct(ts, tz = "UTC")
  if (!inherits(ts, "POSIXct") || is.na(ts)) return(Inf)
  as.numeric(difftime(Sys.time(), ts, units = "hours"))
}

#' Check whether a cached value is fresh enough to use.
#' @param ts timestamp on the cached row (POSIXct, Date, or "YYYY-MM-DD" / ISO string)
#' @param policy list as returned by resolve_freshness_policy()
#' @return logical — TRUE if usable, FALSE if stale (or force_refresh)
is_fresh <- function(ts, policy) {
  if (isTRUE(policy$force_refresh)) return(FALSE)
  hours_since(ts) <= policy$max_age_hours
}

#' Find mtime of the latest scanner CSV in `out_dir` (NULL if none).
scanner_csv_mtime <- function(out_dir) {
  files <- list.files(out_dir, pattern = "^swing_scanner_\\d{8}\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files <- files[order(files, decreasing = TRUE)]
  file.info(files[1])$mtime
}
