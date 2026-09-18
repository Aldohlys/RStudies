# reports/shared/weekly.R — weekly bars resampled from daily.
#
# Used by BOT_daily for the weekly half of the daily-vs-weekly read
# (docs/BOT_TOOLS_DESIGN.md section 3). No new data source: weekly bars are a
# resample of the daily series already fetched, so this adds no IBKR or Yahoo
# dependency.
#
# The in-progress week is dropped by default. A partial week has fewer sessions,
# so its High-Low range and Volume are both understated, which biases
# `squeeze_ratio` low (a partial week looks compressed) and `vol_surge` low.

#' Resample daily OHLCV to weekly bars.
#'
#' Week boundaries are ISO weeks (Monday start), taken from `%G-%V` so the
#' year-end boundary does not split a week across two labels.
#'
#' @param d data.frame with date (Date), Open, High, Low, Close, Volume
#' @param drop_partial logical — drop the final week when it is still in
#'   progress, i.e. when the last daily bar is not a Friday and the week has
#'   fewer than 5 sessions. Default TRUE.
#' @return data.frame with the same columns, one row per week, chronological;
#'   NULL when there is nothing to resample.
to_weekly <- function(d, drop_partial = TRUE) {
  if (is.null(d) || !nrow(d)) return(NULL)
  need <- c("date", "Open", "High", "Low", "Close", "Volume")
  if (!all(need %in% names(d))) return(NULL)

  d <- d[order(d$date), , drop = FALSE]
  d <- d[!is.na(d$Close), , drop = FALSE]
  if (!nrow(d)) return(NULL)

  wk <- format(as.Date(d$date), "%G-%V")
  idx <- split(seq_len(nrow(d)), factor(wk, levels = unique(wk)))

  out <- do.call(rbind, lapply(idx, function(i) {
    data.frame(
      date     = d$date[max(i)],           # week's last session
      week     = format(as.Date(d$date[max(i)]), "%G-%V"),
      sessions = length(i),
      Open     = d$Open[min(i)],
      High     = max(d$High[i], na.rm = TRUE),
      Low      = min(d$Low[i],  na.rm = TRUE),
      Close    = d$Close[max(i)],
      Volume   = sum(d$Volume[i], na.rm = TRUE),
      stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL

  if (isTRUE(drop_partial) && nrow(out) > 1) {
    last_day <- as.Date(out$date[nrow(out)])
    # %u: 1 = Monday .. 5 = Friday
    if (as.integer(format(last_day, "%u")) < 5 && out$sessions[nrow(out)] < 5) {
      out <- out[-nrow(out), , drop = FALSE]
    }
  }
  out
}
