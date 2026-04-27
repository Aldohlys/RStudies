# universe_filter.R — Phase A: Rich-options universe gate (Step A.1)
#
# A stock enters the scanner universe only if both hold:
#   1. Weekly options listed within next 14 days
#   2. ATM bid/ask width <= 12% on nearest monthly cycle (end-of-day snapshot)
#
# Refresh cadence: weekly. End-of-day data only — live quotes during pre-market
# or thin sessions are unreliable.
#
# Source: Tdata::getExpirationDates() + Tdata::getOptionStrikes() + Prices table
# for the ATM bid/ask. If the rich-universe row is older than 7 days for a
# ticker, the gate is re-evaluated; otherwise the cached state is reused.

#' Evaluate the rich-options universe gate for a list of tickers.
#'
#' @param tickers character vector
#' @param conn DBI connection (used to read/write scanner_rich_universe table)
#' @param force_refresh logical: if TRUE, bypass cache and re-evaluate all
#' @return data.frame: sym, has_weekly, atm_bid_ask_pct, passes_gate, reason
evaluate_rich_universe <- function(tickers, conn, force_refresh = FALSE) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  cutoff <- format(Sys.Date() - 7, "%Y-%m-%d")

  cached <- tryCatch(
    DBI::dbGetQuery(conn,
      sprintf("SELECT * FROM scanner_rich_universe
               WHERE cache_date >= '%s' AND sym IN (%s)",
              cutoff,
              paste(sprintf("'%s'", tickers), collapse = ","))),
    error = function(e) data.frame())

  results <- data.frame(
    sym = tickers,
    has_weekly = NA, atm_bid_ask_pct = NA_real_,
    passes_gate = NA, reason = NA_character_,
    stringsAsFactors = FALSE)

  for (i in seq_along(tickers)) {
    t <- tickers[i]
    if (!force_refresh && nrow(cached) > 0) {
      hit <- cached[cached$sym == t, ][1, , drop = FALSE]
      if (!is.na(hit$sym)) {
        results[i, c("has_weekly", "atm_bid_ask_pct", "passes_gate", "reason")] <-
          hit[c("has_weekly", "atm_bid_ask_pct", "passes_gate", "reason")]
        next
      }
    }

    # Fresh evaluation
    eval_res <- tryCatch(.evaluate_one_rich(t), error = function(e)
      list(has_weekly = FALSE, atm_bid_ask_pct = NA_real_,
           passes_gate = FALSE, reason = paste("eval error:", e$message)))
    results[i, "has_weekly"]      <- as.integer(eval_res$has_weekly)
    results[i, "atm_bid_ask_pct"] <- eval_res$atm_bid_ask_pct
    results[i, "passes_gate"]     <- as.integer(eval_res$passes_gate)
    results[i, "reason"]          <- eval_res$reason

    DBI::dbExecute(conn,
      "INSERT OR REPLACE INTO scanner_rich_universe
         (cache_date, sym, has_weekly, atm_bid_ask_pct, passes_gate, reason)
       VALUES (?, ?, ?, ?, ?, ?)",
      params = list(today, t,
                    as.integer(eval_res$has_weekly),
                    eval_res$atm_bid_ask_pct,
                    as.integer(eval_res$passes_gate),
                    eval_res$reason))
  }
  results
}

#' Per-ticker gate evaluation. Returns list with has_weekly, atm_bid_ask_pct,
#' passes_gate, reason.
#'
#' Until the daily post-close fetch (see project_swing_scanner_redesign.md
#' implementation step 4) populates the option_skew_history /
#' option_chain_oi_history tables and a derivable ATM bid/ask snapshot, this
#' gate is permissive: PASSES by default and logs a warning. The downstream
#' phases (B/C/D) will still cull names that lack the option data they need.
.evaluate_one_rich <- function(sym) {
  # getExpirationDates lives in Python module; access via Tdata's active binding.
  expiries <- tryCatch(Tdata:::tdata_py$getExpirationDates(sym),
                       error = function(e) NULL)
  if (is.null(expiries) || length(expiries) == 0) {
    # TWS unavailable or expiry cache empty — permissive pass, downstream culls.
    return(list(has_weekly = NA, atm_bid_ask_pct = NA_real_,
                passes_gate = TRUE, reason = "no expiry data — passing by default"))
  }
  exp_dates <- as.Date(as.character(expiries), format = "%Y%m%d")
  has_weekly <- any(!is.na(exp_dates) &
                    exp_dates >= Sys.Date() &
                    exp_dates <= Sys.Date() + 14)

  # ATM bid/ask not yet wired — pass on weekly presence alone.
  passes <- isTRUE(has_weekly)
  reason <- if (passes) "weekly present (b/a check pending data)"
            else        "no weekly in 14d"

  list(has_weekly = has_weekly, atm_bid_ask_pct = NA_real_,
       passes_gate = passes, reason = reason)
}
