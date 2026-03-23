# vol_profile.R — Gate 3 IV assessment from Prices table
#
# Stylized facts from DOW A+ pattern analysis:
#   - IV30 < 40% favorable (winners avg 27.7% vs losers 35.9%)
#   - IVP < 60% = room for expansion
#   - RVP > 30% = stock is moving
#   - VRP context: iv30 - rv30 combined with IVP
#
# If data is stale (>1 day), auto-refresh via Tdata::getVolMetrics() (requires TWS).

library(DBI)

#' Load latest vol profiles for given tickers, auto-refresh stale data
#' @param tickers Character vector of ticker symbols
#' @param conn DBI connection
#' @return data.frame with iv30, iv180, ivp, rv30, rvp per ticker + datetime
load_vol_profiles <- function(tickers, conn) {
  # Fetch latest row per ticker where iv30 is not NULL
  query <- sprintf(
    "SELECT p.* FROM Prices p
     INNER JOIN (
       SELECT sym, MAX(ROWID) as max_id FROM Prices
       WHERE iv30 IS NOT NULL AND sym IN (%s)
       GROUP BY sym
     ) latest ON p.sym = latest.sym AND p.ROWID = latest.max_id",
    paste(sprintf("'%s'", tickers), collapse = ",")
  )

  vol <- tryCatch(dbGetQuery(conn, query), error = function(e) {
    message("vol_profile: query error — ", e$message)
    data.frame()
  })

  # Determine staleness: datetime format is "YYYYMMDD HH:MM"
  # Data from yesterday's close (e.g. 20260318 22:00) is fresh — only refresh if >1 day old
  cutoff_str <- format(Sys.Date() - 1, "%Y%m%d")

  if (nrow(vol) > 0 && "datetime" %in% names(vol)) {
    vol$date_part <- substr(vol$datetime, 1, 8)
    stale_syms <- vol$sym[vol$date_part < cutoff_str]
  } else {
    stale_syms <- character(0)
  }

  # Only auto-refresh STALE data (had data but >1 day old).
  # Missing data (never had IV) stays as NO DATA — flagged for manual TWS update.
  missing_syms <- setdiff(tickers, vol$sym)
  need_refresh <- stale_syms  # NOT missing_syms

  if (length(missing_syms) > 0)
    message(sprintf("Vol data missing for %d tickers (no Prices row) — run getVolMetrics() manually or via TWS",
      length(missing_syms)))

  if (length(need_refresh) > 0) {
    message(sprintf("Vol data stale (>1d) for %d tickers — attempting TWS refresh...",
      length(need_refresh)))
    refreshed <- tryCatch({
      Tdata::getVolMetrics(need_refresh)
    }, error = function(e) {
      message("TWS refresh failed (TWS not connected?): ", e$message)
      NULL
    })

    if (!is.null(refreshed) && nrow(refreshed) > 0) {
      message(sprintf("TWS refreshed %d/%d tickers", nrow(refreshed), length(need_refresh)))
      # Re-query DB after refresh (getVolMetrics appends to Prices)
      vol <- tryCatch(dbGetQuery(conn, query), error = function(e) vol)
      if ("datetime" %in% names(vol)) vol$date_part <- substr(vol$datetime, 1, 8)
    }
  }

  vol
}

#' Evaluate Gate 3 criteria from vol data
#' @param vol_data data.frame from load_vol_profiles()
#' @param tickers All tickers in scanner (to flag missing)
#' @return data.frame with gate3 assessment per ticker
evaluate_gate3 <- function(vol_data, tickers) {
  results <- data.frame(
    Ticker    = tickers,
    IV30      = NA_real_,
    RV30      = NA_real_,
    IVP       = NA_real_,
    RVP       = NA_real_,
    VRP       = NA_real_,
    Optionality     = "NO DATA",
    TermStr   = "NO DATA",
    Vol_Date  = NA_character_,
    stringsAsFactors = FALSE
  )

  if (nrow(vol_data) == 0) return(results)

  for (i in seq_len(nrow(vol_data))) {
    sym <- vol_data$sym[i]
    idx <- which(results$Ticker == sym)
    if (length(idx) == 0) next

    iv30  <- vol_data$iv30[i]
    iv180 <- if ("iv180" %in% names(vol_data)) vol_data$iv180[i] else NA
    ivp   <- vol_data$ivp[i]
    rv30  <- vol_data$rv30[i]
    rvp   <- vol_data$rvp[i]

    # Store as percentages for display (multiply by 100)
    results$IV30[idx] <- if (!is.na(iv30)) round(iv30 * 100, 1) else NA
    results$RV30[idx] <- if (!is.na(rv30)) round(rv30 * 100, 1) else NA
    results$IVP[idx]  <- if (!is.na(ivp)) round(ivp, 1) else NA

    # RVP (realized vol percentile, already 0-100 from DB)
    results$RVP[idx] <- if (!is.na(rvp)) round(rvp, 1) else NA

    # VRP = IV30 - RV30 (in percentage points)
    vrp <- if (!is.na(iv30) && !is.na(rv30)) round((iv30 - rv30) * 100, 1) else NA
    results$VRP[idx] <- vrp

    # Timestamp
    if ("datetime" %in% names(vol_data) && !is.na(vol_data$datetime[i]))
      results$Vol_Date[idx] <- vol_data$datetime[i]

    # Optionality criteria (aligned with composite scoring)
    iv30_pct <- if (!is.na(iv30)) iv30 * 100 else NA
    term_is_contango <- !is.na(iv30) && !is.na(iv180) && iv180 > 0 && iv30 < iv180
    opt_1 <- !is.na(iv30_pct) && iv30_pct < 40       # IV30 level favorable
    opt_2 <- !is.na(ivp) && ivp < 60                  # IVP headroom
    opt_3 <- !is.na(vrp) && vrp < 0                   # Negative VRP = opportunity
    opt_4 <- term_is_contango                          # Contango term structure

    opt_score <- sum(c(opt_1, opt_2, opt_3, opt_4))
    results$Optionality[idx] <- if (opt_score >= 3) "PASS"
                          else if (opt_score == 2) "PARTIAL"
                          else "FAIL"

    # Term structure proxy: iv30 vs iv180
    if (!is.na(iv30) && !is.na(iv180) && iv180 > 0) {
      results$TermStr[idx] <- if (iv30 < iv180) "Contango" else "Backwardation"
    }
  }

  results
}

#' Flag tickers with missing or stale IV data
#' @param gate3_results data.frame from evaluate_gate3()
#' @return Character vector of tickers needing TWS update
flag_missing <- function(gate3_results) {
  missing <- gate3_results$Ticker[gate3_results$Optionality == "NO DATA"]
  if (length(missing) > 0)
    message(sprintf("CHECK TWS — %d tickers still without IV data: %s",
      length(missing), paste(head(missing, 10), collapse = ", ")))
  missing
}
