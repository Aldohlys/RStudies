# reports/analyze/funnel.R — Phase C.2 Directional Vol Funnel.
#
# Pure measurement: tabulates IV landscape, VRP (both forms), term shape,
# skew/RR, earnings. Produces a 6-row grid + signal tally for the user's
# direction. Does NOT modify any conviction (there is no conviction output).

# ── Mechanical labels (no prescriptive language) ──────────────────────────
.regime_label <- function(ivp, cfg) {
  if (is.na(ivp)) return("unknown")
  if (ivp < cfg$ivp_regime$cheap_max) "cheap"
  else if (ivp > cfg$ivp_regime$rich_min) "rich"
  else "mid"
}

.vrp_log_band <- function(vrp_log, cfg) {
  if (is.na(vrp_log)) return("unavailable")
  b <- cfg$vrp_log_bands
  if (vrp_log <= b$neg_max) "negative (IV<RV)"
  else if (vrp_log <= b$mild_max) "mildly positive"
  else if (vrp_log <= b$moderate_max) "moderately positive"
  else if (vrp_log <= b$strong_max) "strongly positive"
  else "very strongly positive"
}

.term_shape <- function(iv_by_dte) {
  ok <- !is.na(iv_by_dte) & iv_by_dte > 0
  if (sum(ok) < 2) return("unknown")
  v <- iv_by_dte[ok]
  d <- diff(v)
  if (all(d >= -0.5) && (tail(v, 1) - v[1]) > 1) "contango"
  else if (all(d <= 0.5) && (v[1] - tail(v, 1)) > 1) "backwardation"
  else if (length(v) >= 3 && which.max(v) %in% c(1, length(v)) &&
           min(v) < min(v[c(1, length(v))])) "U-shape"
  else if (length(v) >= 3 && which.max(v) %in% 2:(length(v)-1)) "hump"
  else "flat"
}

.rr_label <- function(rr) {
  if (is.na(rr)) "unavailable"
  else if (abs(rr) < 1) "flat"
  else if (rr > 0) "calls bid (positive RR)"
  else "puts bid (negative RR)"
}

# ── Funnel data pull ──────────────────────────────────────────────────────
run_funnel_deep_dive <- function(ticker, direction, v5_row, config) {
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  on.exit(if (!is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)

  # Pull most-recent Prices snapshot for IV/RV
  prices_row <- if (!is.null(conn)) {
    tryCatch(DBI::dbGetQuery(conn,
      "SELECT datetime, price, iv30, iv90, rv30, vrp, ivr, ivp, ivp_2y
       FROM Prices WHERE sym = ? ORDER BY ROWID DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
  } else NULL

  iv30 <- if (!is.null(prices_row) && nrow(prices_row) > 0) prices_row$iv30 else NA_real_
  iv90 <- if (!is.null(prices_row) && nrow(prices_row) > 0) prices_row$iv90 else NA_real_
  rv30 <- if (!is.null(prices_row) && nrow(prices_row) > 0) prices_row$rv30 else NA_real_
  ivp_used <- if (!is.null(v5_row) && !is.na(v5_row$ivp_used)) as.numeric(v5_row$ivp_used)
              else if (!is.null(prices_row) && nrow(prices_row) > 0) prices_row$ivp
              else NA_real_

  # VRP — both forms
  vrp_log <- if (!is.na(iv30) && !is.na(rv30) && rv30 > 0)
               log(iv30 / rv30) * 100 else NA_real_
  vrp_vp  <- if (!is.na(iv30) && !is.na(rv30))
               (iv30 - rv30) * 100 else NA_real_

  # Term ratio (front vs back)
  term_pct <- if (!is.na(iv30) && !is.na(iv90) && iv90 > 0)
                round((iv30 - iv90) / iv90 * 100, 1) else NA_real_

  # Term shape from Prices alone is binary (front vs back). Mark accordingly.
  term_shape_label <- if (is.na(term_pct)) "unknown"
                      else if (term_pct < -2) "contango"
                      else if (term_pct >  2) "backwardation"
                      else "flat"

  # Skew history → RR mechanical read
  skew_row <- if (!is.null(conn)) {
    tryCatch(DBI::dbGetQuery(conn,
      "SELECT cache_date, iv30, iv90, rv30, call25_iv, put25_iv, skew_25d
       FROM option_skew_history WHERE sym = ?
       ORDER BY cache_date DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
  } else NULL
  rr_vp <- if (!is.null(skew_row) && nrow(skew_row) > 0 &&
               !is.na(skew_row$call25_iv) && !is.na(skew_row$put25_iv))
             (skew_row$call25_iv - skew_row$put25_iv) * 100 else NA_real_

  # Earnings — getNextEarningsDate may return a Date or a YYYYMMDD string
  ed_raw <- tryCatch(Tdata::getNextEarningsDate(ticker), error = function(e) NA)
  ed <- if (inherits(ed_raw, "Date")) ed_raw
        else if (is.character(ed_raw) && nzchar(ed_raw) && !is.na(ed_raw)) {
          tryCatch(as.Date(ed_raw, format = "%Y%m%d"), error = function(e) NA)
        } else NA
  ed_dte <- if (inherits(ed, "Date") && !is.na(ed))
              as.integer(ed - Sys.Date()) else NA_integer_

  # ── Build funnel grid (6 rows, mechanical) ──────────────────────────────
  regime <- .regime_label(ivp_used, config)
  vrp_band <- .vrp_log_band(vrp_log, config)
  rr_lab <- .rr_label(rr_vp)

  rows <- list(
    list(signal = "IV Rank 1Y",
         reading = if (is.na(ivp_used)) "n/a" else sprintf("%.1f%%", ivp_used),
         label   = regime),
    list(signal = "VRP",
         reading = if (is.na(vrp_log)) "n/a"
                   else sprintf("log %+.1f / %+.1fvp", vrp_log, vrp_vp),
         label   = vrp_band),
    list(signal = "Term IV30/IV90",
         reading = if (is.na(term_pct)) "n/a" else sprintf("%+.1f%%", term_pct),
         label   = term_shape_label),
    list(signal = "Skew (RR 25Δ)",
         reading = if (is.na(rr_vp)) "n/a" else sprintf("%+.1f vp", rr_vp),
         label   = rr_lab),
    list(signal = "Earnings",
         reading = if (is.na(ed_dte)) "n/a" else sprintf("%s (%dd)", as.character(ed), ed_dte),
         label   = if (is.na(ed_dte)) "unavailable"
                   else if (ed_dte <= 0) "today/past"
                   else if (ed_dte <= config$earnings_window_days) "within event window"
                   else "outside event window"),
    list(signal = "Sector x-rank",
         reading = "see Phase B sector_rs_rank",
         label   = "passthrough")
  )

  # ── Funnel tally for the user's direction (buy-premium read) ────────────
  # Favorable for buying premium on the named side: IVP cheap, VRP<=0,
  # contango, RR aligned with direction, no earnings within window.
  fav <- 0L; unfav <- 0L; unav <- 0L

  # IV Rank
  if (is.na(ivp_used)) unav <- unav + 1L
  else if (regime == "cheap") fav <- fav + 1L
  else unfav <- unfav + 1L

  # VRP
  if (is.na(vrp_log)) unav <- unav + 1L
  else if (vrp_log <= 0) fav <- fav + 1L
  else unfav <- unfav + 1L

  # Term
  if (is.na(term_pct)) unav <- unav + 1L
  else if (term_pct < 0) fav <- fav + 1L
  else unfav <- unfav + 1L

  # Skew RR — direction-dependent
  if (is.na(rr_vp)) unav <- unav + 1L
  else {
    if (direction == "long"  && rr_vp > 0) fav <- fav + 1L
    else if (direction == "short" && rr_vp < 0) fav <- fav + 1L
    else unfav <- unfav + 1L
  }

  # Earnings
  if (is.na(ed_dte)) unav <- unav + 1L
  else if (ed_dte <= config$earnings_window_days && ed_dte > -7) unfav <- unfav + 1L
  else fav <- fav + 1L

  # Sector — not counted; user reads from Phase B
  unav <- unav + 1L

  list(
    grid = rows,
    iv30 = iv30, iv90 = iv90, rv30 = rv30, ivp_used = ivp_used,
    vrp_log = vrp_log, vrp_vp = vrp_vp,
    term_pct = term_pct, term_shape = term_shape_label,
    rr_vp = rr_vp,
    earnings_date = ed, earnings_dte = ed_dte,
    regime = regime,
    tally = list(favorable = fav, unfavorable = unfav, unavailable = unav)
  )
}
