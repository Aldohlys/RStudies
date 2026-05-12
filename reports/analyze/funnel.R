# reports/analyze/funnel.R — Phase C.2 Directional Vol Funnel.
#
# Pure measurement: tabulates IV landscape, VRP (both forms), term shape,
# skew/RR, earnings. Produces a 6-row grid + signal tally for the user's
# direction. Does NOT modify any conviction (there is no conviction output).
#
# Sourcing (Step 1 inversion 2026-05-12 — see project_analyze_redesign_2026_05):
#   All field reads go through resolvers in shared/live_sources.R. Resolvers
#   escalate DB-cache-if-fresh → live IBKR / yfinance → FETCH FAILED with a
#   precise reason. Scanner-CSV fields no longer consulted at funnel level.
#   IVP gap (UPS 2026-05-11 test) closed by resolve_ivp() via
#   Tdata::getIVPercentileLevels.

# ── Mechanical labels (no prescriptive language) ──────────────────────────
.regime_label <- function(ivp, cfg) {
  if (is.na(ivp)) return("FETCH FAILED")
  if (ivp < cfg$ivp_regime$cheap_max) "cheap"
  else if (ivp > cfg$ivp_regime$rich_min) "rich"
  else "mid"
}

.vrp_log_band <- function(vrp_log, cfg) {
  if (is.na(vrp_log)) return("FETCH FAILED")
  b <- cfg$vrp_log_bands
  if (vrp_log <= b$neg_max) "negative (IV<RV)"
  else if (vrp_log <= b$mild_max) "mildly positive"
  else if (vrp_log <= b$moderate_max) "moderately positive"
  else if (vrp_log <= b$strong_max) "strongly positive"
  else "very strongly positive"
}

.rr_label <- function(rr) {
  if (is.na(rr)) "FETCH FAILED"
  else if (abs(rr) < 1) "flat"
  else if (rr > 0) "calls bid (positive RR)"
  else "puts bid (negative RR)"
}

# ── Funnel data pull ──────────────────────────────────────────────────────
#
# scanner_row argument retained for signature stability but no longer read.
# All values come from resolvers in shared/live_sources.R.
run_funnel_deep_dive <- function(ticker, direction, scanner_row, config,
                                 spot = NA_real_, freshness = NULL) {
  tws_ok <- isTRUE(config$tws_reachable)
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  on.exit(if (!is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)

  # Spot — fall back to resolver if caller didn't provide one
  if (is.na(spot)) {
    s <- resolve_spot(ticker)
    spot <- s$value
  }

  # IV30 / IV90 / RV30 / IVP / skew via resolvers
  iv30_r <- resolve_iv30(ticker, spot, freshness, tws_ok = tws_ok, conn = conn)
  iv90_r <- resolve_iv90(ticker, spot, freshness, tws_ok = tws_ok, conn = conn)
  rv30_r <- resolve_rv30(ticker, freshness, conn = conn)
  ivp_r  <- resolve_ivp(ticker, freshness, tws_ok = tws_ok, conn = conn)
  skew_r <- resolve_skew_25d(ticker, spot, freshness,
                              tws_ok = tws_ok, conn = conn)
  earn_r <- resolve_earnings(ticker)

  iv30 <- iv30_r$value; iv90 <- iv90_r$value
  rv30 <- rv30_r$value; ivp_used <- ivp_r$value

  iv30_reason <- iv30_r$reason; iv90_reason <- iv90_r$reason
  rv30_reason <- rv30_r$reason; ivp_reason  <- ivp_r$reason

  # VRP — both forms
  vrp_log <- if (!is.na(iv30) && !is.na(rv30) && rv30 > 0)
               log(iv30 / rv30) * 100 else NA_real_
  vrp_vp  <- if (!is.na(iv30) && !is.na(rv30))
               (iv30 - rv30) * 100 else NA_real_
  .join_reasons <- function(...) {
    parts <- Filter(function(x) !is.null(x) && !is.na(x) && nzchar(x),
                    list(...))
    if (length(parts) == 0) NULL else paste(unlist(parts), collapse = "; ")
  }
  vrp_reason <- if (!is.na(vrp_log)) NULL
                else .join_reasons(iv30_reason, rv30_reason)

  # Term ratio (front vs back)
  term_pct <- if (!is.na(iv30) && !is.na(iv90) && iv90 > 0)
                round((iv30 - iv90) / iv90 * 100, 1) else NA_real_
  term_reason <- if (!is.na(term_pct)) NULL
                 else .join_reasons(iv30_reason, iv90_reason)

  term_shape_label <- if (is.na(term_pct)) "FETCH FAILED"
                      else if (term_pct < -2) "contango"
                      else if (term_pct >  2) "backwardation"
                      else "flat"

  # 25Δ skew → RR vol-points
  rr_vp <- if (is.list(skew_r$value)) skew_r$value$rr_vp else NA_real_
  rr_reason <- skew_r$reason

  ed_date <- if (is.list(earn_r$value)) earn_r$value$date else as.Date(NA)
  ed_dte  <- if (is.list(earn_r$value)) earn_r$value$dte  else NA_integer_
  ed_reason <- earn_r$reason

  # ── Build funnel grid (6 rows, mechanical) ──────────────────────────────
  regime <- .regime_label(ivp_used, config)
  vrp_band <- .vrp_log_band(vrp_log, config)
  rr_lab <- .rr_label(rr_vp)

  .with_reason <- function(value, reason, fmt) {
    if (!is.na(value)) sprintf(fmt, value)
    else if (!is.null(reason) && nzchar(reason)) paste0("FETCH FAILED: ", reason)
    else "FETCH FAILED"
  }

  ivp_reading <- if (!is.na(ivp_used)) {
                   src_tag <- if (ivp_r$source == "computed") " (live interp)" else ""
                   sprintf("%.1f%%%s", ivp_used, src_tag)
                 } else .with_reason(ivp_used, ivp_reason, "%.1f%%")

  rows <- list(
    list(signal = "IV Rank 1Y",
         reading = ivp_reading,
         label   = regime),
    list(signal = "VRP",
         reading = if (!is.na(vrp_log))
                     sprintf("log %+.1f / %+.1fvp", vrp_log, vrp_vp)
                   else .with_reason(vrp_log, vrp_reason, "%+.1f"),
         label   = vrp_band),
    list(signal = "Term IV30/IV90",
         reading = .with_reason(term_pct, term_reason, "%+.1f%%"),
         label   = term_shape_label),
    list(signal = "Skew (RR 25Δ)",
         reading = .with_reason(rr_vp, rr_reason, "%+.1f vp"),
         label   = rr_lab),
    list(signal = "Earnings",
         reading = if (!is.na(ed_dte))
                     sprintf("%s (%dd)", as.character(ed_date), ed_dte)
                   else paste0("FETCH FAILED: ", ed_reason %||% "no earnings date"),
         label   = if (is.na(ed_dte)) "FETCH FAILED"
                   else if (ed_dte <= 0) "today/past"
                   else if (ed_dte <= config$earnings_window_days) "within event window"
                   else "outside event window"),
    list(signal = "Sector x-rank",
         reading = "see Phase B sector_rs_rank",
         label   = "passthrough")
  )

  # ── Funnel tally for the user's direction (buy-premium read) ────────────
  fav <- 0L; unfav <- 0L; unav <- 0L

  if (is.na(ivp_used)) unav <- unav + 1L
  else if (regime == "cheap") fav <- fav + 1L
  else unfav <- unfav + 1L

  if (is.na(vrp_log)) unav <- unav + 1L
  else if (vrp_log <= 0) fav <- fav + 1L
  else unfav <- unfav + 1L

  if (is.na(term_pct)) unav <- unav + 1L
  else if (term_pct < 0) fav <- fav + 1L
  else unfav <- unfav + 1L

  if (is.na(rr_vp)) unav <- unav + 1L
  else {
    if (direction == "long"  && rr_vp > 0) fav <- fav + 1L
    else if (direction == "short" && rr_vp < 0) fav <- fav + 1L
    else unfav <- unfav + 1L
  }

  if (is.na(ed_dte)) unav <- unav + 1L
  else if (ed_dte <= config$earnings_window_days && ed_dte > -7) unfav <- unfav + 1L
  else fav <- fav + 1L

  unav <- unav + 1L  # Sector — passthrough

  # Retrieval timestamps per source — fed into HTML tooltips
  retrieved <- list(
    prices_db = if (iv30_r$source == "db") as.character(iv30_r$retrieved_at) else NA_character_,
    skew_db   = if (skew_r$source == "db") as.character(skew_r$retrieved_at) else NA_character_,
    live_now  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  list(
    grid = rows,
    iv30 = iv30, iv90 = iv90, rv30 = rv30, ivp_used = ivp_used,
    vrp_log = vrp_log, vrp_vp = vrp_vp,
    term_pct = term_pct, term_shape = term_shape_label,
    rr_vp = rr_vp,
    earnings_date = ed_date, earnings_dte = ed_dte,
    regime = regime,
    spot = spot,
    sources = list(
      iv30 = iv30_r$source, iv90 = iv90_r$source, rv30 = rv30_r$source,
      ivp = ivp_r$source, skew = skew_r$source, earnings = earn_r$source),
    reasons = list(
      iv30 = iv30_reason, iv90 = iv90_reason, rv30 = rv30_reason,
      ivp = ivp_reason, vrp = vrp_reason, term = term_reason,
      rr = rr_reason, earnings = ed_reason),
    retrieved = retrieved,
    tally = list(favorable = fav, unfavorable = unfav, unavailable = unav)
  )
}
