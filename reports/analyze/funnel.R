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

# ── Provenance helpers ─────────────────────────────────
# A metric is unavailable for two distinct reasons; carry that distinction.
.STATUS_RANK <- c("LIVE" = 0L, "CACHED" = 1L, "NO DATA" = 2L, "FETCH FAILED" = 3L)

#' Rank of a provenance status (0 = best). Unknown statuses rank worst (3).
#' Single-bracket index returns NA for unknown names (no out-of-bounds error).
.status_rank <- function(s) {
  r <- unname(.STATUS_RANK[s %||% "FETCH FAILED"])
  if (is.na(r)) 3L else r
}

#' Worse (more-degraded) of two provenance statuses. Used to fold a composite
#' metric's provenance (e.g. VRP needs both IV30 and RV30).
.worse_status <- function(a, b) {
  a <- a %||% "FETCH FAILED"; b <- b %||% "FETCH FAILED"
  if (.status_rank(a) >= .status_rank(b)) a else b
}

#' Provenance status for a composite metric: if the value resolved, inherit the
#' better component's status; otherwise report the worse component's status.
.metric_status <- function(value, status_a, status_b = NULL) {
  if (!is.na(value)) {
    if (is.null(status_b)) return(status_a %||% "LIVE")
    # value present -> the better (less degraded) of the two sources
    if (.status_rank(status_a) <= .status_rank(status_b)) return(status_a)
    return(status_b)
  }
  if (is.null(status_b)) return(status_a %||% "FETCH FAILED")
  .worse_status(status_a, status_b)
}

# ── Mechanical labels (no prescriptive language) ──────────────────────────
# When the value is NA the label falls back to the provenance status so the
# grid never lies about *why* a cell is blank (NO DATA vs FETCH FAILED).
.regime_label <- function(ivp, cfg, status = "FETCH FAILED") {
  if (is.na(ivp)) return(status)
  if (ivp < cfg$ivp_regime$cheap_max) "cheap"
  else if (ivp > cfg$ivp_regime$rich_min) "rich"
  else "mid"
}

.vrp_log_band <- function(vrp_log, cfg, status = "FETCH FAILED") {
  if (is.na(vrp_log)) return(status)
  b <- cfg$vrp_log_bands
  if (vrp_log <= b$neg_max) "negative (IV<RV)"
  else if (vrp_log <= b$mild_max) "mildly positive"
  else if (vrp_log <= b$moderate_max) "moderately positive"
  else if (vrp_log <= b$strong_max) "strongly positive"
  else "very strongly positive"
}

.rr_label <- function(rr, status = "FETCH FAILED") {
  if (is.na(rr)) status
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

  # IV30 / IV90 / RV30 / IVP / RVP / skew via resolvers
  iv30_r <- resolve_iv30(ticker, spot, freshness, tws_ok = tws_ok, conn = conn)
  iv90_r <- resolve_iv90(ticker, spot, freshness, tws_ok = tws_ok, conn = conn)
  rv30_r <- resolve_rv30(ticker, freshness, conn = conn)
  ivp_r  <- resolve_ivp(ticker, freshness, tws_ok = tws_ok, conn = conn)
  rvp_r  <- resolve_rvp(ticker, freshness, tws_ok = tws_ok, conn = conn)
  skew_r <- resolve_skew_25d(ticker, spot, freshness,
                              tws_ok = tws_ok, conn = conn)
  earn_r <- resolve_earnings(ticker)

  iv30 <- iv30_r$value; iv90 <- iv90_r$value
  rv30 <- rv30_r$value; ivp_used <- ivp_r$value
  rvp  <- rvp_r$value

  iv30_reason <- iv30_r$reason; iv90_reason <- iv90_r$reason
  rv30_reason <- rv30_r$reason; ivp_reason  <- ivp_r$reason
  rvp_reason  <- rvp_r$reason

  # Per-resolver provenance status (LIVE / CACHED / NO DATA / FETCH FAILED)
  iv30_status <- iv30_r$status %||% "FETCH FAILED"
  iv90_status <- iv90_r$status %||% "FETCH FAILED"
  rv30_status <- rv30_r$status %||% "FETCH FAILED"
  ivp_status  <- ivp_r$status  %||% "FETCH FAILED"
  rvp_status  <- rvp_r$status  %||% "FETCH FAILED"
  skew_status <- skew_r$status %||% "FETCH FAILED"
  earn_status <- earn_r$status %||% "FETCH FAILED"

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

  # Label set to NA when the value is missing; the grid fills it from the
  # composite term provenance status below.
  term_shape_label <- if (is.na(term_pct)) NA_character_
                      else if (term_pct < -2) "contango"
                      else if (term_pct >  2) "backwardation"
                      else "flat"

  # 25Δ skew → RR vol-points
  rr_vp <- if (is.list(skew_r$value)) skew_r$value$rr_vp else NA_real_
  rr_reason <- skew_r$reason

  ed_date <- if (is.list(earn_r$value)) earn_r$value$date else as.Date(NA)
  ed_dte  <- if (is.list(earn_r$value)) earn_r$value$dte  else NA_integer_
  ed_reason <- earn_r$reason

  # ── Composite-metric provenance ─────────────────────────────────────────
  vrp_status  <- .metric_status(vrp_log,  iv30_status, rv30_status)
  term_status <- .metric_status(term_pct, iv30_status, iv90_status)

  # ── Build funnel grid (6 rows, mechanical) ──────────────────────────────
  regime   <- .regime_label(ivp_used, config, status = ivp_status)
  vrp_band <- .vrp_log_band(vrp_log, config, status = vrp_status)
  rr_lab   <- .rr_label(rr_vp, status = skew_status)

  # Status-aware reading: value when present, else "<STATUS>: <reason>".
  # Distinguishes NO DATA (response empty/NaN) from FETCH FAILED (no response).
  .prov_reading <- function(value, fmt, status, reason) {
    if (!is.na(value)) return(sprintf(fmt, value))
    msg <- if (!is.null(reason) && nzchar(reason)) paste0(status, ": ", reason)
           else status
    msg
  }

  ivp_reading <- if (!is.na(ivp_used)) {
                   src_tag <- if (ivp_r$source == "computed") " (live interp)" else ""
                   sprintf("%.1f%%%s", ivp_used, src_tag)
                 } else .prov_reading(ivp_used, "%.1f%%", ivp_status, ivp_reason)

  ed_label <- if (is.na(ed_dte)) earn_status
              else if (ed_dte <= 0) "today/past"
              else if (ed_dte <= config$earnings_window_days) "within event window"
              else "outside event window"

  rows <- list(
    list(signal = "IV Rank 1Y", status = ivp_status,
         reading = ivp_reading,
         label   = regime),
    list(signal = "VRP", status = vrp_status,
         reading = if (!is.na(vrp_log))
                     sprintf("log %+.1f / %+.1fvp", vrp_log, vrp_vp)
                   else .prov_reading(vrp_log, "%+.1f", vrp_status, vrp_reason),
         label   = vrp_band),
    list(signal = "Term IV30/IV90", status = term_status,
         reading = .prov_reading(term_pct, "%+.1f%%", term_status, term_reason),
         label   = term_shape_label %||% term_status),
    list(signal = "Skew (RR 25Δ)", status = skew_status,
         reading = .prov_reading(rr_vp, "%+.1f vp", skew_status, rr_reason),
         label   = rr_lab),
    list(signal = "Earnings", status = earn_status,
         reading = if (!is.na(ed_dte))
                     sprintf("%s (%dd)", as.character(ed_date), ed_dte)
                   else paste0(earn_status, ": ", ed_reason %||% "no earnings date"),
         label   = ed_label),
    list(signal = "Sector x-rank", status = "LIVE",
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
    rvp = rvp, rvp_reason = rvp_reason,
    vrp_log = vrp_log, vrp_vp = vrp_vp,
    term_pct = term_pct, term_shape = term_shape_label %||% term_status,
    rr_vp = rr_vp,
    earnings_date = ed_date, earnings_dte = ed_dte,
    regime = regime,
    spot = spot,
    sources = list(
      iv30 = iv30_r$source, iv90 = iv90_r$source, rv30 = rv30_r$source,
      ivp = ivp_r$source, skew = skew_r$source, earnings = earn_r$source),
    statuses = list(
      iv30 = iv30_status, iv90 = iv90_status, rv30 = rv30_status,
      ivp = ivp_status, vrp = vrp_status, term = term_status,
      skew = skew_status, earnings = earn_status),
    reasons = list(
      iv30 = iv30_reason, iv90 = iv90_reason, rv30 = rv30_reason,
      ivp = ivp_reason, vrp = vrp_reason, term = term_reason,
      rr = rr_reason, earnings = ed_reason),
    retrieved = retrieved,
    tally = list(favorable = fav, unfavorable = unfav, unavailable = unav)
  )
}
