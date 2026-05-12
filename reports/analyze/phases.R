# reports/analyze/phases.R — Phase A, B, C.1, E orchestration for /analyze.
#
# Loads the latest swing_scanner CSV row for the ticker and exposes its
# Phase A/B/C/D fields. Falls back to live Tdata helpers when CSV emits NA.
# Phase outputs are mechanical — PASS / SKIP / NO SIGNAL / STALE — no advice.

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1) {
    if (is.na(a) || identical(a, "")) return(b)
  }
  a
}

# ── Scanner CSV loader ────────────────────────────────────────────────────
.find_latest_scanner_csv <- function(out_dir = "C:/Users/aldoh/Documents/NewTrading/reports") {
  files <- list.files(out_dir, pattern = "^swing_scanner_\\d{8}\\.csv$",
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files <- files[order(files, decreasing = TRUE)]
  files[1]
}

#' Read the latest scanner CSV row for a ticker.
#' When `freshness` is supplied and the CSV mtime exceeds the policy cutoff,
#' returns row=NULL with stale=TRUE so callers fall back to live fetches.
.read_scanner_row <- function(ticker, freshness = NULL) {
  csv <- .find_latest_scanner_csv()
  if (is.null(csv)) return(list(row = NULL, csv_path = NULL, stale = TRUE))
  mtime <- file.info(csv)$mtime
  stale <- !is.null(freshness) && !is_fresh(mtime, freshness)
  if (stale) return(list(row = NULL, csv_path = csv, stale = TRUE, mtime = mtime))
  df <- tryCatch(
    read.csv2(csv, stringsAsFactors = FALSE, na.strings = c("NA", "")),
    error = function(e) NULL)
  if (is.null(df) || !"sym" %in% names(df))
    return(list(row = NULL, csv_path = csv, stale = FALSE, mtime = mtime))
  hit <- df[df$sym == ticker, , drop = FALSE]
  if (nrow(hit) == 0)
    return(list(row = NULL, csv_path = csv, stale = FALSE, mtime = mtime))
  list(row = hit[1, , drop = FALSE], csv_path = csv, stale = FALSE, mtime = mtime)
}

# ── PHASE A ──────────────────────────────────────────────────────────────
# Step 5 rewrite 2026-05-12: Phase A is INFORMATIONAL only — never SKIPs the
# downstream phases. Live IBKR probe (getExpirationDates + ATM strikes). DB
# scanner_rich_universe cache and scanner CSV are last-resort fallbacks.
run_phase_a <- function(ticker, freshness = NULL) {
  tws_ok <- TRUE  # reachability is checked at module level (CONFIG$tws_reachable),
                  # but the probe call below has its own try/error guards.

  # Live probe — try to fetch expiries
  py <- tryCatch(Tdata:::tdata_py, error = function(e) NULL)
  if (!is.null(py)) {
    expiries <- tryCatch(py$getExpirationDates(ticker),
                         error = function(e) NULL)
    if (is.list(expiries) || (is.character(expiries) && length(expiries) > 1)) {
      n_exp <- length(expiries)
      # Find expiries in the 14-90 DTE window
      exp_dates <- tryCatch(as.Date(as.character(expiries), format = "%Y%m%d"),
                            error = function(e) as.Date(NA))
      dtes <- as.integer(exp_dates - Sys.Date())
      tradeable <- sum(!is.na(dtes) & dtes >= 14 & dtes <= 90, na.rm = TRUE)
      return(list(
        result = "INFO",
        n_expiries = n_exp,
        tradeable_expiries = tradeable,
        source = "live IBKR",
        reason = NULL,
        retrieved_at = Sys.time()
      ))
    }
  }

  # Fallback: DB scanner_rich_universe (recent end-of-day verdict)
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  if (!is.null(conn)) {
    db_row <- tryCatch(DBI::dbGetQuery(conn,
      "SELECT cache_date, reason FROM scanner_rich_universe
       WHERE sym = ? ORDER BY cache_date DESC LIMIT 1",
      params = list(ticker)), error = function(e) NULL)
    DBI::dbDisconnect(conn)
    if (!is.null(db_row) && nrow(db_row) > 0) {
      return(list(
        result = "INFO",
        n_expiries = NA_integer_,
        tradeable_expiries = NA_integer_,
        source = "DB scanner_rich_universe",
        reason = paste("DB:", db_row$reason),
        retrieved_at = db_row$cache_date
      ))
    }
  }

  # Last resort: scanner CSV
  scan <- .read_scanner_row(ticker, freshness)
  if (!is.null(scan$row)) {
    return(list(
      result = "INFO",
      n_expiries = NA_integer_,
      tradeable_expiries = NA_integer_,
      source = "scanner CSV",
      reason = "live + DB unavailable; using scanner CSV",
      retrieved_at = scan$mtime
    ))
  }

  list(result = "INFO",
       n_expiries = NA_integer_,
       tradeable_expiries = NA_integer_,
       source = "unavailable",
       reason = "live IBKR, DB, and scanner CSV all unavailable",
       retrieved_at = NA)
}

# ── PHASE B ──────────────────────────────────────────────────────────────
# Step 2 rewrite (2026-05-12): scanner CSV demoted. Pull score / stage_pts /
# sector_pts / footprint_pts all dropped — they triple-counted the per-
# indicator breakdown. Output is now: per-indicator breakdown + direction-aware
# sector RS context (sector, ETF, stock-vs-sector RS @ 20d/60d, sector-vs-SPY
# RS, sector rank) + stage label + direction alignment.
#
# Stage label is recomputed locally from breakdown setup/breakout counts +
# MA50 position; no dependency on isTrendContinuation.
run_phase_b <- function(ticker, direction, freshness = NULL) {
  spot <- .live_price(ticker)
  breakdown <- .compute_phase_b_breakdown(ticker, spot, direction)

  # Per-indicator counts from breakdown attrs (set by compute_breakdown)
  setup_n <- if (!is.null(breakdown)) attr(breakdown, "setup_count") %||% 0L else NA_integer_
  bk_n    <- if (!is.null(breakdown)) attr(breakdown, "breakout_count") %||% 0L else NA_integer_

  # MA50 position from the last indicator row (for stage + direction alignment)
  ind_last <- .phase_b_last_indicators(ticker)
  ma50 <- if (!is.null(ind_last)) as.numeric(ind_last$ma50) else NA_real_
  ma50_slope <- if (!is.null(ind_last)) as.numeric(ind_last$ma50_slope) else NA_real_

  # Direction alignment: long ALIGNED iff price > MA50; short ALIGNED iff price < MA50.
  align <- if (is.na(spot) || is.na(ma50)) "n/a"
           else if (direction == "long"  && spot > ma50) "ALIGNED"
           else if (direction == "short" && spot < ma50) "ALIGNED"
           else "MISMATCH"

  # Stage label (mechanical, direction-aware)
  stage <- .compute_stage_label(spot, ma50, ma50_slope, setup_n, bk_n, direction)

  # Sector RS context (heavy: walks all sector ETFs)
  stock_ret20 <- if (!is.null(ind_last)) as.numeric(ind_last$ret20) else NA_real_
  stock_ret60 <- if (!is.null(ind_last)) as.numeric(ind_last$ret60) else NA_real_
  sector_ctx <- tryCatch(
    compute_sector_rs_context(ticker, direction, stock_ret20, stock_ret60),
    error = function(e) {
      message("Sector RS context failed: ", conditionMessage(e)); NULL
    })

  # Phase B result: PASS if direction aligns AND sector is in the favorable
  # half by direction-aware rank. SKIP otherwise. STALE only if breakdown
  # itself failed (no live OHLC).
  result <- if (is.null(breakdown) || nrow(breakdown) == 0) "STALE"
            else if (align == "ALIGNED" &&
                     !is.null(sector_ctx) &&
                     !is.na(sector_ctx$sector_rank) &&
                     !is.na(sector_ctx$n_sectors) &&
                     sector_ctx$sector_rank <= ceiling(sector_ctx$n_sectors / 2)) "PASS"
            else "SKIP"

  list(
    result           = result,
    stage            = stage,
    direction_match  = align,
    sector           = if (!is.null(sector_ctx)) sector_ctx$sector else NA_character_,
    sector_etf       = if (!is.null(sector_ctx)) sector_ctx$etf_sym else NA_character_,
    sector_rs_rank   = if (!is.null(sector_ctx)) sector_ctx$sector_rank else NA_integer_,
    n_sectors        = if (!is.null(sector_ctx)) sector_ctx$n_sectors else NA_integer_,
    sector_context   = sector_ctx,
    setup_count      = setup_n,
    breakout_count   = bk_n,
    price            = spot,
    breakdown        = breakdown,
    breakdown_retrieved_at = if (!is.null(breakdown)) Sys.time() else NULL
  )
}

#' Compute stage label from breakdown counts + MA50 position, direction-aware.
.compute_stage_label <- function(spot, ma50, ma50_slope,
                                  setup_n, bk_n, direction) {
  if (is.na(spot) || is.na(ma50)) return(NA_character_)
  setup_n <- if (is.na(setup_n)) 0L else as.integer(setup_n)
  bk_n    <- if (is.na(bk_n))    0L else as.integer(bk_n)
  if (direction == "long") {
    if (spot > ma50 * 1.15) return("extended")
    if (setup_n >= 4L && bk_n >= 3L) return("early")
    if (!is.na(ma50_slope) && ma50_slope > 0 && spot > ma50) return("continuation")
    return("none")
  } else {  # short
    if (spot < ma50 * 0.85) return("extended")
    if (setup_n >= 4L && bk_n >= 3L) return("early")
    if (!is.na(ma50_slope) && ma50_slope < 0 && spot < ma50) return("continuation")
    return("none")
  }
}

#' Fetch the latest indicator row for the ticker (for stage + sector RS).
.phase_b_last_indicators <- function(ticker) {
  tryCatch({
    raw <- fetch_single_ohlcv(ticker)
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    ind <- calc_ind(raw)
    if (is.null(ind) || nrow(ind) == 0) return(NULL)
    tail(ind, 1)
  }, error = function(e) {
    message("Phase B indicators failed: ", conditionMessage(e)); NULL
  })
}

#' Compute the per-criterion technical breakdown for /analyze Phase B.
#' Returns NULL silently on any failure so the report falls back to aggregate.
.compute_phase_b_breakdown <- function(ticker, price, direction) {
  tryCatch({
    raw <- fetch_single_ohlcv(ticker)
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    ind <- calc_ind(raw)
    if (is.null(ind) || nrow(ind) == 0) return(NULL)
    last <- ind |> dplyr::filter(!is.na(adx10), !is.na(ma50), !is.na(rsi14),
                                 !is.na(obv_slope), !is.na(updn_ratio),
                                 !is.na(ret20)) |> tail(1)
    if (nrow(last) == 0) return(NULL)
    p <- if (is.null(price) || is.na(price)) tail(ind$Close, 1) else price
    compute_breakdown(last, p, direction)
  }, error = function(e) {
    message("Phase B breakdown failed: ", conditionMessage(e)); NULL
  })
}

.live_price <- function(ticker) {
  p <- tryCatch(Tdata::getLastSymPrice(ticker), error = function(e) NA_real_)
  if (is.null(p) || length(p) == 0) return(NA_real_)
  # Some Tdata variants return a data.frame; pluck a price-like column.
  if (is.data.frame(p)) {
    cand <- intersect(c("price", "Close", "close", "last", "value"), names(p))
    if (length(cand) > 0) p <- p[[cand[1]]] else {
      # Pick the last numeric column (date columns sit at the start)
      num_cols <- which(sapply(p, is.numeric))
      p <- if (length(num_cols) > 0) p[[tail(num_cols, 1)]] else p[[1]]
    }
  }
  as.numeric(p)[1]
}

# ── PHASE C (C.1 cheap score; C.2 funnel handled in funnel.R) ────────────
#
# Live-fetch policy: funnel runs unconditionally, even when the scanner CSV row
# is missing or all cached cheap_* fields are NA. When the scanner doesn't carry
# a cheap_score (the most common case after a Phase B SKIP), we recompute it
# from the live funnel using the same ivp_pts / vrp_pts / term_pts thresholds
# the scanner uses. cheap_side is derived from funnel.rr_vp sign.
# Step 3 rewrite (2026-05-12): cheap_score always computed live from the
# funnel. Components (ivp_pts/4 + vrp_pts/2 + term_pts/2 + rr_pts/1, max=9)
# always exposed in the output for transparency. Scanner CSV no longer read.
# PASS cutoff = >=6 of 9.
run_phase_c <- function(ticker, direction, run_funnel = TRUE, config,
                        spot = NA_real_, freshness = NULL) {
  funnel <- if (run_funnel)
    run_funnel_deep_dive(ticker, direction, NULL, config, spot = spot,
                         freshness = freshness)
  else NULL

  components <- if (!is.null(funnel))
    .compute_cheap_components(funnel, direction, config)
  else NULL

  cheap_score <- if (!is.null(components)) components$score else NA_integer_
  cheap_side  <- if (!is.null(components)) components$side  else NA_character_
  ivp_used    <- if (!is.null(funnel)) funnel$ivp_used else NA_real_
  vrp_value   <- if (!is.null(funnel)) funnel$vrp_log  else NA_real_

  cheap_pass <- !is.na(cheap_score) && cheap_score >= 6L

  result <- if (!is.na(cheap_score)) {
    if (cheap_pass) "PASS" else "SKIP"
  } else "STALE"

  list(
    result      = result,
    cheap_pass  = cheap_pass,
    cheap_score = cheap_score,
    cheap_max   = 9L,
    cheap_side  = cheap_side,
    components  = components,
    ivp_used    = ivp_used,
    vrp         = vrp_value,
    funnel      = funnel,
    source      = if (!is.null(components)) "live funnel" else "unavailable"
  )
}

#' Compute cheap_score components from funnel data.
#' Components: ivp_pts (max 4) + vrp_pts (max 2) + term_pts (max 2) +
#' rr_pts (max 1) = score in [0, 9]. Each component's threshold is
#' surfaced so the report can show "47.8% → 2 pts (≤60 band)".
.compute_cheap_components <- function(funnel, direction, config) {
  ivp <- funnel$ivp_used; vrp <- funnel$vrp_log
  term <- funnel$term_pct; rr <- funnel$rr_vp

  ivp_pts <- if (is.na(ivp)) 0L
             else if (ivp <= config$ivp_pts$pt4_max) 4L
             else if (ivp <= config$ivp_pts$pt3_max) 3L
             else if (ivp <= config$ivp_pts$pt2_max) 2L
             else if (ivp <= config$ivp_pts$pt1_max) 1L
             else 0L
  ivp_band <- if (is.na(ivp)) "n/a"
              else if (ivp <= config$ivp_pts$pt4_max) sprintf("&le;%g", config$ivp_pts$pt4_max)
              else if (ivp <= config$ivp_pts$pt3_max) sprintf("&le;%g", config$ivp_pts$pt3_max)
              else if (ivp <= config$ivp_pts$pt2_max) sprintf("&le;%g", config$ivp_pts$pt2_max)
              else if (ivp <= config$ivp_pts$pt1_max) sprintf("&le;%g", config$ivp_pts$pt1_max)
              else sprintf(">%g", config$ivp_pts$pt1_max)

  vrp_pts <- if (is.na(vrp)) 0L
             else if (vrp <= config$vrp_pts$pt2_max) 2L
             else if (vrp <= config$vrp_pts$pt1_max) 1L
             else 0L
  vrp_band <- if (is.na(vrp)) "n/a"
              else if (vrp <= config$vrp_pts$pt2_max) sprintf("&le;%g", config$vrp_pts$pt2_max)
              else if (vrp <= config$vrp_pts$pt1_max) sprintf("&le;%g", config$vrp_pts$pt1_max)
              else sprintf(">%g", config$vrp_pts$pt1_max)

  term_pts <- if (is.na(term)) 0L
              else if (term <= config$term_pts$pt2_max) 2L
              else if (term <= config$term_pts$pt1_max) 1L
              else 0L
  term_band <- if (is.na(term)) "n/a"
               else if (term <= config$term_pts$pt2_max) sprintf("&le;%g%%", config$term_pts$pt2_max)
               else if (term <= config$term_pts$pt1_max) sprintf("&le;%g%%", config$term_pts$pt1_max)
               else sprintf(">%g%%", config$term_pts$pt1_max)

  rr_pts <- if (is.na(rr)) 0L
            else if ((direction == "long"  && rr > 0) ||
                     (direction == "short" && rr < 0)) 1L
            else 0L
  rr_band <- if (is.na(rr)) "n/a"
             else if (rr_pts == 1L) sprintf("RR aligned with %s", direction)
             else sprintf("RR mismatched %s", direction)

  score <- ivp_pts + vrp_pts + term_pts + rr_pts
  side <- if (is.na(rr) || abs(rr) < 1) "neutral"
          else if (rr > 0) "long" else "short"

  list(
    score = score, side = side,
    ivp_pts = ivp_pts, ivp_max = 4L, ivp_value = ivp, ivp_band = ivp_band,
    vrp_pts = vrp_pts, vrp_max = 2L, vrp_value = vrp, vrp_band = vrp_band,
    term_pts = term_pts, term_max = 2L, term_value = funnel$term_pct, term_band = term_band,
    rr_pts = rr_pts, rr_max = 1L, rr_value = rr, rr_band = rr_band
  )
}

# ── PHASE E ──────────────────────────────────────────────────────────────
# Step 5 rewrite 2026-05-12: Phase A is informational, no longer gates Phase E.
# Classification driven by B / C / D outcomes.
run_phase_e <- function(phase_a, phase_b, phase_c, phase_d, config) {
  pass_b <- phase_b$result == "PASS"
  pass_c <- phase_c$result == "PASS"
  pass_d <- phase_d$result == "PASS"

  # phase_of_drop: B / C / D / none. Phase A no longer participates.
  drop <- if (!pass_b) "B"
          else if (!pass_c) "C"
          else if (!pass_d) "D"
          else "none"

  label <- if (pass_b && pass_c && pass_d &&
               isTRUE(phase_d$any_within_cap)) "TOP PICK"
           else if (pass_b && pass_c) "WATCH"
           else "SKIP"

  list(
    classification = label,
    phase_of_drop  = drop,
    pass_a = TRUE, pass_b = pass_b, pass_c = pass_c, pass_d = pass_d
  )
}
