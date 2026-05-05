# reports/analyze/structures.R — Phase D: setup, chain, R:R + structures table.
#
# Live-fetch policy (see feedback_analyze_live_data_fallback.md):
#   Phase D NEVER short-circuits on upstream phase SKIPs. We always invoke
#   the live spread pricer. When the scanner CSV doesn't carry an expiry, we
#   pick one live from IBKR (~45 DTE). When option_chain_oi_history is empty
#   for the ticker, we live-pull OI via get_chain_oi. Failures surface as
#   "FETCH FAILED: <cause>" — never as silent placeholders.

run_phase_d <- function(ticker, direction, phase_b, phase_c, config,
                        freshness = NULL) {
  scan <- .read_scanner_row(ticker, freshness)
  r  <- if (!is.null(scan$row)) scan$row else NULL
  spot <- phase_b$price
  if (is.null(spot) || is.na(spot)) {
    spot <- tryCatch(.live_price(ticker), error = function(e) NA_real_)
  }

  tws_ok <- isTRUE(config$tws_reachable)

  # Resolve expiry: prefer scanner row, fall back to IBKR (~45 DTE)
  expiry_resolved <- .resolve_expiry(r, ticker, target_dte = 45, tws_ok = tws_ok)
  expiry  <- expiry_resolved$expiry
  expiry_reason <- expiry_resolved$reason

  # Vehicle: prefer scanner-row value; otherwise re-derive via shared rule.
  cheap_score_for_rule <- if (!is.null(phase_c) && !is.na(phase_c$cheap_score))
                            as.integer(phase_c$cheap_score) else NA_integer_
  stage_for_rule <- if (!is.null(phase_b) && !is.null(phase_b$stage)) phase_b$stage else NA_character_
  vehicle_pick <- pick_vehicle_expiry(spot,
                                       cheap_score = cheap_score_for_rule,
                                       stage       = stage_for_rule)
  vehicle <- if (!is.null(r) && !is.na(r$vehicle) && nzchar(r$vehicle)) r$vehicle
             else vehicle_pick$vehicle %||% "spread"
  vehicle_reason <- vehicle_pick$reason

  # Targets: scanner-derived; not reconstructable in /analyze without a re-run.
  drop_phase <- if (!is.null(r) && !is.na(r$phase_of_drop) && nzchar(r$phase_of_drop))
                  r$phase_of_drop else NA_character_
  # Targets: prefer scanner row; otherwise live-compute from fresh OHLC.
  has_targets <- !is.null(r) && !is.na(suppressWarnings(as.numeric(r$spot_target_low)))
  targets <- if (has_targets) {
    list(
      spot_target_low  = as.numeric(r$spot_target_low),
      spot_target_high = as.numeric(r$spot_target_high),
      targets_agreeing = suppressWarnings(as.integer(r$targets_agreeing)),
      fib_confirms     = as.logical(r$fib_confirms),
      source           = "scanner CSV",
      reason           = NULL
    )
  } else {
    .live_targets(ticker, spot)
  }

  # Chain / OI: prefer scanner row, fall back to live get_chain_oi
  chain <- .resolve_chain(r, ticker, expiry, spot, config, tws_ok = tws_ok,
                          freshness = freshness)

  # Structures: live pricer runs whenever TWS is reachable; otherwise we
  # surface FETCH FAILED instead of issuing a request that may hang.
  structures <- enumerate_structures(ticker, direction, spot, expiry, vehicle,
                                     config, expiry_reason = expiry_reason,
                                     tws_ok = tws_ok)
  within <- if ("within_cap" %in% names(structures))
              structures$within_cap else logical(0)
  n_within <- sum(within, na.rm = TRUE)

  # Entry framework: prefer scanner row; live-derive when scanner is silent.
  cached_rr <- if (!is.null(r)) suppressWarnings(as.numeric(r$rr)) else NA_real_
  cached_entry_state <- if (!is.null(r) && !is.na(r$entry_state) && nzchar(r$entry_state))
                          r$entry_state else NA_character_
  cached_effective_target <- if (!is.null(r))
                               suppressWarnings(as.numeric(r$effective_target)) else NA_real_
  cached_strike <- if (!is.null(r)) suppressWarnings(as.numeric(r$strike)) else NA_real_

  has_entry_framework <- !is.na(cached_rr) && !is.na(cached_entry_state) &&
                         nzchar(cached_entry_state)

  rr_obj <- if (has_entry_framework) {
    list(rr = cached_rr,
         entry_floor   = if (!is.null(r)) suppressWarnings(as.numeric(r$entry_floor))   else NA_real_,
         entry_ceiling = if (!is.null(r)) suppressWarnings(as.numeric(r$entry_ceiling)) else NA_real_,
         headroom_band = if (!is.null(r) && !is.na(r$headroom_band)) r$headroom_band else NA_character_,
         entry_state   = cached_entry_state,
         effective_target = cached_effective_target,
         strike        = cached_strike,
         spread_short_strike = NA_real_,
         iv_used       = NA_real_,
         source        = "scanner CSV",
         reason        = NULL)
  } else {
    .live_entry_framework(ticker, direction, vehicle, spot, expiry,
                          targets, chain, phase_c, config)
  }

  d_pass <- !is.na(targets$targets_agreeing) &&
            targets$targets_agreeing >= 2L &&
            isTRUE(rr_obj$rr >= config$rr_min) &&
            (rr_obj$entry_state == "IN BAND") &&
            !identical(chain$chain_state, "chain-capped") &&
            n_within > 0

  list(
    result            = if (d_pass) "PASS" else "SKIP",
    vehicle           = vehicle,
    vehicle_reason    = vehicle_reason,
    structures_retrieved_at = if (isTRUE(tws_ok) && !is.null(structures) &&
                                  "source" %in% names(structures) &&
                                  any(structures$source == "live", na.rm = TRUE))
                                format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                              else NA_character_,
    strike            = rr_obj$strike,
    expiry            = expiry,
    expiry_reason     = expiry_reason,
    targets           = targets,
    targets_agreeing  = targets$targets_agreeing,
    chain_state       = chain$chain_state,
    chain_reason      = chain$reason,
    oi_cap_call       = chain$oi_cap_call,
    oi_cap_put        = chain$oi_cap_put,
    effective_target  = rr_obj$effective_target,
    rr                = rr_obj$rr,
    entry_floor       = rr_obj$entry_floor,
    entry_ceiling     = rr_obj$entry_ceiling,
    headroom_band     = rr_obj$headroom_band,
    entry_state       = rr_obj$entry_state,
    entry_reason      = rr_obj$reason,
    entry_source      = rr_obj$source,
    structures        = structures,
    n_structures_within_cap = n_within,
    any_within_cap    = n_within > 0,
    spot              = spot
  )
}

#' Live-compute structural targets via 300-day OHLC + shared
#' compute_structural_target(). Returns the same shape as the scanner-row path.
.live_targets <- function(ticker, spot) {
  if (is.na(spot)) return(list(
    spot_target_low = NA_real_, spot_target_high = NA_real_,
    targets_agreeing = NA_integer_, fib_confirms = NA,
    source = "live", reason = "spot price unavailable"))
  raw <- tryCatch(fetch_single_ohlcv(ticker), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 60) return(list(
    spot_target_low = NA_real_, spot_target_high = NA_real_,
    targets_agreeing = NA_integer_, fib_confirms = NA,
    source = "live",
    reason = "OHLC history insufficient (<60 days) for structural targets"))
  raw <- raw[order(raw$date), ]
  res <- tryCatch(compute_structural_target(spot, raw$Close, raw$High),
                  error = function(e) NULL)
  if (is.null(res)) return(list(
    spot_target_low = NA_real_, spot_target_high = NA_real_,
    targets_agreeing = NA_integer_, fib_confirms = NA,
    source = "live", reason = "compute_structural_target failed"))
  list(
    spot_target_low  = res$spot_target_low,
    spot_target_high = res$spot_target_high,
    targets_agreeing = res$targets_agreeing,
    fib_confirms     = res$fib_confirms,
    source           = "live OHLC",
    reason           = NULL
  )
}

#' Live-derive R:R + entry framework when scanner row is silent. Picks strikes
#' off rounded grid, prices via Black-Scholes (Tbasics::getOptPrice) using
#' phase_c$ivp_used (or 0.30 fallback) as IV, then calls compute_rr_entry +
#' classify_entry_state from shared/setup_chain_rr.R.
.live_entry_framework <- function(ticker, direction, vehicle, spot, expiry,
                                  targets, chain, phase_c, config) {
  empty <- list(rr = NA_real_, entry_floor = NA_real_, entry_ceiling = NA_real_,
                headroom_band = NA_character_, entry_state = NA_character_,
                effective_target = NA_real_, strike = NA_real_,
                spread_short_strike = NA_real_, iv_used = NA_real_,
                source = "live", reason = NULL)
  if (is.na(spot)) return(modifyList(empty,
    list(reason = "spot price unavailable — cannot derive R:R")))
  if (is.na(expiry) || !nzchar(expiry)) return(modifyList(empty,
    list(reason = "expiry unavailable — cannot derive R:R")))
  spot_target_low  <- targets$spot_target_low
  spot_target_high <- targets$spot_target_high
  if (is.na(spot_target_low)) return(modifyList(empty,
    list(reason = "no structural target — cannot derive R:R")))

  # Effective target: capped by chain OI if chain says so.
  eff_target <- if (!is.null(chain$oi_cap_call) && !is.na(chain$oi_cap_call) &&
                    chain$oi_cap_call < spot_target_low) chain$oi_cap_call
                else spot_target_low

  # IV: prefer phase_c funnel iv30, else cheap_score-side IVP estimate, else 0.30.
  iv_now <- NA_real_
  if (!is.null(phase_c$funnel) && !is.na(phase_c$funnel$iv30)) iv_now <- phase_c$funnel$iv30
  if (is.na(iv_now)) iv_now <- 0.30

  # Strike picks. Long: round up to $5 grid (call) or down (put).
  right_C <- direction == "long"
  strike_long <- NA_real_; strike_short <- NA_real_
  if (vehicle == "call") {
    strike_long <- if (right_C) ceiling(spot / 5) * 5 else floor(spot / 5) * 5
  } else if (vehicle == "spread") {
    strike_long  <- round(spot / 5) * 5
    strike_short <- round(eff_target / 5) * 5
    if (!is.na(strike_short) && strike_short <= strike_long)
      strike_short <- strike_long + 5
  }

  # Entry premium via BS at current spot/strike with iv_now.
  expiry_dt <- tryCatch(as.Date(expiry, format = "%Y%m%d"),
                        error = function(e) NA)
  dte <- if (inherits(expiry_dt, "Date") && !is.na(expiry_dt))
           as.integer(expiry_dt - Sys.Date()) else NA_integer_
  if (is.na(dte) || dte <= 0) return(modifyList(empty,
    list(reason = sprintf("expiry %s is in the past — cannot derive R:R", expiry))))

  entry_prem <- if (vehicle == "stock") {
    spot * 0.05  # 5%-of-spot stop-distance proxy
  } else if (vehicle == "call" && !is.na(strike_long)) {
    tryCatch(Tbasics::getOptPrice(
      type = if (right_C) "Call" else "Put",
      S = spot, K = strike_long, r = 0.045, DTE = dte, sig = iv_now),
      error = function(e) NA_real_)
  } else if (vehicle == "spread" && !is.na(strike_long) && !is.na(strike_short)) {
    long_p  <- tryCatch(Tbasics::getOptPrice(
      type = "Call", S = spot, K = strike_long,  r = 0.045, DTE = dte, sig = iv_now),
      error = function(e) NA_real_)
    short_p <- tryCatch(Tbasics::getOptPrice(
      type = "Call", S = spot, K = strike_short, r = 0.045, DTE = dte, sig = iv_now),
      error = function(e) NA_real_)
    if (!is.na(long_p) && !is.na(short_p)) max(long_p - short_p, 0.05) else NA_real_
  } else NA_real_

  if (is.na(entry_prem)) return(modifyList(empty,
    list(strike = strike_long, spread_short_strike = strike_short,
         effective_target = eff_target, iv_used = iv_now,
         reason = "BS entry premium computation failed")))

  rr_obj <- tryCatch(compute_rr_entry(
    vehicle = vehicle, strike = strike_long, expiry = expiry,
    current_price = spot, effective_target = eff_target,
    iv_now = iv_now, entry_premium = entry_prem,
    spread_short_strike = strike_short,
    spot_target_high = spot_target_high,
    rr_min = config$rr_min),
    error = function(e) NULL)
  if (is.null(rr_obj)) return(modifyList(empty,
    list(strike = strike_long, spread_short_strike = strike_short,
         effective_target = eff_target, iv_used = iv_now,
         reason = "compute_rr_entry failed")))

  chain_walk_status <- if (identical(chain$chain_state, "FETCH FAILED") ||
                           is.na(chain$chain_state) ||
                           !nzchar(chain$chain_state %||% "")) "FAILED" else "OK"
  entry_state <- classify_entry_state(rr_obj$entry_floor, rr_obj$entry_ceiling,
                                       chain_walk_status)

  list(
    rr            = rr_obj$rr,
    entry_floor   = rr_obj$entry_floor,
    entry_ceiling = rr_obj$entry_ceiling,
    headroom_band = rr_obj$headroom_band,
    entry_state   = entry_state,
    effective_target = eff_target,
    strike        = strike_long,
    spread_short_strike = strike_short,
    iv_used       = iv_now,
    source        = "live",
    reason        = NULL
  )
}

# ── Expiry resolution: scanner row first, then live IBKR pick ────────────
.resolve_expiry <- function(scanner_row, ticker, target_dte = 45, tws_ok = TRUE) {
  if (!is.null(scanner_row) && !is.na(scanner_row$expiry) && nzchar(as.character(scanner_row$expiry))) {
    return(list(expiry = as.character(scanner_row$expiry), reason = NULL))
  }
  if (!isTRUE(tws_ok)) return(list(
    expiry = NA_character_,
    reason = "TWS not reachable; cannot pick live expiry"))
  py <- tryCatch(Tdata:::tdata_py, error = function(e) NULL)
  if (is.null(py)) return(list(
    expiry = NA_character_,
    reason = "tdata_py unavailable; cannot pick live expiry"))
  expiries <- tryCatch(py$getExpirationDates(ticker),
                       error = function(e) conditionMessage(e))
  if (is.character(expiries) && length(expiries) == 1) return(list(
    expiry = NA_character_, reason = paste("getExpirationDates:", expiries)))
  if (is.null(expiries) || length(expiries) == 0) return(list(
    expiry = NA_character_, reason = "no expirations from IBKR"))
  exp_dates <- as.Date(as.character(expiries), format = "%Y%m%d")
  dtes <- as.integer(exp_dates - Sys.Date())
  ok <- !is.na(dtes) & dtes > 0
  if (!any(ok)) return(list(
    expiry = NA_character_, reason = "no future expirations from IBKR"))
  expiries <- expiries[ok]; dtes <- dtes[ok]
  list(expiry = expiries[which.min(abs(dtes - target_dte))], reason = NULL)
}

# ── Chain / OI resolution ────────────────────────────────────────────────
.resolve_chain <- function(scanner_row, ticker, expiry, spot, config, tws_ok = TRUE,
                           freshness = NULL) {
  cached_oi_call <- if (!is.null(scanner_row)) suppressWarnings(as.numeric(scanner_row$oi_cap_call)) else NA_real_
  cached_oi_put  <- if (!is.null(scanner_row)) suppressWarnings(as.numeric(scanner_row$oi_cap_put))  else NA_real_
  cached_state   <- if (!is.null(scanner_row) && !is.na(scanner_row$chain_state) && nzchar(scanner_row$chain_state))
                      scanner_row$chain_state else NA_character_
  if (!is.na(cached_oi_call) && !is.na(cached_oi_put) && !is.na(cached_state)) {
    return(list(oi_cap_call = cached_oi_call, oi_cap_put = cached_oi_put,
                chain_state = cached_state, reason = NULL))
  }

  # DB cache (option_chain_oi_history) — gated by freshness policy
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  on.exit(if (!is.null(conn)) DBI::dbDisconnect(conn), add = TRUE)
  oi_rows <- if (!is.null(conn) && !is.na(expiry)) {
    tryCatch(DBI::dbGetQuery(conn,
      "SELECT strike, right, open_interest, cache_date FROM option_chain_oi_history
       WHERE sym = ? AND expiry = ?
       ORDER BY cache_date DESC", params = list(ticker, expiry)),
      error = function(e) NULL)
  } else NULL

  if (!is.null(oi_rows) && nrow(oi_rows) > 0) {
    latest_cache <- max(oi_rows$cache_date, na.rm = TRUE)
    if (is.null(freshness) || is_fresh(latest_cache, freshness)) {
      return(.summarize_oi(oi_rows, source = "DB option_chain_oi_history"))
    }
    # Stale — fall through to live fetch with reason
  }

  # Live fallback: get_chain_oi
  if (!isTRUE(tws_ok)) return(list(
    oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    chain_state = NA_character_,
    reason = "DB option_chain_oi_history empty; TWS not reachable"))
  py <- tryCatch(Tdata:::tdata_py, error = function(e) NULL)
  if (is.null(py)) return(list(
    oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    chain_state = NA_character_,
    reason = "DB option_chain_oi_history empty; tdata_py unavailable"))
  if (is.na(expiry) || is.na(spot)) return(list(
    oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    chain_state = NA_character_,
    reason = "missing expiry or spot — cannot pull live OI"))

  smin <- spot * (1 - 0.25); smax <- spot * (1 + 0.25)
  live_oi <- tryCatch(py$get_chain_oi(
    sym = ticker, expiration = expiry,
    strike_min = smin, strike_max = smax),
    error = function(e) conditionMessage(e))
  if (is.character(live_oi) && length(live_oi) == 1) return(list(
    oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    chain_state = NA_character_,
    reason = paste("get_chain_oi:", live_oi)))
  if (is.null(live_oi) || !is.data.frame(live_oi) || nrow(live_oi) == 0) return(list(
    oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    chain_state = NA_character_,
    reason = sprintf("get_chain_oi returned no rows for %s @ %s", ticker, expiry)))
  .summarize_oi(live_oi, source = "live get_chain_oi")
}

#' Reduce a per-strike/per-right OI table into oi_cap_call, oi_cap_put,
#' and a chain_state label.
.summarize_oi <- function(oi_rows, source = "DB") {
  oi_rows$open_interest <- suppressWarnings(as.numeric(oi_rows$open_interest))
  oi_rows <- oi_rows[!is.na(oi_rows$open_interest) & oi_rows$open_interest > 0, ]
  if (nrow(oi_rows) == 0) return(list(
    oi_cap_call = NA_real_, oi_cap_put = NA_real_,
    chain_state = NA_character_,
    reason = sprintf("%s: all OI rows empty/zero", source)))

  calls <- oi_rows[oi_rows$right == "C", , drop = FALSE]
  puts  <- oi_rows[oi_rows$right == "P", , drop = FALSE]
  oi_cap_call <- if (nrow(calls) > 0)
    calls$strike[which.max(calls$open_interest)] else NA_real_
  oi_cap_put  <- if (nrow(puts)  > 0)
    puts$strike[which.max(puts$open_interest)]   else NA_real_

  # Chain state — mechanical: top-3 OI / total ratio
  total_oi <- sum(oi_rows$open_interest)
  top3 <- sum(sort(oi_rows$open_interest, decreasing = TRUE)[1:3], na.rm = TRUE)
  conc <- if (total_oi > 0) top3 / total_oi else NA_real_
  state <- if (is.na(conc)) "open"
           else if (conc >= 0.6) "chain-capped"
           else if (conc >= 0.4) "crowded"
           else "open"

  list(oi_cap_call = oi_cap_call, oi_cap_put = oi_cap_put,
       chain_state = state, reason = NULL)
}

# ── Structure enumeration (live spread pricer; FETCH FAILED row otherwise) ──
enumerate_structures <- function(ticker, direction, spot, expiry, vehicle,
                                 config, expiry_reason = NULL, tws_ok = TRUE) {
  cap <- config$risk_cap_lot_usd
  right <- if (direction == "long") "C" else "P"

  if (!isTRUE(tws_ok)) return(.fetch_failed_structures(
    "TWS not reachable — cannot price spreads"))
  if (is.na(spot)) return(.fetch_failed_structures(
    "spot price unavailable — cannot price spreads"))
  if (is.na(expiry)) return(.fetch_failed_structures(
    paste0("expiry unavailable",
           if (!is.null(expiry_reason)) paste0(" (", expiry_reason, ")") else "")))
  if (!requireNamespace("reticulate", quietly = TRUE)) return(.fetch_failed_structures(
    "reticulate package unavailable — cannot reach live pricer"))

  spread_mod <- tryCatch(reticulate::import("tdata_py.spread", delay_load = TRUE),
                         error = function(e) NULL)
  if (is.null(spread_mod)) return(.fetch_failed_structures(
    "tdata_py.spread import failed"))

  rows <- list(); failures <- character(0)
  for (w in config$spread_widths) {
    df <- tryCatch(spread_mod$compute_spread_risk_reward(
      sym = ticker, trading_class = ticker, expiration = expiry,
      current_price = spot, moneyness_pct = config$moneyness_pct,
      spread_width = as.integer(w), right = right,
      multiplier = 100L, currency = "USD",
      exchangeSec = "SMART", exchangeOpt = "SMART",
      force_refresh = TRUE),
      error = function(e) {
        failures <<- c(failures,
                       sprintf("width=%s: %s", w, conditionMessage(e)))
        NULL
      })
    if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
      df$source <- "live"
      rows[[length(rows) + 1]] <- df
    }
  }

  if (length(rows) == 0) return(.fetch_failed_structures(
    if (length(failures) > 0)
      paste("compute_spread_risk_reward returned no rows;", paste(failures, collapse = "; "))
    else
      "compute_spread_risk_reward returned no rows for any width"))

  spreads_df <- do.call(rbind, rows)
  spreads_df$within_cap <- spreads_df$max_risk <= cap
  spreads_df <- spreads_df[order(-spreads_df$reward_risk_ratio), ]
  spreads_df
}

#' Single-row data frame surfacing a FETCH FAILED reason in the structures
#' table. Numeric cells stay NA so report.R renders them as `n/a`, but the
#' `reason` column carries the cause so the user is never silently misled.
.fetch_failed_structures <- function(reason) {
  data.frame(
    structure          = "FETCH FAILED",
    expiry             = NA_character_,
    debit              = NA_real_,
    max_risk           = NA_real_,
    max_reward         = NA_real_,
    reward_risk_ratio  = NA_real_,
    prob_success_delta = NA_real_,
    within_cap         = NA,
    surface_fact       = paste0("FETCH FAILED: ", reason),
    source             = "fetch_failed",
    stringsAsFactors   = FALSE
  )
}
