# reports/analyze/structures.R — Phase D: setup, chain, R:R + structures table.
#
# Sourcing (Step 1 inversion 2026-05-12 — see project_analyze_redesign_2026_05):
#   Expiry + chain OI go through resolvers in shared/live_sources.R (live IBKR
#   primary, DB-cache-if-fresh second). Scanner CSV row is still consulted for
#   higher-level fields not yet migrated (strike / vehicle / entry framework /
#   structural targets) — those phases get rewritten in Steps 2-4.

run_phase_d <- function(ticker, direction, phase_b, phase_c, config,
                        freshness = NULL) {
  scan <- .read_scanner_row(ticker, freshness)
  r  <- if (!is.null(scan$row)) scan$row else NULL
  spot <- phase_b$price
  if (is.null(spot) || is.na(spot)) {
    spot <- tryCatch(.live_price(ticker), error = function(e) NA_real_)
  }

  tws_ok <- isTRUE(config$tws_reachable)

  # Resolve TWO expiries — ~30 DTE (faster decay) and ~55 DTE (time cushion).
  # Step 4 rewrite 2026-05-12 (project_analyze_redesign_2026_05.md).
  exp_short_r <- resolve_expiry(ticker, target_dte = 30, tws_ok = tws_ok)
  exp_long_r  <- resolve_expiry(ticker, target_dte = 55, tws_ok = tws_ok)
  expiry_short <- exp_short_r$value
  expiry_long  <- exp_long_r$value
  # Dedupe (if 30/55 collapse to the same expiration)
  expiries <- unique(c(expiry_short, expiry_long))
  expiries <- expiries[!is.na(expiries)]
  # Primary expiry (used by entry framework / R:R) — the ~30 DTE pick
  expiry <- if (length(expiries) > 0) expiries[1] else NA_character_
  expiry_reason <- if (is.na(expiry_short)) exp_short_r$reason
                   else if (is.na(expiry_long)) exp_long_r$reason
                   else NULL

  # Vehicle: prefer scanner-row value; otherwise re-derive via shared rule.
  cheap_score_for_rule <- if (!is.null(phase_c) && !is.na(phase_c$cheap_score))
                            as.integer(phase_c$cheap_score) else NA_integer_
  stage_for_rule <- if (!is.null(phase_b) && !is.null(phase_b$stage)) phase_b$stage else NA_character_
  vehicle_pick <- pick_vehicle_expiry(spot,
                                       cheap_score = cheap_score_for_rule,
                                       stage       = stage_for_rule,
                                       direction   = direction)
  vehicle <- vehicle_pick$vehicle %||% "spread"
  vehicle_reason <- vehicle_pick$reason

  # Targets: ALWAYS live-compute (direction-aware). Scanner CSV is LONG-only
  # and can't be reused for shorts (wrong tail). Step 4 rewrite 2026-05-12.
  targets <- .live_targets(ticker, spot, direction = direction)

  # Chain / OI: resolver (DB-fresh → live get_chain_oi). CSV oi_cap_call/_put
  # in scanner row are last-resort fallback if both DB and live fail.
  thin_oi_threshold <- as.integer(config$thin_oi_threshold %||% 100L)
  chain_r <- resolve_chain_oi(ticker, expiry, spot, freshness, tws_ok = tws_ok,
                              thin_oi_threshold = thin_oi_threshold)
  chain <- if (is.list(chain_r$value)) {
    list(oi_cap_call = chain_r$value$oi_cap_call,
         oi_cap_put  = chain_r$value$oi_cap_put,
         chain_state = chain_r$value$chain_state,
         reason      = NULL)
  } else if (!is.null(r) &&
             !is.na(suppressWarnings(as.numeric(r$oi_cap_call))) &&
             !is.na(suppressWarnings(as.numeric(r$oi_cap_put)))) {
    list(oi_cap_call = as.numeric(r$oi_cap_call),
         oi_cap_put  = as.numeric(r$oi_cap_put),
         chain_state = r$chain_state,
         reason      = paste0("live/DB failed (", chain_r$reason, "); using scanner CSV"))
  } else {
    list(oi_cap_call = NA_real_, oi_cap_put = NA_real_,
         chain_state = NA_character_, reason = chain_r$reason)
  }

  # Structures: live pricer runs whenever TWS is reachable; otherwise we
  # surface FETCH FAILED instead of issuing a request that may hang. Two
  # expiries enumerated side-by-side (~30 DTE and ~55 DTE).
  structures <- enumerate_structures(ticker, direction, spot, expiries, vehicle,
                                     config, expiry_reason = expiry_reason,
                                     tws_ok = tws_ok)
  within <- if ("within_cap" %in% names(structures))
              structures$within_cap else logical(0)
  n_within <- sum(within, na.rm = TRUE)

  # Entry framework: ALWAYS live-derive (CSV is LONG-only). Step 4 2026-05-12.
  rr_obj <- .live_entry_framework(ticker, direction, vehicle, spot, expiry,
                                   targets, chain, phase_c, config)

  # Outright option grid (single-leg long-option pricing across strikes ×
  # expiries). Always enumerated when TWS is reachable — the user reads it
  # as informational regardless of vehicle rule's preference.
  iv_outright <- if (!is.null(phase_c$funnel) && !is.na(phase_c$funnel$iv30))
                   phase_c$funnel$iv30 else 0.30
  outrights <- tryCatch(enumerate_outrights(
    direction, spot, expiries, rr_obj$effective_target %||% targets$spot_target_low,
    iv_outright, config),
    error = function(e) { message("outrights enum failed: ", conditionMessage(e)); NULL })

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
    outrights         = outrights,
    spot              = spot
  )
}

#' Live-compute direction-aware structural targets via 300-day OHLC + shared
#' compute_structural_target(). For shorts, targets are below current price
#' (prior swing lows / 52w low / round below). For longs, above.
.live_targets <- function(ticker, spot, direction = "long") {
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
  res <- tryCatch(compute_structural_target(spot, raw$Close, raw$High,
                                             hist_low = raw$Low,
                                             direction = direction),
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
    direction        = direction,
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

  # Effective target: direction-aware OI cap.
  # Long: cap by oi_cap_call (resistance above) if it's closer than the
  #       structural target (spot_target_low is the closer level above).
  # Short: cap by oi_cap_put (support below) if it's closer than the
  #        structural target (spot_target_low is the closer level below).
  eff_target <- if (direction == "long") {
    if (!is.null(chain$oi_cap_call) && !is.na(chain$oi_cap_call) &&
        chain$oi_cap_call < spot_target_low) chain$oi_cap_call
    else spot_target_low
  } else {  # short — pick the higher (less aggressive) of OI put cap vs structural
    if (!is.null(chain$oi_cap_put) && !is.na(chain$oi_cap_put) &&
        chain$oi_cap_put > spot_target_low) chain$oi_cap_put
    else spot_target_low
  }

  # IV: prefer phase_c funnel iv30, else cheap_score-side IVP estimate, else 0.30.
  iv_now <- NA_real_
  if (!is.null(phase_c$funnel) && !is.na(phase_c$funnel$iv30)) iv_now <- phase_c$funnel$iv30
  if (is.na(iv_now)) iv_now <- 0.30

  # Strike picks — direction-aware.
  # Long outright (call): ATM strike rounded UP to $5 grid (slight OTM call).
  # Short outright (put): ATM strike rounded DOWN to $5 grid (slight OTM put).
  # Spread (long debit / bear-put debit):
  #   long leg = ATM rounded to $5 grid; short leg = at eff_target rounded.
  #   Constraint: for long debit call, long < short. For bear-put debit,
  #   long > short (you buy the higher put and sell the lower).
  bs_right <- if (direction == "long") "Call" else "Put"
  strike_long <- NA_real_; strike_short <- NA_real_
  if (vehicle %in% c("call", "put")) {
    strike_long <- if (direction == "long") ceiling(spot / 5) * 5
                   else                       floor(spot / 5) * 5
  } else if (vehicle == "spread") {
    strike_long  <- round(spot / 5) * 5
    strike_short <- round(eff_target / 5) * 5
    if (direction == "long" && !is.na(strike_short) && strike_short <= strike_long)
      strike_short <- strike_long + 5
    if (direction == "short" && !is.na(strike_short) && strike_short >= strike_long)
      strike_short <- strike_long - 5
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
  } else if (vehicle %in% c("call", "put") && !is.na(strike_long)) {
    tryCatch(Tbasics::getOptPrice(
      type = bs_right,
      S = spot, K = strike_long, r = 0.045, DTE = dte, sig = iv_now),
      error = function(e) NA_real_)
  } else if (vehicle == "spread" && !is.na(strike_long) && !is.na(strike_short)) {
    long_p  <- tryCatch(Tbasics::getOptPrice(
      type = bs_right, S = spot, K = strike_long,  r = 0.045, DTE = dte, sig = iv_now),
      error = function(e) NA_real_)
    short_p <- tryCatch(Tbasics::getOptPrice(
      type = bs_right, S = spot, K = strike_short, r = 0.045, DTE = dte, sig = iv_now),
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
    rr_min = config$rr_min,
    direction = direction),
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

# ── Outright option enumeration ──────────────────────────────────────────
#
# For vehicle ∈ {"call","put"}, enumerate a strike × expiry grid of single-leg
# long-option pricing. Mirrors enumerate_structures() but for outrights.
# Step "outrights" rewrite 2026-05-12 (project_analyze_redesign_2026_05.md).
#
# Each row: expiry, dte, strike, entry_premium, fwd_premium_at_target,
# max_loss (=entry_premium), reward, rr.
# Strikes: $5 grid spanning [spot-20%, spot+10%] for puts (short),
# [spot-10%, spot+20%] for calls (long). 5 strikes typical.
enumerate_outrights <- function(direction, spot, expiries, eff_target, iv_now,
                                 config) {
  if (is.null(direction) || !direction %in% c("long", "short"))
    return(NULL)
  if (is.na(spot) || is.na(eff_target) || is.na(iv_now))
    return(NULL)
  expiries <- expiries[!is.na(expiries) & nzchar(expiries)]
  if (length(expiries) == 0) return(NULL)

  bs_type <- if (direction == "short") "Put" else "Call"
  # Strike grid: 5 strikes on $5 increments around ATM, biased toward the
  # direction of the move (puts skew OTM-toward-target for shorts; calls
  # skew OTM-toward-target for longs).
  if (direction == "short") {
    atm <- floor(spot / 5) * 5
    strikes <- atm + c(-10, -5, 0, 5)  # OTM to slightly-ITM puts
  } else {
    atm <- ceiling(spot / 5) * 5
    strikes <- atm + c(-5, 0, 5, 10)   # slightly-ITM to OTM calls
  }
  strikes <- strikes[strikes > 0]

  rows <- list()
  for (exp in expiries) {
    exp_dt <- tryCatch(as.Date(as.character(exp), format = "%Y%m%d"),
                       error = function(e) NA)
    dte <- if (inherits(exp_dt, "Date") && !is.na(exp_dt))
             as.integer(exp_dt - Sys.Date()) else NA_integer_
    if (is.na(dte) || dte <= 0) next
    fwd_dte <- max(dte - 5, 1)  # theta buffer
    fwd_iv  <- iv_now + 0.02
    for (K in strikes) {
      entry_prem <- tryCatch(Tbasics::getOptPrice(
        type = bs_type, S = spot, K = K,
        r = 0.045, DTE = dte, sig = iv_now),
        error = function(e) NA_real_)
      fwd_prem <- tryCatch(Tbasics::getOptPrice(
        type = bs_type, S = eff_target, K = K,
        r = 0.045, DTE = fwd_dte, sig = fwd_iv),
        error = function(e) NA_real_)
      if (is.na(entry_prem) || entry_prem <= 0) next
      reward <- if (is.na(fwd_prem)) NA_real_ else fwd_prem - entry_prem
      rr <- if (!is.na(reward) && entry_prem > 0) reward / entry_prem else NA_real_
      rows[[length(rows) + 1]] <- data.frame(
        expiry = exp, dte = dte, strike = K,
        entry_premium = round(entry_prem * 100, 2),    # per-lot $ cost
        fwd_premium = if (is.na(fwd_prem)) NA_real_ else round(fwd_prem * 100, 2),
        max_loss = round(entry_prem * 100, 2),
        reward = if (is.na(reward)) NA_real_ else round(reward * 100, 2),
        rr = if (is.na(rr)) NA_real_ else round(rr, 2),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(NULL)
  df <- do.call(rbind, rows)
  df[order(-df$rr), , drop = FALSE]
}

# ── Structure enumeration (live spread pricer; FETCH FAILED row otherwise) ──
#
# Step 4 rewrite 2026-05-12:
#  - `expiries` is a vector — typically two values (~30 DTE, ~50-60 DTE).
#  - Each row carries an explicit `expiry` column.
#  - Filtered to DEBIT-only for the trade direction (the actual directional
#    bet with limited risk). CREDIT spreads are dropped.
#  - within_cap=FALSE rows dropped.
#  - Phantom rows dropped: max_risk < $5 (BS pricing artifact when both legs
#    round to zero).
#  - Sorted by expected_value descending.
enumerate_structures <- function(ticker, direction, spot, expiries, vehicle,
                                 config, expiry_reason = NULL, tws_ok = TRUE) {
  cap <- config$risk_cap_lot_usd
  right <- if (direction == "long") "C" else "P"

  if (!isTRUE(tws_ok)) return(.fetch_failed_structures(
    "TWS not reachable — cannot price spreads"))
  if (is.na(spot)) return(.fetch_failed_structures(
    "spot price unavailable — cannot price spreads"))
  expiries <- expiries[!is.na(expiries) & nzchar(expiries)]
  if (length(expiries) == 0) return(.fetch_failed_structures(
    paste0("no expiries available",
           if (!is.null(expiry_reason)) paste0(" (", expiry_reason, ")") else "")))
  if (!requireNamespace("reticulate", quietly = TRUE)) return(.fetch_failed_structures(
    "reticulate package unavailable — cannot reach live pricer"))

  spread_mod <- tryCatch(reticulate::import("tdata_py.spread", delay_load = TRUE),
                         error = function(e) NULL)
  if (is.null(spread_mod)) return(.fetch_failed_structures(
    "tdata_py.spread import failed"))

  rows <- list(); failures <- character(0)
  for (exp in expiries) {
    for (w in config$spread_widths) {
      df <- tryCatch(spread_mod$compute_spread_risk_reward(
        sym = ticker, trading_class = ticker, expiration = exp,
        current_price = spot, moneyness_pct = config$moneyness_pct,
        spread_width = as.integer(w), right = right,
        multiplier = 100L, currency = "USD",
        exchangeSec = "SMART", exchangeOpt = "SMART",
        force_refresh = TRUE),
        error = function(e) {
          failures <<- c(failures,
                         sprintf("exp=%s w=%s: %s", exp, w, conditionMessage(e)))
          NULL
        })
      if (!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
        df$expiry <- exp
        df$source <- "live"
        rows[[length(rows) + 1]] <- df
      }
    }
  }

  if (length(rows) == 0) return(.fetch_failed_structures(
    if (length(failures) > 0)
      paste("compute_spread_risk_reward returned no rows;", paste(failures, collapse = "; "))
    else
      "compute_spread_risk_reward returned no rows for any width"))

  spreads_df <- do.call(rbind, rows)

  # Filter: DEBIT-only for the direction (verticals that pay off on the move
  # you want, with limited capped risk).
  if ("spread_type" %in% names(spreads_df)) {
    spreads_df <- spreads_df[spreads_df$spread_type == "DEBIT", , drop = FALSE]
  }

  # Phantom rows: BS pricing artifact when both legs round to zero.
  spreads_df <- spreads_df[!is.na(spreads_df$max_risk) &
                            spreads_df$max_risk >= 5, , drop = FALSE]

  # within_cap flag
  spreads_df$within_cap <- spreads_df$max_risk <= cap
  spreads_df <- spreads_df[isTRUE(spreads_df$within_cap) |
                            spreads_df$within_cap, , drop = FALSE]

  if (nrow(spreads_df) == 0) return(.fetch_failed_structures(
    "no DEBIT spreads survived the within-cap + phantom filter"))

  # Sort by expected_value descending. Fall back to RR if EV missing.
  sort_key <- if ("expected_value" %in% names(spreads_df))
                -spreads_df$expected_value
              else -spreads_df$reward_risk_ratio
  spreads_df <- spreads_df[order(sort_key), , drop = FALSE]
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
