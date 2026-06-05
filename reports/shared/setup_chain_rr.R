# reports/shared/setup_chain_rr.R — shared Phase D primitives.
#
# Single source of truth for swing_scanner (universe) and /analyze
# (per-ticker drill-down). Functions exposed:
#   compute_structural_target() — D.2 spot-target consensus
#   walk_chain_oi()              — D.3 per-strike OI walk against DB cache
#   compute_rr_entry()           — D.4 R:R + entry-band BS-inversion
#   classify_entry_state()       — IN BAND / PRICED OUT / NO CHAIN
#
# Vehicle/expiry rule lives separately in shared/vehicle_rule.R.

#' Compute structural spot target consensus (Step D.2).
#'
#' Three independent sources:
#'   1. closest unbroken prior monthly swing high above current
#'   2. 52-week high if within 15% of current
#'   3. nearest round-number above on tiered grid:
#'      stock <  $50  → nearest $5
#'      stock $50-$200 → nearest $10
#'      stock > $200  → nearest $25
#'      $100 multiples supersede if closer
#'
#' Fib 1.272/1.618 is a CONFIRMATION OVERLAY only — counts only if it lands
#' within ±2% of one of the above. Lone Fib levels are ignored.
#'
#' @param price       numeric — current spot
#' @param hist_close  numeric vector — daily close history (most-recent last)
#' @param hist_high   numeric vector — daily high history (parallel to hist_close)
#' @return list with spot_target_low, spot_target_high, targets_agreeing,
#'              fib_confirms
#' Compute direction-aware structural target.
#'
#' Long: target is ABOVE current — prior swing highs, 52w high, round number above.
#' Short: target is BELOW current — prior swing lows, 52w low, round number below.
#' Fib 1.272/1.618 confirmation overlay applies symmetrically (retracement up
#' for longs, retracement down for shorts).
#'
#' @param price       numeric — current spot
#' @param hist_close  numeric vector — daily close history (most-recent last)
#' @param hist_high   numeric vector — daily high (used for long)
#' @param hist_low    numeric vector — daily low (used for short)
#' @param direction   "long" or "short". Defaults to "long" for backwards
#'                    compatibility with existing swing_scanner callers.
#' @return list(spot_target_low, spot_target_high, targets_agreeing,
#'              fib_confirms, source_levels)
#'   For shorts, spot_target_low/high are still labelled "low/high" but
#'   represent the *closer/farther* downside levels (low = closer to spot).
compute_structural_target <- function(price, hist_close, hist_high,
                                       hist_low = NULL, direction = "long",
                                       move_lookback = 40) {
  if (direction == "long") {
    .structural_target_long(price, hist_close, hist_high, move_lookback)
  } else if (direction == "short") {
    .structural_target_short(price, hist_close,
                              if (is.null(hist_low)) hist_high else hist_low,
                              move_lookback)
  } else {
    stop(sprintf("compute_structural_target: unknown direction '%s'", direction))
  }
}

.structural_target_long <- function(price, hist_close, hist_high,
                                     move_lookback = 40) {
  if (length(hist_high) < 60) {
    return(list(spot_target_low = NA_real_, spot_target_high = NA_real_,
                targets_agreeing = 0L, fib_confirms = FALSE,
                source_levels = c(), direction = "long",
                move_base = NA_real_, move_pct = NA_real_,
                move_fib = NA_character_, move_next_ext = NA_character_))
  }

  swing_window <- min(120, length(hist_high))
  recent_highs <- tail(hist_high, swing_window)
  recent_closes <- tail(hist_close, swing_window)
  swing_candidates <- numeric(0)
  for (i in 21:(length(recent_highs) - 5)) {
    is_local_max <- recent_highs[i] == max(recent_highs[max(1, i-20):min(length(recent_highs), i+5)])
    if (is_local_max && recent_highs[i] > price) {
      remaining <- recent_closes[(i+1):length(recent_closes)]
      if (length(remaining) == 0 || max(remaining, na.rm = TRUE) < recent_highs[i]) {
        swing_candidates <- c(swing_candidates, recent_highs[i])
      }
    }
  }
  swing_high <- if (length(swing_candidates) > 0) min(swing_candidates) else NA_real_

  yr_window <- min(252, length(hist_high))
  yr_high <- max(tail(hist_high, yr_window), na.rm = TRUE)
  yr_high_in_reach <- if (!is.na(yr_high) && yr_high > price &&
                          (yr_high - price) / price <= 0.15) yr_high else NA_real_

  round_target <- .nearest_round_above(price)

  candidates <- c(swing_high, yr_high_in_reach, round_target)
  candidates <- candidates[!is.na(candidates) & candidates > price]
  .finalize_targets(candidates, price, hist_close, direction = "long",
                    move_lookback = move_lookback)
}

.structural_target_short <- function(price, hist_close, hist_low,
                                      move_lookback = 40) {
  if (length(hist_low) < 60) {
    return(list(spot_target_low = NA_real_, spot_target_high = NA_real_,
                targets_agreeing = 0L, fib_confirms = FALSE,
                source_levels = c(), direction = "short",
                move_base = NA_real_, move_pct = NA_real_,
                move_fib = NA_character_, move_next_ext = NA_character_))
  }

  swing_window <- min(120, length(hist_low))
  recent_lows <- tail(hist_low, swing_window)
  recent_closes <- tail(hist_close, swing_window)
  swing_candidates <- numeric(0)
  for (i in 21:(length(recent_lows) - 5)) {
    is_local_min <- recent_lows[i] == min(recent_lows[max(1, i-20):min(length(recent_lows), i+5)])
    if (is_local_min && recent_lows[i] < price) {
      remaining <- recent_closes[(i+1):length(recent_closes)]
      if (length(remaining) == 0 || min(remaining, na.rm = TRUE) > recent_lows[i]) {
        swing_candidates <- c(swing_candidates, recent_lows[i])
      }
    }
  }
  swing_low <- if (length(swing_candidates) > 0) max(swing_candidates) else NA_real_

  yr_window <- min(252, length(hist_low))
  yr_low <- min(tail(hist_low, yr_window), na.rm = TRUE)
  yr_low_in_reach <- if (!is.na(yr_low) && yr_low < price &&
                          (price - yr_low) / price <= 0.15) yr_low else NA_real_

  round_target <- .nearest_round_below(price)

  candidates <- c(swing_low, yr_low_in_reach, round_target)
  candidates <- candidates[!is.na(candidates) & candidates < price]
  .finalize_targets(candidates, price, hist_close, direction = "short",
                    move_lookback = move_lookback)
}

#' Fib 1.272/1.618 confirmation overlay — direction-aware. TRUE if a projected
#' Fib extension lands within ±2% of either target endpoint. `spot_target_high`
#' may be NA (single corroborated target) — only the set endpoints are tested.
.fib_overlay <- function(spot_target_low, spot_target_high, hist_close, direction) {
  recent_60 <- tail(hist_close, min(60, length(hist_close)))
  if (length(recent_60) < 20) return(FALSE)
  tgts <- c(spot_target_low, spot_target_high)
  tgts <- tgts[!is.na(tgts)]
  if (length(tgts) == 0) return(FALSE)
  hit <- function(fib) any(abs(fib - tgts) / abs(tgts) <= 0.02)
  if (direction == "long") {
    anchor_idx <- which.min(recent_60)
    if (anchor_idx >= length(recent_60)) return(FALSE)
    peak <- max(recent_60[(anchor_idx + 1):length(recent_60)], na.rm = TRUE)
    if (peak <= recent_60[anchor_idx]) return(FALSE)
    leg <- peak - recent_60[anchor_idx]
    for (fib in c(recent_60[anchor_idx] + leg * 1.272,
                   recent_60[anchor_idx] + leg * 1.618))
      if (hit(fib)) return(TRUE)
  } else {  # short
    anchor_idx <- which.max(recent_60)
    if (anchor_idx >= length(recent_60)) return(FALSE)
    trough <- min(recent_60[(anchor_idx + 1):length(recent_60)], na.rm = TRUE)
    if (trough >= recent_60[anchor_idx]) return(FALSE)
    leg <- recent_60[anchor_idx] - trough
    for (fib in c(recent_60[anchor_idx] - leg * 1.272,
                   recent_60[anchor_idx] - leg * 1.618))
      if (hit(fib)) return(TRUE)
  }
  FALSE
}

#' Swing base that launched the CURRENT leg, within the supplied close window.
#' ZigZag detection: a pivot is confirmed when price reverses by >= `th` from a
#' running extreme. For a long we return the swing LOW that started the current
#' up-leg (the running min if price is mid-pullback, else the last confirmed
#' swing low); mirror for a short. This anchors on the latest leg — in a
#' stair-step uptrend it picks the most recent higher-low, not the stale low of
#' the whole window. Falls back to the windowed extreme when no >= th reversal
#' exists inside the cap (one uninterrupted run). `th` = reversal fraction
#' (default 4%, a normal breakout-base pullback).
.recent_swing_anchor <- function(recent, direction, th = 0.04) {
  recent <- recent[!is.na(recent)]            # Yahoo's in-progress bar = NA close
  n <- length(recent)
  ext <- if (direction == "long") min(recent, na.rm = TRUE)
         else                     max(recent, na.rm = TRUE)
  if (n < 3) return(ext)
  run_max <- recent[1]; run_min <- recent[1]
  dir <- 0L                       # 0 unknown, 1 up-leg, -1 down-leg
  last_swing_low <- NA_real_; last_swing_high <- NA_real_
  for (i in 2:n) {
    if (recent[i] > run_max) run_max <- recent[i]
    if (recent[i] < run_min) run_min <- recent[i]
    if (dir >= 0 && recent[i] <= run_max * (1 - th)) {       # reversal down
      last_swing_high <- run_max; dir <- -1L; run_min <- recent[i]
    } else if (dir <= 0 && recent[i] >= run_min * (1 + th)) { # reversal up
      last_swing_low <- run_min; dir <- 1L; run_max <- recent[i]
    }
  }
  if (direction == "long") {
    base <- if (dir < 0) run_min
            else if (!is.na(last_swing_low)) last_swing_low else ext
    if (is.na(base) || base >= recent[n]) base <- ext
    base
  } else {
    base <- if (dir > 0) run_max
            else if (!is.na(last_swing_high)) last_swing_high else ext
    if (is.na(base) || base <= recent[n]) base <- ext
    base
  }
}

#' Move-maturity overlay for the Fib/structural block. Expresses how far the
#' current move has travelled from its swing base toward the nearest structural
#' target (leg_top), as (a) % of the base->leg_top leg and (b) the nearest
#' Fibonacci rung, plus the next extension rung as a forward price marker.
#' Close-based. Direction-aware:
#'   long  — base = recent swing low,  travel up toward a higher leg_top
#'   short — base = recent swing high, travel down toward a lower leg_top
#' `window` caps the lookback for the swing base (default 40 ~= the 2-4 week
#' breakout horizon; tunable via analyze.move_lookback_days). The base is the
#' most recent swing pivot in that window, NOT the windowed extreme.
#' Returns list(base, pct, ratio, label, next_ext); NA fields on bad inputs.
.move_extension <- function(price, hist_close, leg_top, direction,
                            window = 40) {
  na <- list(base = NA_real_, pct = NA_real_, ratio = NA_real_,
             label = NA_character_, next_ext = NA_character_)
  hist_close <- hist_close[!is.na(hist_close)]   # drop Yahoo's NA in-progress bar
  if (is.na(price) || is.na(leg_top) || length(hist_close) < 10) return(na)
  recent <- tail(hist_close, min(window, length(hist_close)))
  base <- .recent_swing_anchor(recent, direction)
  span <- if (direction == "long") leg_top - base else base - leg_top
  if (is.na(base) || is.na(span) || span <= 0)
    return(modifyList(na, list(base = round(base, 2))))
  travel <- if (direction == "long") price - base else base - price
  ratio  <- travel / span
  rungs  <- c(0, 0.236, 0.382, 0.5, 0.618, 0.786, 1.0, 1.272, 1.618, 2.0)
  near   <- rungs[which.min(abs(rungs - ratio))]
  label  <- if (near >= 1.272) sprintf("%.3f ext", near) else sprintf("%.3f", near)
  # Next true extension rung (> 1.0) above the current ratio, priced out as a
  # forward level — "where the move would push beyond the wall / get extended".
  above  <- rungs[rungs > ratio + 1e-9 & rungs > 1.0]
  next_ext <- if (length(above) > 0) {
    r   <- above[1]
    lvl <- if (direction == "long") base + r * span else base - r * span
    sprintf("%.3f = %.2f", r, lvl)
  } else NA_character_
  list(base = round(base, 2), pct = round(ratio * 100, 1),
       ratio = round(ratio, 3), label = label, next_ext = next_ext)
}

#' Pick the structural-target band and tally agreement. Direction-symmetric.
#' For long: candidates sorted ascending, closer = lower price (closer to spot).
#' For short: candidates sorted descending, closer = higher price (closer to spot).
#'
#' Coincident levels: the nearest unbroken swing high is frequently ALSO the
#' 52-week high (literally the same bar), so two of the three sources return the
#' identical value. Rather than emit a zero-width band (spot_target_low ==
#' spot_target_high), a level corroborated by >=2 sources within ±2% is reported
#' as a SINGLE target (spot_target_high = NA) with targets_agreeing = its support
#' count. Genuinely distinct levels still form a low/high band as before.
.finalize_targets <- function(candidates, price, hist_close, direction,
                              move_lookback = 40) {
  if (direction == "long") candidates <- sort(candidates)
  else                     candidates <- sort(candidates, decreasing = TRUE)

  if (length(candidates) == 0) {
    return(list(spot_target_low = NA_real_, spot_target_high = NA_real_,
                targets_agreeing = 0L, fib_confirms = FALSE,
                source_levels = numeric(0), direction = direction,
                move_base = NA_real_, move_pct = NA_real_,
                move_fib = NA_character_, move_next_ext = NA_character_))
  }

  if (length(candidates) < 2) {
    spot_target_low  <- candidates[1]
    spot_target_high <- NA_real_
    agreeing <- 1L
  } else {
    # Cluster near-coincident levels (±2%, matching the agreement tooltip) into
    # distinct structural levels, tracking how many sources support each.
    reps <- numeric(0); counts <- integer(0)
    for (cand in candidates) {
      if (length(reps) > 0 &&
          abs(cand - reps[length(reps)]) / abs(reps[length(reps)]) <= 0.02) {
        counts[length(counts)] <- counts[length(counts)] + 1L
      } else {
        reps <- c(reps, cand); counts <- c(counts, 1L)
      }
    }

    if (max(counts) >= 2L) {
      # A corroborated level exists — report it as a single target (closest such
      # level to spot wins ties; reps stay in proximity order from the sort).
      primary_idx <- which.max(counts)
      spot_target_low  <- reps[primary_idx]
      spot_target_high <- NA_real_
      agreeing <- min(as.integer(max(counts)), 3L)
    } else {
      # No coincidence — tightest adjacent pair is the consensus band.
      spreads <- abs(diff(candidates)) / abs(candidates[-length(candidates)])
      best_idx <- which.min(spreads)
      spot_target_low  <- candidates[best_idx]      # closer to spot
      spot_target_high <- candidates[best_idx + 1]  # farther from spot
      agreeing <- if (spreads[best_idx] <= 0.05) 2L else 1L
      if (length(candidates) >= 3 && spreads[best_idx] <= 0.05 &&
          length(spreads) >= best_idx + 1 && spreads[best_idx + 1] <= 0.05)
        agreeing <- 3L
    }
  }

  fib_confirms <- .fib_overlay(spot_target_low, spot_target_high,
                               hist_close, direction)

  # Move-maturity overlay: leg_top = nearest structural target (the wall the
  # move is testing). Base = most recent swing pivot within move_lookback days,
  # sized to the 2-4 week breakout horizon (not the multi-month swing low).
  move <- .move_extension(price, hist_close, spot_target_low, direction,
                          window = move_lookback)

  list(spot_target_low = round(spot_target_low, 2),
       spot_target_high = if (is.na(spot_target_high)) NA_real_
                          else round(spot_target_high, 2),
       targets_agreeing = as.integer(agreeing),
       fib_confirms = fib_confirms,
       source_levels = round(candidates, 2),
       direction = direction,
       move_base = move$base, move_pct = move$pct,
       move_fib = move$label, move_next_ext = move$next_ext,
       move_lookback = move_lookback)
}

#' Nearest round number above current price on tiered grid.
.nearest_round_above <- function(price) {
  grid <- if (price < 50) 5
          else if (price < 200) 10
          else 25
  candidate <- ceiling(price / grid) * grid
  hundred_mult <- ceiling(price / 100) * 100
  if (hundred_mult > price && (hundred_mult - price) < (candidate - price)) {
    candidate <- hundred_mult
  }
  if (candidate <= price) candidate <- candidate + grid
  candidate
}

#' Nearest round number BELOW current price on the tiered grid.
.nearest_round_below <- function(price) {
  grid <- if (price < 50) 5
          else if (price < 200) 10
          else 25
  candidate <- floor(price / grid) * grid
  hundred_mult <- floor(price / 100) * 100
  if (hundred_mult > 0 && hundred_mult < price &&
      (price - hundred_mult) < (price - candidate)) {
    candidate <- hundred_mult
  }
  if (candidate >= price) candidate <- candidate - grid
  if (candidate <= 0) candidate <- price * 0.5  # floor — never go negative
  candidate
}

#' Per-strike chain OI walk (Step D.3). Median over last ~5 trading days from
#' option_chain_oi_history. Caller passes a DBI connection.
walk_chain_oi <- function(sym, expiry, spot_target_low, spot_target_high,
                          current_price, conn) {
  cutoff <- format(Sys.Date() - 7, "%Y-%m-%d")
  oi <- tryCatch(DBI::dbGetQuery(conn,
    "SELECT strike, right, AVG(open_interest) AS oi_avg,
            COUNT(*) AS n_days, AVG(qualified) AS qual_rate
     FROM option_chain_oi_history
     WHERE sym = ? AND expiry = ? AND cache_date >= ?
       AND open_interest IS NOT NULL
     GROUP BY strike, right",
    params = list(sym, as.character(expiry), cutoff)),
    error = function(e) data.frame())

  if (nrow(oi) == 0) {
    return(list(oi_cap_call = NA_real_, oi_cap_call_magnitude = NA_integer_,
                oi_cap_put = NA_real_, oi_cap_put_magnitude = NA_integer_,
                oi_concentration_pct = NA_real_, total_chain_oi = NA_integer_,
                chain_state = "NO DATA",
                effective_target = spot_target_low,
                chain_walk_status = "FAILED"))
  }

  calls <- oi[oi$right == "C" & oi$strike > current_price, ]
  puts  <- oi[oi$right == "P" & oi$strike < current_price, ]

  oi_cap_call <- if (nrow(calls) > 0) calls$strike[which.max(calls$oi_avg)] else NA_real_
  oi_cap_call_mag <- if (nrow(calls) > 0) round(max(calls$oi_avg)) else NA_integer_
  oi_cap_put <- if (nrow(puts) > 0) puts$strike[which.max(puts$oi_avg)] else NA_real_
  oi_cap_put_mag <- if (nrow(puts) > 0) round(max(puts$oi_avg)) else NA_integer_

  in_band <- oi[oi$strike >= current_price & oi$strike <= spot_target_low, ]
  oi_in_band <- sum(in_band$oi_avg, na.rm = TRUE)
  total_oi <- sum(oi$oi_avg, na.rm = TRUE)
  oi_conc <- if (total_oi > 0) round(oi_in_band / total_oi * 100, 1) else NA_real_

  chain_state <- if (is.na(oi_cap_call)) "NO DATA"
                 else if (oi_cap_call < spot_target_low) "chain-capped"
                 else if (oi_cap_call > spot_target_high) "structurally bounded"
                 else "mixed"

  effective_target <- if (chain_state == "chain-capped") oi_cap_call else spot_target_low

  status <- if (mean(oi$qual_rate, na.rm = TRUE) >= 0.95) "OK" else "PARTIAL"

  list(oi_cap_call = oi_cap_call, oi_cap_call_magnitude = oi_cap_call_mag,
       oi_cap_put = oi_cap_put, oi_cap_put_magnitude = oi_cap_put_mag,
       oi_concentration_pct = oi_conc,
       total_chain_oi = round(total_oi),
       chain_state = chain_state,
       effective_target = effective_target,
       chain_walk_status = status)
}

#' Compute Risk:Reward and entry interval (Step D.4). Direction-aware.
#'
#' @param vehicle "stock" | "call" | "put" | "spread"
#' @param direction "long" or "short". For "put" vehicle or short spreads,
#'   forward pricing uses type="Put". For "long" + "call" vehicle, type="Call".
#'   Default "long" preserves swing_scanner caller behavior.
#' @param spread_short_strike — for a bear-put debit (short direction spread),
#'   this is the LOWER strike (the one you sell). long_strike > short_strike.
#'   For a bull-call debit (long direction spread), long_strike < short_strike.
compute_rr_entry <- function(vehicle, strike, expiry, current_price,
                             effective_target, iv_now, entry_premium,
                             spread_short_strike = NA, spot_target_high = NA,
                             risk_free = 0.045, iv_bump = 0.02, theta_buffer = 5,
                             rr_min = 0.5, direction = "long") {
  if (vehicle == "stock") {
    risk <- entry_premium
    reward <- if (direction == "long") effective_target - current_price
              else                      current_price - effective_target
    rr <- if (risk > 0) reward / risk else NA_real_
    return(list(rr = round(rr, 2),
                entry_floor = round(current_price, 2),
                entry_ceiling = round(current_price, 2),
                headroom_band = sprintf("%s%.0f%%",
                                        if (direction == "long") "+" else "-",
                                        abs(reward) / current_price * 100),
                reward = round(reward, 2)))
  }

  expiry_dt <- as.Date(as.character(expiry), format = "%Y%m%d")
  dte <- as.integer(expiry_dt - Sys.Date())
  fwd_dte <- max(dte - theta_buffer, 1)
  fwd_iv <- iv_now + iv_bump
  bs_type <- if (vehicle == "put" ||
                  (vehicle == "spread" && direction == "short")) "Put" else "Call"

  if (vehicle %in% c("call", "put")) {
    fwd_price <- tryCatch(
      Tbasics::getOptPrice(type = bs_type, S = effective_target, K = strike,
                           r = risk_free, DTE = fwd_dte, sig = fwd_iv),
      error = function(e) NA_real_)
    risk <- entry_premium
    reward <- if (!is.na(fwd_price)) fwd_price - entry_premium else NA_real_
    rr <- if (!is.na(reward) && risk > 0) reward / risk else NA_real_
    entry_ceiling <- if (!is.na(fwd_price)) fwd_price / (1 + rr_min) else NA_real_
    headroom_band <- if (!is.na(spot_target_high) && !is.na(fwd_price) &&
                          ((direction == "long"  && spot_target_high > effective_target) ||
                           (direction == "short" && spot_target_high < effective_target))) {
      fwd_price_high <- tryCatch(
        Tbasics::getOptPrice(type = bs_type, S = spot_target_high, K = strike,
                             r = risk_free, DTE = fwd_dte, sig = fwd_iv),
        error = function(e) NA_real_)
      if (!is.na(fwd_price_high))
        sprintf("+%.0f%% to +%.0f%%",
                (fwd_price - entry_premium) / entry_premium * 100,
                (fwd_price_high - entry_premium) / entry_premium * 100)
      else
        sprintf("+%.0f%%", (fwd_price - entry_premium) / entry_premium * 100)
    } else if (!is.na(reward)) {
      sprintf("+%.0f%%", reward / entry_premium * 100)
    } else "n/a"
    return(list(rr = round(rr, 2),
                entry_floor = round(entry_premium, 3),
                entry_ceiling = round(entry_ceiling, 3),
                headroom_band = headroom_band,
                reward = round(reward, 3)))
  }

  if (vehicle == "spread") {
    if (is.na(spread_short_strike)) {
      return(list(rr = NA_real_, entry_floor = round(entry_premium, 3),
                  entry_ceiling = NA_real_, headroom_band = "n/a",
                  reward = NA_real_))
    }
    fwd_long <- tryCatch(
      Tbasics::getOptPrice(type = bs_type, S = effective_target, K = strike,
                           r = risk_free, DTE = fwd_dte, sig = fwd_iv),
      error = function(e) NA_real_)
    fwd_short <- tryCatch(
      Tbasics::getOptPrice(type = bs_type, S = effective_target,
                           K = spread_short_strike,
                           r = risk_free, DTE = fwd_dte, sig = fwd_iv),
      error = function(e) NA_real_)
    if (is.na(fwd_long) || is.na(fwd_short))
      return(list(rr = NA_real_, entry_floor = round(entry_premium, 3),
                  entry_ceiling = NA_real_, headroom_band = "n/a",
                  reward = NA_real_))
    fwd_spread <- max(fwd_long - fwd_short, 0)
    # Max payoff depends on direction:
    # Long debit call: max = (short_strike - long_strike), short > long.
    # Bear debit put:  max = (long_strike - short_strike), long > short.
    max_payoff <- if (direction == "short") strike - spread_short_strike
                  else                       spread_short_strike - strike
    if (is.na(max_payoff) || max_payoff <= 0)
      return(list(rr = NA_real_, entry_floor = round(entry_premium, 3),
                  entry_ceiling = NA_real_, headroom_band = "n/a",
                  reward = NA_real_))
    fwd_spread <- min(fwd_spread, max_payoff)
    risk <- entry_premium
    reward <- fwd_spread - entry_premium
    rr <- if (risk > 0) reward / risk else NA_real_
    entry_ceiling <- fwd_spread / (1 + rr_min)
    return(list(rr = round(rr, 2),
                entry_floor = round(entry_premium, 3),
                entry_ceiling = round(entry_ceiling, 3),
                headroom_band = sprintf("+%.0f%%", reward / entry_premium * 100),
                reward = round(reward, 3)))
  }

  list(rr = NA_real_, entry_floor = round(entry_premium, 3),
       entry_ceiling = NA_real_, headroom_band = "n/a", reward = NA_real_)
}

#' Determine Entry_State.
classify_entry_state <- function(entry_floor, entry_ceiling, chain_walk_status) {
  if (chain_walk_status == "FAILED") return("NO CHAIN")
  if (is.na(entry_floor) || is.na(entry_ceiling)) return("NO CHAIN")
  if (entry_floor <= entry_ceiling) "IN BAND" else "PRICED OUT"
}
