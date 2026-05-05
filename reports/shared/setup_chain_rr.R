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
compute_structural_target <- function(price, hist_close, hist_high) {
  if (length(hist_high) < 60) {
    return(list(spot_target_low = NA_real_, spot_target_high = NA_real_,
                targets_agreeing = 0L, fib_confirms = FALSE,
                source_levels = c()))
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
  candidates <- sort(candidates)

  if (length(candidates) < 2) {
    return(list(spot_target_low = if (length(candidates) == 1) candidates[1] else NA_real_,
                spot_target_high = NA_real_,
                targets_agreeing = length(candidates),
                fib_confirms = FALSE,
                source_levels = candidates))
  }

  spreads <- diff(candidates) / candidates[-length(candidates)]
  best_idx <- which.min(spreads)
  spot_target_low  <- candidates[best_idx]
  spot_target_high <- candidates[best_idx + 1]
  agreeing <- if (spreads[best_idx] <= 0.05) 2L else 1L
  if (length(candidates) >= 3 && spreads[best_idx] <= 0.05) {
    if (length(spreads) >= best_idx + 1 && spreads[best_idx + 1] <= 0.05)
      agreeing <- 3L
  }

  fib_confirms <- FALSE
  recent_60 <- tail(hist_close, min(60, length(hist_close)))
  if (length(recent_60) >= 20) {
    low_idx <- which.min(recent_60)
    if (low_idx < length(recent_60)) {
      after_low <- recent_60[(low_idx + 1):length(recent_60)]
      high_after <- max(after_low, na.rm = TRUE)
      if (high_after > recent_60[low_idx]) {
        leg <- high_after - recent_60[low_idx]
        fib1272 <- recent_60[low_idx] + leg * 1.272
        fib1618 <- recent_60[low_idx] + leg * 1.618
        for (fib in c(fib1272, fib1618)) {
          if (abs(fib - spot_target_low)  / spot_target_low  <= 0.02 ||
              abs(fib - spot_target_high) / spot_target_high <= 0.02) {
            fib_confirms <- TRUE
            break
          }
        }
      }
    }
  }

  list(spot_target_low = round(spot_target_low, 2),
       spot_target_high = round(spot_target_high, 2),
       targets_agreeing = as.integer(agreeing),
       fib_confirms = fib_confirms,
       source_levels = round(candidates, 2))
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

#' Compute Risk:Reward and entry interval (Step D.4).
compute_rr_entry <- function(vehicle, strike, expiry, current_price,
                             effective_target, iv_now, entry_premium,
                             spread_short_strike = NA, spot_target_high = NA,
                             risk_free = 0.045, iv_bump = 0.02, theta_buffer = 5,
                             rr_min = 0.5) {
  if (vehicle == "stock") {
    risk <- entry_premium
    reward <- effective_target - current_price
    rr <- if (risk > 0) reward / risk else NA_real_
    return(list(rr = round(rr, 2),
                entry_floor = round(current_price, 2),
                entry_ceiling = round(current_price, 2),
                headroom_band = sprintf("+%.0f%%", reward / current_price * 100),
                reward = round(reward, 2)))
  }

  expiry_dt <- as.Date(as.character(expiry), format = "%Y%m%d")
  dte <- as.integer(expiry_dt - Sys.Date())
  fwd_dte <- max(dte - theta_buffer, 1)
  fwd_iv <- iv_now + iv_bump

  if (vehicle == "call") {
    fwd_price <- tryCatch(
      Tbasics::getOptPrice(type = "Call", S = effective_target, K = strike,
                           r = risk_free, DTE = fwd_dte, sig = fwd_iv),
      error = function(e) NA_real_)
    risk <- entry_premium
    reward <- if (!is.na(fwd_price)) fwd_price - entry_premium else NA_real_
    rr <- if (!is.na(reward) && risk > 0) reward / risk else NA_real_
    entry_ceiling <- if (!is.na(fwd_price)) fwd_price / (1 + rr_min) else NA_real_
    headroom_band <- if (!is.na(spot_target_high) && !is.na(fwd_price) &&
                          spot_target_high > effective_target) {
      fwd_price_high <- tryCatch(
        Tbasics::getOptPrice(type = "Call", S = spot_target_high, K = strike,
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
      Tbasics::getOptPrice(type = "Call", S = effective_target, K = strike,
                           r = risk_free, DTE = fwd_dte, sig = fwd_iv),
      error = function(e) NA_real_)
    fwd_short <- tryCatch(
      Tbasics::getOptPrice(type = "Call", S = effective_target,
                           K = spread_short_strike,
                           r = risk_free, DTE = fwd_dte, sig = fwd_iv),
      error = function(e) NA_real_)
    if (is.na(fwd_long) || is.na(fwd_short))
      return(list(rr = NA_real_, entry_floor = round(entry_premium, 3),
                  entry_ceiling = NA_real_, headroom_band = "n/a",
                  reward = NA_real_))
    fwd_spread <- max(fwd_long - fwd_short, 0)
    max_payoff <- spread_short_strike - strike
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
