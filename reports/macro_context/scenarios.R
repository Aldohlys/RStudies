# scenarios.R — Behavioral regime detection for 2-4 week swing trading
#
# 3 regimes based on observable investor behavior:
#   1. LIQUIDITY STRESS — correlation → 1, forced selling, don't open trades
#   2. DIRECTIONAL FLOW — capital moving into identifiable sector groups
#   3. NEUTRAL — no clear direction, rely on stock-level technicals
#
# Signals are continuous (sigmoid functions), not binary flags.
# Sector flow scored by: RS momentum + volume confirmation + internal breadth momentum.
# Positioning stress from weekly COT data. Catalyst proximity from events.R.

library(DBI)

# ── Sigmoid function ──────────────────────────────────────────────────────────
# Maps any value to [0, 1] with smooth transition around center.
# At center: output = 0.5. At center ± 2*scale: output ≈ 0.12 or 0.88.
sig <- function(x, center, scale) {
  if (is.na(x)) return(0.5)
  1 / (1 + exp(-(x - center) / scale))
}

# ── Signal Computation ────────────────────────────────────────────────────────
# Converts raw market data into continuous [0,1] signals.
#
# Each signal has a tooltip description for the report:
# Parameters calibrated from 10yr historical medians/SDs (calibrate_from_history.R, 2026-03-26)
# center = median (0.5 = historically normal), scale = SD (full 0-1 range used)
SIGNAL_TOOLTIPS <- list(
  vix_stress    = "VIX stress level | sig(VIX, center=16.65, scale=7.33) | 0.5=normal, >0.85=extreme",
  vix_calm      = "VIX calm level | 1 - sig(VIX, center=16.65, scale=7.33) | inverse of stress",
  backwardation = "VIX term structure inversion | sig(1 - VIX/VIX3M, center=0.12, scale=0.08) | 1=severe backwardation",
  breadth_bull  = "Breadth bullish | sig(S5FI, center=50, scale=10) | >0.5 = majority above MA50",
  breadth_bear  = "Breadth bearish | 1 - sig(S5FI, center=35, scale=8) | high when S5FI < 35",
  rates_press   = "Rate pressure | sig(10Y, center=2.66, scale=1.19) | 0.5=normal, >0.85=high rates",
  dxy_strength  = "Dollar momentum | sig(DXY 20d ret, center=0.07, scale=1.77) | 0.5=flat, >0.85=strong rally",
  reflation     = "Reflation composite | mean of oil+gold strength, dollar weakness",
  tlt_bid       = "Bond bid | sig(TLT 20d ret, center=-0.33, scale=3.80) | 0.5=normal, >0.85=strong rally",
  credit_stress = "Credit stress | sig(-(HYG 20d ret), center=-0.16, scale=2.19) | 0.5=normal, >0.85=selloff",
  copper_gold   = "Risk appetite proxy | sig(CPER/GLD ratio change, center=-0.17, scale=6.27) | 0.5=normal, >0.85=growth>safety",
  sentiment     = "Sentiment composite | mean of VIX calm + breadth bull + credit health"
)

#' Compute all continuous signals from raw market data
#' @param vix_res VIX analysis results
#' @param rates_res Rates analysis results
#' @param breadth Breadth results
#' @param comm_res Commodities analysis results
#' @param raw Raw price data (for HYG, LQD, CPER, GLD series)
#' @return Named list of signals in [0, 1]
compute_signals <- function(vix_res, rates_res, breadth, comm_res, raw) {
  # Helper to get 20d return from raw data
  ret20 <- function(tk) {
    s <- raw |> dplyr::filter(ticker == tk) |> dplyr::arrange(date) |>
      dplyr::filter(!is.na(Close)) |> tail(30)
    if (nrow(s) < 20) return(NA_real_)
    (tail(s$Close, 1) / s$Close[max(1, nrow(s) - 19)] - 1) * 100
  }

  vix   <- vix_res$vix
  ratio <- vix_res$ratio  # VIX / VIX3M
  s5fi  <- breadth$pct
  y10   <- rates_res$y10
  dxy_ret <- comm_res$dxy_ret20
  uso_ret <- comm_res$uso_ret20
  gld_ret <- comm_res$gld_ret20
  tlt_ret <- rates_res$tlt_pct

  # Credit: HYG 20d return (negative = stress)
  hyg_ret <- ret20("HYG")
  lqd_ret <- ret20("LQD")
  credit_spread_ret <- if (!is.na(hyg_ret) && !is.na(lqd_ret)) hyg_ret - lqd_ret else NA

  # Copper/Gold ratio momentum
  cper_ret <- ret20("CPER")
  gld_ret2 <- ret20("GLD")
  copper_gold_ret <- if (!is.na(cper_ret) && !is.na(gld_ret2)) cper_ret - gld_ret2 else NA

  # Calibrated from 10yr historical data (calibrate_from_history.R, 2026-03-26)
  # center = median, scale = SD → 0.5 = historically normal
  signals <- list(
    vix_stress    = sig(vix, 16.65, 7.33),
    vix_calm      = 1 - sig(vix, 16.65, 7.33),
    backwardation = if (!is.na(ratio)) sig(1 - ratio, 0.12, 0.08) else 0,
    breadth_bull  = sig(s5fi, 50, 10),       # S5FI: no Yahoo history, keep manual
    breadth_bear  = 1 - sig(s5fi, 35, 8),    # S5FI: no Yahoo history, keep manual
    rates_press   = sig(y10, 2.66, 1.19),
    dxy_strength  = sig(dxy_ret, 0.07, 1.77),
    reflation     = mean(c(
      sig(uso_ret, 1.46, 11.02),
      sig(gld_ret, 0.76, 4.12),
      1 - sig(dxy_ret, 0.07, 1.77)
    ), na.rm = TRUE),
    tlt_bid       = sig(tlt_ret, -0.33, 3.80),
    credit_stress = if (!is.na(hyg_ret)) sig(-hyg_ret, -0.16, 2.19) else 0.3,
    copper_gold   = sig(copper_gold_ret, -0.17, 6.27),
    sentiment     = NA  # computed below as composite
  )

  # Sentiment composite: mean of calm + bullish breadth + credit health
  credit_health <- if (!is.na(hyg_ret)) sig(hyg_ret, 0.16, 2.19) else 0.5
  signals$sentiment <- mean(c(signals$vix_calm, signals$breadth_bull, credit_health), na.rm = TRUE)

  signals
}

# ── Regime Definitions ────────────────────────────────────────────────────────
# Weights define how each signal contributes to a regime's score.
# Positive = supports the regime. Negative = contradicts it.
# EDIT THESE to tune regime detection. Backtest to validate.

REGIME_WEIGHTS <- list(
  liquidity_stress = list(
    label   = "Liquidity Stress",
    tooltip = "Forced selling, correlation converging to 1. Sell what you can, not what you want. DO NOT open new trades.",
    weights = list(
      vix_stress    = 0.35,   # was 0.25 — core stress signal, highest weight
      backwardation = 0.05,   # was 0.25 — mild backwardation is common, low weight
      breadth_bear  = 0.25,   # was 0.20
      credit_stress = 0.25,   # was 0.20
      vix_calm      = -0.10,  # was -0.20 — reduce negative drag
      sentiment     = -0.10   # was -0.15
    )
  ),
  directional_flow = list(
    label   = "Directional Flow",
    tooltip = "Capital moving into identifiable sector groups. Sectors with RS + volume + breadth momentum are attracting flow. This is where swing trades live.",
    weights = list(
      breadth_bull  = 0.25,   # was 0.20
      vix_calm      = 0.20,   # was 0.15
      copper_gold   = 0.15,
      sentiment     = 0.20,
      credit_stress = -0.20,  # was -0.15
      vix_stress    = -0.15,
      backwardation = -0.05   # was -0.10
    )
  ),
  neutral = list(
    label   = "Neutral",
    tooltip = "No clear direction. Mixed signals, low conviction. Rely on stock-level technicals or wait.",
    weights = list()  # computed as residual
  )
)

#' Compute regime raw scores from signals
#' @param signals Named list from compute_signals()
#' @return Named numeric vector of raw scores
compute_regime_scores <- function(signals) {
  scores <- list()
  for (regime in names(REGIME_WEIGHTS)) {
    if (regime == "neutral") next
    w <- REGIME_WEIGHTS[[regime]]$weights
    score <- 0
    for (sig_name in names(w)) {
      sig_val <- signals[[sig_name]]
      if (!is.null(sig_val) && !is.na(sig_val)) {
        score <- score + w[[sig_name]] * sig_val
      }
    }
    scores[[regime]] <- score
  }

  # Neutral = residual: high when no other regime is strong
  max_other <- max(unlist(scores), na.rm = TRUE)
  breadth_mid <- if (!is.na(signals$breadth_bull))
    1 - abs(signals$breadth_bull - 0.5) / 0.5 else 0.5  # peaks at S5FI=50
  scores[["neutral"]] <- 0.5 * (1 - max_other / 0.5) + 0.3 * signals$vix_calm + 0.2 * breadth_mid

  scores
}

# ── Positioning Stress ────────────────────────────────────────────────────────

#' Compute positioning stress from COT data and sector RS extremes
#' @param sector_ok Named list of sector gate results (from swing_scanner)
#' @param cot_data List from positioning.R COT_POSITIONING
#' @return Named list: crowding_score (0-1), extreme_sectors, cot_extremes
compute_positioning_stress <- function(sector_ok = NULL, cot_data = NULL) {
  crowding <- 0
  cot_extremes <- character(0)

  # Sector RS extremes (if available)
  if (!is.null(sector_ok)) {
    rs_values <- vapply(sector_ok, function(s) {
      if (!is.na(s$rs)) abs(s$rs) else 0
    }, numeric(1))
    n_extreme <- sum(rs_values > 8, na.rm = TRUE)
    crowding <- crowding + n_extreme / max(length(rs_values), 1) * 0.5
  }

  # COT extremes
  if (!is.null(cot_data) && length(cot_data) > 0) {
    n_extreme_cot <- sum(vapply(cot_data, function(c) isTRUE(c$extreme), logical(1)))
    crowding <- crowding + n_extreme_cot / length(cot_data) * 0.5
    cot_extremes <- vapply(
      Filter(function(c) isTRUE(c$extreme), cot_data),
      function(c) c$asset, character(1))
  }

  list(crowding_score = min(crowding, 1), cot_extremes = cot_extremes)
}

# ── Catalyst Proximity ────────────────────────────────────────────────────────

#' Compute catalyst boost from upcoming events
#' @param events List from events.R
#' @param today Date
#' @return Numeric boost (0 to 0.15)
compute_catalyst_boost <- function(events, today = Sys.Date()) {
  if (is.null(events) || length(events) == 0) return(0)
  max_boost <- 0
  year <- format(today, "%Y")
  for (ev in events) {
    ev_date <- tryCatch(as.Date(paste0(ev$date, "-", year), format = "%b-%d-%Y"),
                         error = function(e) NA)
    if (is.na(ev_date)) next
    days_until <- as.integer(ev_date - today)
    if (days_until < 0 || days_until > 5) next
    boost <- switch(ev$impact,
      CRITICAL = if (days_until <= 2) 0.15 else 0.08,
      HIGH     = if (days_until <= 2) 0.08 else 0.04,
      MODERATE = if (days_until <= 1) 0.04 else 0,
      0)
    max_boost <- max(max_boost, boost)
  }
  max_boost
}

# ── Macro Outcome Modifier ────────────────────────────────────────────────────

#' Get recent macro surprise modifier
#' @param outcomes List from macro_outcomes.R
#' @return Named list with direction and decay factor
get_outcome_modifier <- function(outcomes) {
  if (is.null(outcomes) || length(outcomes) == 0) return(NULL)
  today <- Sys.Date()
  recent <- Filter(function(o) {
    d <- tryCatch(as.Date(o$date), error = function(e) NA)
    !is.na(d) && as.integer(today - d) <= 30
  }, outcomes)
  if (length(recent) == 0) return(NULL)
  # Most recent outcome
  recent[[length(recent)]]
}

# ── Softmax with Temperature ─────────────────────────────────────────────────

#' Softmax normalization with temperature parameter
#' @param scores Named numeric vector
#' @param temperature Higher = flatter distribution (less certainty)
#' @return Named numeric vector of probabilities (sum to 1)
softmax <- function(scores, temperature = 2) {
  vals <- unlist(scores)
  exp_vals <- exp(vals / temperature)
  probs <- exp_vals / sum(exp_vals)
  setNames(as.numeric(probs), names(scores))
}

# ── Sector Flow Scoring ──────────────────────────────────────────────────────
# Per-regime sector flow expectations: [-1, +1]
# Positive = sector attracts capital. Negative = capital leaves.
# Neutral regime = all zeros (no macro wind, pure technicals).
# EDIT THESE based on backtest results.

REGIME_SECTOR_FLOWS <- list(
  liquidity_stress = list(
    # Everything sells, PreciousMetals may catch safe-haven bid
    Technology = -0.7, Financials = -0.6, Industrials = -0.5,
    Materials = -0.5, Energy = -0.4, Agriculture = -0.3,
    ConsumerStaples = +0.1, Healthcare = 0, Defence = +0.2,
    PreciousMetals = +0.3
  ),
  directional_flow = list(
    # Flows determined by actual RS/volume data, not static mapping.
    # These are structural tilts when flow is active.
    # DXY weak = Industrials/Materials/EM-exposed benefit
    # Rates stable = Tech/Growth benefit
    # Set to near-zero; actual flow detection happens in the sector scoring.
    Technology = 0, Financials = 0, Industrials = 0,
    Materials = 0, Energy = 0, Agriculture = 0,
    ConsumerStaples = 0, Healthcare = 0, Defence = 0,
    PreciousMetals = 0
  ),
  neutral = list(
    Technology = 0, Financials = 0, Industrials = 0,
    Materials = 0, Energy = 0, Agriculture = 0,
    ConsumerStaples = 0, Healthcare = 0, Defence = 0,
    PreciousMetals = 0
  )
)

# ── DXY / Yield Curve Sector Modifiers ────────────────────────────────────────
# Applied on top of regime flows when directional_flow is dominant.
# These are mechanical relationships, not opinions.

DXY_SECTOR_IMPACT <- list(
  # Strong DXY = headwind multinationals, tailwind domestics
  strong = list(Technology = -0.3, Industrials = -0.2, Materials = -0.2,
                Healthcare = +0.1, ConsumerStaples = +0.1),
  # Weak DXY = tailwind exporters/commodities
  weak = list(Technology = +0.1, Industrials = +0.2, Materials = +0.3,
              Energy = +0.2, PreciousMetals = +0.2)
)

CURVE_SECTOR_IMPACT <- list(
  # Steepening = banks earn more spread
  steepening = list(Financials = +0.3),
  # Flattening = bank margins compress
  flattening = list(Financials = -0.2)
)

# ── DB Operations ─────────────────────────────────────────────────────────────

load_prev_dominant <- function(conn) {
  prev <- tryCatch(
    dbGetQuery(conn, "SELECT regime FROM macro_regimes WHERE is_dominant = 1
                      AND cache_date < ? ORDER BY cache_date DESC LIMIT 1",
               params = list(as.character(Sys.Date()))),
    error = function(e) NULL
  )
  if (!is.null(prev) && nrow(prev) > 0) prev$regime[1] else NULL
}

load_prev_probabilities <- function(conn) {
  prev <- tryCatch(
    dbGetQuery(conn, "SELECT regime, probability FROM macro_regimes
                      WHERE cache_date = (SELECT MAX(cache_date) FROM macro_regimes WHERE cache_date < ?)",
               params = list(as.character(Sys.Date()))),
    error = function(e) NULL
  )
  if (!is.null(prev) && nrow(prev) > 0) setNames(prev$probability, prev$regime) else NULL
}

save_regimes <- function(conn, results) {
  .today <- as.character(Sys.Date())
  export <- data.frame(
    cache_date      = .today,
    regime          = results$regime,
    regime_label    = results$regime_label,
    probability     = results$probability,
    raw_score       = results$raw_score,
    is_dominant     = as.integer(results$is_dominant),
    catalyst_boost  = results$catalyst_boost,
    positioning_adj = results$positioning_adj,
    details         = results$details,
    stringsAsFactors = FALSE
  )
  tryCatch(dbExecute(conn, "DELETE FROM macro_regimes WHERE cache_date = ?",
                     params = list(.today)), error = function(e) NULL)
  dbWriteTable(conn, "macro_regimes", export, append = TRUE)
}

# ── Orchestrator ──────────────────────────────────────────────────────────────

#' Main entry: evaluate regimes with all modifiers
#' @param raw Raw price data (for new macro tickers)
#' @param vix_res VIX analysis results
#' @param rates_res Rates analysis results
#' @param breadth Breadth results
#' @param comm_res Commodities analysis results
#' @param events Events list from events.R
#' @param conn DBI connection
#' @return data.frame with regime probabilities and metadata
run_scenarios <- function(raw, vix_res, rates_res, breadth, comm_res, events, conn) {

  # 1. Compute continuous signals
  signals <- compute_signals(vix_res, rates_res, breadth, comm_res, raw)

  # 2. Compute raw regime scores
  raw_scores <- compute_regime_scores(signals)

  # 3. Positioning stress from COT
  cot_data <- tryCatch(COT_POSITIONING, error = function(e) list())
  pos_stress <- compute_positioning_stress(cot_data = cot_data)

  # Boost liquidation/neutral when positioning is crowded
  if (pos_stress$crowding_score > 0.3) {
    raw_scores[["liquidity_stress"]] <- raw_scores[["liquidity_stress"]] + 0.10 * pos_stress$crowding_score
  }

  # 4. Catalyst proximity
  catalyst_boost <- compute_catalyst_boost(events)
  # Boost the challenger (second-highest regime)
  sorted_regimes <- names(sort(unlist(raw_scores), decreasing = TRUE))
  challenger <- if (length(sorted_regimes) >= 2) sorted_regimes[2] else NULL
  if (!is.null(challenger) && catalyst_boost > 0) {
    raw_scores[[challenger]] <- raw_scores[[challenger]] + catalyst_boost
  }

  # 5. Regime inertia
  prev_dominant <- load_prev_dominant(conn)
  prev_probs <- load_prev_probabilities(conn)
  inertia_bonus <- 0.15
  if (!is.null(prev_dominant) && prev_dominant %in% names(raw_scores)) {
    raw_scores[[prev_dominant]] <- raw_scores[[prev_dominant]] + inertia_bonus
  }

  # 6. Softmax → probabilities
  probs <- softmax(raw_scores, temperature = 0.8)  # was 2 — lower = more decisive

  # 7. Build results data.frame
  results <- data.frame(
    regime          = names(probs),
    regime_label    = vapply(names(probs), function(r) REGIME_WEIGHTS[[r]]$label, character(1)),
    probability     = round(as.numeric(probs) * 100, 1),
    raw_score       = round(vapply(names(probs), function(r) raw_scores[[r]], numeric(1)), 3),
    is_dominant     = as.numeric(probs) == max(as.numeric(probs)),
    catalyst_boost  = vapply(names(probs), function(r) {
      if (!is.null(challenger) && r == challenger) catalyst_boost else 0
    }, numeric(1)),
    positioning_adj = vapply(names(probs), function(r) {
      if (r == "liquidity_stress" && pos_stress$crowding_score > 0.3)
        round(0.10 * pos_stress$crowding_score, 3) else 0
    }, numeric(1)),
    details = vapply(names(probs), function(r) {
      # Signal breakdown
      w <- REGIME_WEIGHTS[[r]]$weights
      if (length(w) == 0) return("Residual: high when other regimes are weak")
      parts <- vapply(names(w), function(s) {
        v <- signals[[s]]; if (is.null(v) || is.na(v)) v <- 0
        sprintf("%s:%.2f*%.2f=%.2f", s, w[[s]], v, w[[s]] * v)
      }, character(1))
      paste(parts, collapse = " | ")
    }, character(1)),
    stringsAsFactors = FALSE
  )

  # Delta vs previous run
  if (!is.null(prev_probs)) {
    results$delta <- vapply(results$regime, function(r) {
      prev <- prev_probs[r]
      if (is.na(prev)) 0 else round(results$probability[results$regime == r] - prev, 1)
    }, numeric(1))
  } else {
    results$delta <- rep(NA_real_, nrow(results))
  }

  # Save to DB
  save_regimes(conn, results)

  dominant <- results$regime_label[results$is_dominant][1]
  dom_prob <- results$probability[results$is_dominant][1]
  message(sprintf("Regimes: dominant = %s (%.1f%%) | Catalyst boost: %.2f | Positioning stress: %.2f",
    dominant, dom_prob, catalyst_boost, pos_stress$crowding_score))
  if (length(pos_stress$cot_extremes) > 0)
    message(sprintf("  COT extremes: %s", paste(pos_stress$cot_extremes, collapse = ", ")))

  # Attach signals and stress for use by render
  attr(results, "signals") <- signals
  attr(results, "positioning") <- pos_stress
  attr(results, "signal_tooltips") <- SIGNAL_TOOLTIPS

  results
}

#' Get sector flow map for final_filter.R
#' Returns the static regime flows + DXY/curve modifiers
get_scenario_sector_map <- function() {
  REGIME_SECTOR_FLOWS
}

#' Get DXY sector impact for flow modifier
get_dxy_impact <- function() { DXY_SECTOR_IMPACT }

#' Get yield curve sector impact for flow modifier
get_curve_impact <- function() { CURVE_SECTOR_IMPACT }
