# flow_score.R — Phase B: Flow screen (Steps B.1 – B.4)
#
# Flow_Score is NOT the raw technical-breakout (BOT) score. It is a higher-level
# flow/context composite: it consumes the BOT setup/breakout counts only through
# the stage bucket (B.1), then layers sector-rotation context (B.2) and money
# footprint (B.3) on top — i.e. "is this stock being pulled along by flow at a
# tradeable point in its life", not "are the technicals breaking out right now".
# The raw BOT synthesis lives in score_breakout() and is surfaced as
# bot_setup / bot_breakout.
#
# All indicators reused from indicators.R — no new technical computation.

#' Score a stock for breakout (BOT) setup
#'
#' 10 criteria split into 2 phases:
#'   SETUP (6): conditions that build the base over time
#'     S1: Price > MA50 (uptrend established)
#'     S2: MA50 slope > 0 (trend confirmed)
#'     S3: RS > 0 vs sector ETF (relative strength built up)
#'     S4: OBV slope > 0 (accumulation happening)
#'     S5: Squeeze < 0.65 (range contracting)
#'     S6: Vol decline < 0.95 (supply drying up)
#'   BREAKOUT (4): conditions that confirm the breakout NOW
#'     BK1: RSI > 50 & slope > 0 (momentum accelerating)
#'     BK2: Up/down ratio > 1.1 (buying pressure)
#'     BK3: Range position >= 70% (pushing against resistance)
#'     BK4: Vol surge >= 1.2x (volume confirmation today)
#'
#' @param last Single-row data.frame with indicators
#' @param price Current price
#' @param etf_ret ETF 20d return for relative strength
#' @return list with setup_score, breakout_score, total score, flags
score_breakout <- function(last, price, etf_ret) {
  rs <- round(last$ret20 - etf_ret, 1)

  # ── SETUP phase (6 criteria) ───────────────────────────────────────────
  S1 <- !is.na(last$ma50)          && price > last$ma50
  S2 <- !is.na(last$ma50_slope)    && last$ma50_slope > 0
  S3 <- rs > 0
  S4 <- !is.na(last$obv_slope)     && last$obv_slope > 0
  S5 <- !is.na(last$squeeze_ratio) && last$squeeze_ratio < 0.65
  S6 <- !is.na(last$vol_decline)   && last$vol_decline < 0.95

  # ── BREAKOUT phase (4 criteria) ────────────────────────────────────────
  BK1 <- !is.na(last$rsi14)      && last$rsi14 > 50 &&
         !is.na(last$rsi_slope)  && last$rsi_slope > 0
  BK2 <- !is.na(last$updn_ratio) && last$updn_ratio > 1.1
  BK3 <- !is.na(last$rng_pct)    && last$rng_pct >= 70
  BK4 <- !is.na(last$vol_surge)  && last$vol_surge >= 1.2

  setup_score    <- sum(c(S1, S2, S3, S4, S5, S6))
  breakout_score <- sum(c(BK1, BK2, BK3, BK4))

  list(
    score = setup_score + breakout_score,
    setup = setup_score,
    breakout = breakout_score,
    rs = rs,
    squeeze = if (!is.na(last$squeeze_ratio)) round(last$squeeze_ratio, 3) else NA,
    vol_dec = if (!is.na(last$vol_decline))   round(last$vol_decline, 3)   else NA,
    vol_surge = if (!is.na(last$vol_surge))   round(last$vol_surge, 2)     else NA,
    flags = paste0(
      "S:", setup_score, "/6",
      " BK:", breakout_score, "/4",
      " | S1:", ifelse(S1,"+","-"), " S2:", ifelse(S2,"+","-"),
      " S3:", ifelse(S3,"+","-"), " S4:", ifelse(S4,"+","-"),
      " S5:", ifelse(S5,"+","-"), " S6:", ifelse(S6,"+","-"),
      " | BK1:", ifelse(BK1,"+","-"), " BK2:", ifelse(BK2,"+","-"),
      " BK3:", ifelse(BK3,"+","-"), " BK4:", ifelse(BK4,"+","-"))
  )
}

#' Score a stock for the Flow screen. Combines stage classification (B.1),
#' sector flow context (B.2), and footprint confirmation (B.3) into the 0–10
#' Flow_Score. Survival to Phase C requires Flow_Score >= 6 AND
#' Stage in {early, continuation}.
#'
#' @param last        single-row indicators data.frame
#' @param price       current close
#' @param etf_ret     sector ETF 20d return for RS calc
#' @param sector_rs_rank integer or NA — rank among LONG-passing sectors (1..N)
#' @param n_long_sectors integer — number of LONG-passing sectors (for top-3 cutoff)
#' @param sector_long_gate logical — sector binary gate passes long
#' @param sector_short_gate logical — sector binary gate passes short
#' @param trend_passes logical — Tdata::isTrendContinuation passes
#' @param rs_3m       3-month RS vs benchmark (from isTrendContinuation)
#' @return list with stage, flow_score, flow_direction, sub-component points
score_flow <- function(last, price, etf_ret,
                       sector_rs_rank = NA, n_long_sectors = 0,
                       sector_long_gate = FALSE, sector_short_gate = FALSE,
                       trend_passes = NA, rs_3m = NA) {

  rs_etf <- round(last$ret20 - etf_ret, 1)

  # ── B.1 Stage classification (4 pts, mutually exclusive) ───────────────────
  # Priority: extended > early > continuation > none. Extended (price beyond
  # MA50*1.15) is a demotion: even if breakout signals fire, the trade is
  # "late, headroom collapses fast" (per spec D.2 design). Don't be tricked
  # by a strong-looking signal on an exhausted move.
  bot_obj <- score_breakout(last, price, etf_ret)
  is_extended       <- !is.na(last$ma50) && last$ma50 > 0 &&
                      price > last$ma50 * 1.15
  is_early_breakout <- !is_extended &&
                      !is.na(bot_obj$setup) && !is.na(bot_obj$breakout) &&
                      bot_obj$setup >= 5 && bot_obj$breakout >= 3
  is_continuation   <- !is_extended && !is_early_breakout && isTRUE(trend_passes)

  stage <- if (is_extended)         "extended"
           else if (is_early_breakout) "early"
           else if (is_continuation)   "continuation"
           else                        "none"
  stage_pts <- switch(stage,
    "early" = 4L, "continuation" = 3L, "extended" = 1L, 0L)

  # Direction inferred from price vs MA50 + sector gate
  flow_direction <- if (!is.na(last$ma50) && price > last$ma50 && (sector_long_gate || sector_rs_rank <= 3))
                      "up"
                    else if (!is.na(last$ma50) && price < last$ma50 && sector_short_gate)
                      "down"
                    else "neutral"

  # ── B.2 Sector flow context (3 pts) ────────────────────────────────────────
  sector_pts <- if (sector_long_gate || sector_short_gate) {
    if (!is.na(sector_rs_rank)) {
      if (sector_rs_rank <= 3) 3L
      else if (sector_rs_rank <= 6) 2L
      else 0L
    } else 0L
  } else if (!sector_long_gate && !sector_short_gate) {
    -2L  # counter-trend stock, both gates closed
  } else 0L

  # ── B.3 Footprint confirmation (3 pts) ────────────────────────────────────
  obv_aligned <- !is.na(last$obv_slope) &&
    ((flow_direction == "up" && last$obv_slope > 0) ||
     (flow_direction == "down" && last$obv_slope < 0))
  updn_aligned <- !is.na(last$updn_ratio) &&
    ((flow_direction == "up" && last$updn_ratio > 1.1) ||
     (flow_direction == "down" && last$updn_ratio < 0.9))
  rs3m_aligned <- !is.na(rs_3m) &&
    ((flow_direction == "up" && rs_3m > 0) ||
     (flow_direction == "down" && rs_3m < 0))
  footprint_pts <- as.integer(sum(c(obv_aligned, updn_aligned, rs3m_aligned)))

  # ── B.4 Aggregate and cutoff ──────────────────────────────────────────────
  flow_score <- max(0L, stage_pts + sector_pts + footprint_pts)
  passes_phase_b <- flow_score >= 6 &&
                    stage %in% c("early", "continuation") ||
                    (stage == "extended" && flow_score >= 8)

  list(
    flow_score    = as.integer(flow_score),
    stage         = stage,
    flow_direction = flow_direction,
    stage_pts     = stage_pts,
    sector_pts    = sector_pts,
    footprint_pts = footprint_pts,
    sector_rs_rank = sector_rs_rank,
    rs_etf        = rs_etf,
    bot_setup     = bot_obj$setup,
    bot_breakout  = bot_obj$breakout,
    passes        = passes_phase_b,
    flags = paste0(
      "Stage:", stage, "(", stage_pts, ")",
      " Sec:", sector_pts,
      " Foot:", footprint_pts, "/3",
      " | OBV:", ifelse(obv_aligned, "+", "-"),
      " UpDn:", ifelse(updn_aligned, "+", "-"),
      " RS3m:", ifelse(rs3m_aligned, "+", "-"))
  )
}
