# pull_score.R — Phase B: Pull screen (Steps B.1 – B.4)
#
# All indicators reused from indicators.R — no new technical computation.

#' Score a stock for the Pull screen. Combines stage classification (B.1),
#' sector flow context (B.2), and footprint confirmation (B.3) into the 0–10
#' Pull_Score. Survival to Phase C requires Pull_Score >= 6 AND
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
#' @return list with stage, pull_score, pull_direction, sub-component points
score_pull <- function(last, price, etf_ret,
                       sector_rs_rank = NA, n_long_sectors = 0,
                       sector_long_gate = FALSE, sector_short_gate = FALSE,
                       trend_passes = NA, rs_3m = NA) {

  rs_etf <- round(last$ret20 - etf_ret, 1)

  # ── B.1 Stage classification (4 pts, mutually exclusive) ───────────────────
  # Priority: extended > early > continuation > none. Extended (price beyond
  # MA50*1.15) is a demotion: even if breakout signals fire, the trade is
  # "late, headroom collapses fast" (per spec D.2 design). Don't be tricked
  # by a strong-looking signal on an exhausted move.
  bot_obj <- score_breakout(last, price, etf_ret)  # reuses scoring.R
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
  pull_direction <- if (!is.na(last$ma50) && price > last$ma50 && (sector_long_gate || sector_rs_rank <= 3))
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
    ((pull_direction == "up" && last$obv_slope > 0) ||
     (pull_direction == "down" && last$obv_slope < 0))
  updn_aligned <- !is.na(last$updn_ratio) &&
    ((pull_direction == "up" && last$updn_ratio > 1.1) ||
     (pull_direction == "down" && last$updn_ratio < 0.9))
  rs3m_aligned <- !is.na(rs_3m) &&
    ((pull_direction == "up" && rs_3m > 0) ||
     (pull_direction == "down" && rs_3m < 0))
  footprint_pts <- as.integer(sum(c(obv_aligned, updn_aligned, rs3m_aligned)))

  # ── B.4 Aggregate and cutoff ──────────────────────────────────────────────
  pull_score <- max(0L, stage_pts + sector_pts + footprint_pts)
  passes_phase_b <- pull_score >= 6 &&
                    stage %in% c("early", "continuation") ||
                    (stage == "extended" && pull_score >= 8)

  list(
    pull_score    = as.integer(pull_score),
    stage         = stage,
    pull_direction = pull_direction,
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
