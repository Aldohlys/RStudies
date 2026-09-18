# reports/shared/gates.R — the single implementation of the nine BOT gates.
#
# docs/BOT_TOOLS_DESIGN.md sections 3.9 and 3.10. The nine gates previously
# existed in three places (indicators.R::compute_breakdown, swing_scanner/
# flow_score.R::score_breakout, bot_scan_universe.py::score_one) which had
# already drifted: the first computes five setup gates, the second six,
# because they disagree on whether S3 exists.
#
# This file is the one callable source. It consumes calc_ind()'s output rather
# than recomputing anything, so the gate inputs cannot drift from indicators.R.
#
# S3 is computed but is NOT one of the nine: it needs a benchmark return, is
# reported alongside the clusters rather than inside one, and #82's
# dimensionality work was done on the nine without it.

#' Extract the gate-input quantities from a calc_ind() row.
#'
#' @param last single-row data.frame from get_last()
#' @param rs20 numeric — ret20 minus the benchmark ETF's ret20, in percentage
#'   points. NA when no benchmark is available; S3 then abstains.
#' @return named list of the section 3.9 quantities
gate_inputs <- function(last, rs20 = NA_real_) {
  if (is.null(last) || !nrow(last)) return(NULL)
  ll <- as.list(last[1, , drop = FALSE])
  num <- function(x) if (is.null(x) || length(x) != 1) NA_real_ else as.numeric(x)
  list(
    d_squeeze      = num(ll$squeeze_ratio),
    d_vol_decline  = num(ll$vol_decline),
    d_vol_surge    = num(ll$vol_surge),
    obv_slope      = num(ll$obv_slope),
    obv_slope_days = if (!is.na(num(ll$vol_ma20)) && num(ll$vol_ma20) > 0)
                       num(ll$obv_slope) / num(ll$vol_ma20) else NA_real_,
    rsi14          = num(ll$rsi14),
    rsi_slope      = num(ll$rsi_slope),
    updn_ratio     = num(ll$updn_ratio),
    ret20          = num(ll$ret20),
    rs20           = num(rs20),
    adx10          = num(ll$adx10),
    rng_pct_20     = num(ll$rng_pct),
    ema50          = num(ll$ma50),
    ema50_slope    = num(ll$ma50_slope))
}

#' Evaluate the nine gates (plus S3) in one direction.
#'
#' A NULL or NA quantity fails its gate; no gate passes on missing data.
#'
#' @param gi list from gate_inputs()
#' @param price numeric spot
#' @param direction "long" or "short"
#' @return named logical vector S1 S2 S4 S5 S6 BK1 BK2 BK3 BK4 S3
eval_gates <- function(gi, price, direction = "long") {
  ok <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)
  long <- identical(direction, "long")

  S1 <- ok(gi$ema50) && ok(price) &&
        (if (long) price > gi$ema50 else price < gi$ema50)
  S2 <- ok(gi$ema50_slope) &&
        (if (long) gi$ema50_slope > 0 else gi$ema50_slope < 0)
  S4 <- ok(gi$obv_slope) &&
        (if (long) gi$obv_slope > 0 else gi$obv_slope < 0)
  S5 <- ok(gi$d_squeeze)     && gi$d_squeeze     < 0.65   # direction-neutral
  S6 <- ok(gi$d_vol_decline) && gi$d_vol_decline < 0.95   # direction-neutral

  BK1 <- ok(gi$rsi14) && ok(gi$rsi_slope) &&
         (if (long) (gi$rsi14 > 50 && gi$rsi_slope > 0)
          else      (gi$rsi14 < 50 && gi$rsi_slope < 0))
  BK2 <- ok(gi$updn_ratio) &&
         (if (long) gi$updn_ratio > 1.1 else gi$updn_ratio < 0.9)
  BK3 <- ok(gi$rng_pct_20) &&
         (if (long) gi$rng_pct_20 >= 70 else gi$rng_pct_20 <= 30)
  BK4 <- ok(gi$d_vol_surge) && gi$d_vol_surge >= 1.2      # direction-neutral

  S3  <- ok(gi$rs20) && (if (long) gi$rs20 > 0 else gi$rs20 < 0)

  c(S1 = S1, S2 = S2, S4 = S4, S5 = S5, S6 = S6,
    BK1 = BK1, BK2 = BK2, BK3 = BK3, BK4 = BK4, S3 = S3)
}

#' Cluster sub-totals, never one flat count.
#'
#' The nine gates carry ~5 effective dimensions in three clusters weighted
#' 6 : 1 : 2, so a flat total overstates confirmation.
#'
#' @param g named logical vector from eval_gates()
#' @return list(trend_state, compression_state, supply_state, rs_state) as the
#'   display strings of section 3.10
cluster_states <- function(g) {
  cnt <- function(ids) sum(vapply(ids, function(i) isTRUE(g[[i]]), logical(1)))
  list(
    trend_state       = sprintf("%d/6", cnt(c("S1", "S2", "S4", "BK1", "BK2", "BK3"))),
    compression_state = sprintf("%d/1", cnt("S5")),
    supply_state      = sprintf("%d/2", cnt(c("S6", "BK4"))),
    rs_state          = if (isTRUE(g[["S3"]])) "+" else "-")
}

#' Weekly-vs-daily agreement on the compression and supply clusters.
#'
#' Reported as a match count "k/3" over S5, S6 and BK4, in the same shape as the
#' cluster states, or "n/a" when weekly bars are unavailable.
#'
#' A count rather than agree/conflicted/unconfirmed, because these three gates
#' are ONE-SIDED: each fires or stays silent, so a timeframe can never state the
#' opposite of the other, only fail to match. Claiming "conflicted" would assert
#' something the booleans cannot express. Counting is also what #82's S-7
#' measured -- phi between the daily and weekly forms of the same gate, with the
#' timeframes differing on ~52% of observations.
#'
#' Trend is deliberately excluded: S-7 found weekly trend gates already
#' correlate 0.39-0.49 with their daily twins, while these three are independent
#' across timeframes (phi -0.061 / -0.064 / +0.009).
#'
#' @param gd,gw named logical vectors from eval_gates() on daily and weekly bars
#' @return "3/3" .. "0/3", or "n/a"
confluence_state <- function(gd, gw) {
  if (is.null(gw)) return("n/a")
  ids <- c("S5", "S6", "BK4")
  d <- vapply(ids, function(i) isTRUE(gd[[i]]), logical(1))
  w <- vapply(ids, function(i) isTRUE(gw[[i]]), logical(1))
  sprintf("%d/3", sum(d == w))
}
