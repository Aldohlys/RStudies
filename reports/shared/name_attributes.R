# reports/shared/name_attributes.R — the slow, name-level quantities BOT_monthly
# writes to Tickers (docs/BOT_TOOLS_DESIGN.md section 1).
#
# These are ticker attributes, not signals: atr_pct has ICC 0.646, and gap share
# and vol-of-vol have rank persistence +0.494 / +0.445 across disjoint 504-day
# blocks. Recomputing them daily is arithmetic on data that has not changed.
#
# gap_share() is specified to live in Tdata/R/vol_metrics.R. It is here for now
# because moving it means a package build and deployment to eight install
# locations; the formula is self-contained and moves unchanged.

#' Overnight share of variance.
#'
#' `sum(r_on^2) / sum(r_cc^2)` over the trailing window, with
#' `r_on = log(Open_t / Close_{t-1})` and `r_cc = log(Close_t / Close_{t-1})`.
#' High values mean the move happens between sessions: price gaps THROUGH a
#' stop rather than trading through it, so ATR-based sizing understates the
#' loss tail.
#'
#' @param d data.frame with date, Open, Close (chronological)
#' @param lookback integer sessions, default 504
#' @return numeric 0-1, or NA when there is too little data
gap_share <- function(d, lookback = 504) {
  if (is.null(d) || nrow(d) < 60) return(NA_real_)
  d <- d[order(d$date), , drop = FALSE]
  d <- utils::tail(d, lookback + 1)
  o <- d$Open; c <- d$Close
  n <- length(c)
  if (n < 60 || !"Open" %in% names(d)) return(NA_real_)
  r_on <- log(o[-1] / c[-n])
  r_cc <- log(c[-1] / c[-n])
  ok <- is.finite(r_on) & is.finite(r_cc)
  if (sum(ok) < 60) return(NA_real_)
  den <- sum(r_cc[ok]^2)
  if (!is.finite(den) || den <= 0) return(NA_real_)
  sum(r_on[ok]^2) / den
}

#' ATR percentiles over a long window, and the membership band.
#'
#' @param d data.frame with High, Low, Close (chronological), >= 5 years ideally
#' @return list(atr_pct, med5y, p25, p75, band)
atr_profile <- function(d) {
  if (is.null(d) || nrow(d) < 130) return(NULL)
  d <- d[order(d$date), , drop = FALSE]
  atr <- as.numeric(TTR::ATR(cbind(d$High, d$Low, d$Close), n = 14)[, "atr"])
  ap <- atr / d$Close * 100
  ap <- ap[is.finite(ap)]
  if (!length(ap)) return(NULL)
  med <- stats::median(ap)
  list(atr_pct = utils::tail(ap, 1),
       med5y = med,
       p25 = as.numeric(stats::quantile(ap, 0.25)),
       p75 = as.numeric(stats::quantile(ap, 0.75)),
       band = if (med < 1.7) "low" else if (med < 3.0) "mid" else "high")
}

#' Average daily traded value in CHF millions over 20 sessions.
#'
#' @param d data.frame with Close, Volume
#' @param currency ISO code of the listing currency
#' @param fx numeric units of CHF per unit of `currency`; 1 for CHF
#' @return numeric CHF millions per day, or NA
adv_chf_m <- function(d, currency = "USD", fx = NA_real_) {
  if (is.null(d) || nrow(d) < 20) return(NA_real_)
  d <- utils::tail(d[order(d$date), , drop = FALSE], 20)
  v <- mean(d$Close * d$Volume, na.rm = TRUE)
  if (!is.finite(v)) return(NA_real_)
  rate <- if (identical(currency, "CHF")) 1 else fx
  if (!is.finite(rate)) return(NA_real_)
  v * rate / 1e6
}

#' Model ATM call cost and the breakeven distances it implies.
#'
#' Brenner-Subrahmanyam closed form rather than a Black-Scholes solve:
#' `C_share = 0.3989 * px * sigma * sqrt(T)`, the constant being `1/sqrt(2*pi)`.
#' This is a feasibility question, not a pricing one -- realised BOT P&L is
#' driven by delta (Spearman +0.797), not vega (-0.146), so an approximate
#' sigma still answers "can the premium be covered".
#'
#' @param px numeric spot
#' @param sigma numeric annualised volatility as a decimal
#' @param atr numeric ATR(14) in price
#' @param em_hi_abs numeric upper edge of the 10-session expected move, in price
#' @param dte integer days to expiry, default 30
#' @param multiplier numeric contract multiplier, default 100
#' @return list(call_cost, breakeven_pct, breakeven_atr, breakeven_pct_em10)
model_call_breakeven <- function(px, sigma, atr, em_hi_abs, dte = 30, multiplier = 100) {
  if (!is.finite(px) || !is.finite(sigma) || sigma <= 0)
    return(list(call_cost = NA_real_, breakeven_pct = NA_real_,
                breakeven_atr = NA_real_, breakeven_pct_em10 = NA_real_))
  tt <- dte / 365
  c_share <- 0.3989 * px * sigma * sqrt(tt)
  list(
    call_cost          = c_share * multiplier,
    breakeven_pct      = c_share / px * 100,
    breakeven_atr      = if (is.finite(atr) && atr > 0) c_share / atr else NA_real_,
    breakeven_pct_em10 = if (is.finite(em_hi_abs) && em_hi_abs > 0)
                           c_share / em_hi_abs * 100 else NA_real_)
}

#' Annualised close-to-close realised volatility over `n` sessions.
#'
#' @param d data.frame with Close
#' @param n integer sessions
#' @return numeric decimal (0.32 = 32%), or NA
hist_vol <- function(d, n = 20) {
  if (is.null(d) || nrow(d) < n + 2) return(NA_real_)
  c <- utils::tail(d$Close[!is.na(d$Close)], n + 1)
  if (length(c) < n + 1) return(NA_real_)
  r <- diff(log(c))
  if (!length(r)) return(NA_real_)
  stats::sd(r) * sqrt(252)
}

#' Count sessions on which the BOT gate set fired, over a trailing window.
#'
#' Runs the real gate function over history rather than a reimplementation, so
#' the count cannot describe gates that no longer exist. `gate_version` is
#' stored beside the count to make a stale count detectable.
#'
#' Requires gates.R and indicators.R to be sourced.
#'
#' @param d data.frame already through calc_ind()
#' @param lookback integer sessions, default 504
#' @param direction "long" or "short"
#' @param min_trend integer -- a session counts when at least this many of the
#'   six trend-cluster gates fire AND the compression or supply cluster fires.
#' @return list(n, last_date)
count_opportunities <- function(d, lookback = 504, direction = "long", min_trend = 4L) {
  if (is.null(d) || nrow(d) < 150) return(list(n = NA_integer_, last_date = NA_character_))
  d <- d[order(d$date), , drop = FALSE]
  rows <- utils::tail(seq_len(nrow(d)), lookback)
  n <- 0L; last <- NA_character_
  for (i in rows) {
    r <- d[i, , drop = FALSE]
    if (is.na(r$adx10) || is.na(r$ma50) || is.na(r$rsi14) ||
        is.na(r$obv_slope) || is.na(r$updn_ratio)) next
    g <- eval_gates(gate_inputs(r), r$Close, direction)
    trend <- sum(vapply(c("S1", "S2", "S4", "BK1", "BK2", "BK3"),
                        function(k) isTRUE(g[[k]]), logical(1)))
    other <- isTRUE(g[["S5"]]) || isTRUE(g[["S6"]]) || isTRUE(g[["BK4"]])
    if (trend >= min_trend && other) { n <- n + 1L; last <- as.character(as.Date(r$date)) }
  }
  list(n = n, last_date = last)
}
