# scoring.R — Long/short scoring logic + macro boost integration

#' Score a stock for long setup
#' @param last Single-row data.frame with indicators
#' @param price Current price
#' @param etf_ret ETF 20d return for relative strength
#' @return list with score, rs, ma50d, flags
score_long <- function(last, price, etf_ret) {
  rs  <- round(last$ret20 - etf_ret, 1)
  ma50d <- ifelse(!is.na(last$ma50) && last$ma50 > 0, (price - last$ma50) / last$ma50 * 100, 0)
  L3  <- rs > 0
  L4a <- !is.na(last$ma50)       && price > last$ma50
  L4b <- !is.na(last$ma50_slope) && last$ma50_slope > 0
  L4c <- !is.na(last$adx10)      && last$adx10 > 20 &&
         !is.na(last$dip)        && !is.na(last$din) && last$dip > last$din
  L4d <- !is.na(last$rng_pct)    && last$rng_pct >= 55
  L5a <- !is.na(last$rsi14)      && last$rsi14 > 50 &&
         !is.na(last$rsi_slope)  && last$rsi_slope > 0
  L5b <- ma50d <= 15
  # L5c: room to run — not pinned against resistance (high20)
  L5c <- if (!is.na(last$high20) && !is.na(last$atr14) && last$atr14 > 0)
    (last$high20 - price) / last$atr14 > 1 else TRUE
  F1a <- !is.na(last$obv_slope)  && last$obv_slope > 0
  F1b <- !is.na(last$updn_ratio) && last$updn_ratio > 1.1
  list(score = sum(c(L3, L4a, L4b, L4c, L4d, L5a, L5b, L5c, F1a, F1b)), rs = rs, ma50d = ma50d,
       flags = paste0("L3:", ifelse(L3, "+", "-"), " L4:", sum(c(L4a, L4b, L4c, L4d)),
                      "/4 L5:", ifelse(L5a, "+", "-"), ifelse(L5b, "+", "-"), ifelse(L5c, "+", "-"),
                      " F1:", ifelse(F1a, "+", "-"), ifelse(F1b, "+", "-")))
}

#' Score a stock for short setup
score_short <- function(last, price, etf_ret) {
  rs  <- round(last$ret20 - etf_ret, 1)
  ma50d <- ifelse(!is.na(last$ma50) && last$ma50 > 0, (price - last$ma50) / last$ma50 * 100, 0)
  S3  <- rs < 0
  S4a <- !is.na(last$ma50)       && price < last$ma50
  S4b <- !is.na(last$ma50_slope) && last$ma50_slope < 0
  S4c <- !is.na(last$adx10)      && last$adx10 > 20 &&
         !is.na(last$dip)        && !is.na(last$din) && last$din > last$dip
  S4d <- !is.na(last$rng_pct)    && last$rng_pct <= 45
  S5a <- !is.na(last$rsi14)      && last$rsi14 < 50 &&
         !is.na(last$rsi_slope)  && last$rsi_slope < 0
  S5b <- ma50d >= -15
  # S5c: room to run — not pinned against support (low20)
  S5c <- if (!is.na(last$low20) && !is.na(last$atr14) && last$atr14 > 0)
    (price - last$low20) / last$atr14 > 1 else TRUE
  F2a <- !is.na(last$obv_slope)  && last$obv_slope < 0
  F2b <- !is.na(last$updn_ratio) && last$updn_ratio < 0.9
  list(score = sum(c(S3, S4a, S4b, S4c, S4d, S5a, S5b, S5c, F2a, F2b)), rs = rs, ma50d = ma50d,
       flags = paste0("S3:", ifelse(S3, "+", "-"), " S4:", sum(c(S4a, S4b, S4c, S4d)),
                      "/4 S5:", ifelse(S5a, "+", "-"), ifelse(S5b, "+", "-"), ifelse(S5c, "+", "-"),
                      " F2:", ifelse(F2a, "+", "-"), ifelse(F2b, "+", "-")))
}

#' Convert score to signal label
sig_label <- function(score) ifelse(score >= 7, "TRADE", ifelse(score >= 5, "WATCH", "SKIP"))

#' Signal CSS class for HTML rendering
sig_css <- function(sig) {
  if (grepl("TRADE", sig)) "sig-trade"
  else if (grepl("WATCH", sig)) "sig-watch"
  else "sig-skip"
}

#' Score a stock for breakout (BOT) setup
#'
#' 10 criteria split into 2 phases:
#'   SETUP (6): conditions that build the base over time
#'     S1: Price > MA50 (uptrend established)
#'     S2: MA50 slope > 0 (trend confirmed)
#'     S3: RS > 0 vs sector ETF (relative strength built up)
#'     S4: OBV slope > 0 (accumulation happening)
#'     S5: Squeeze < 0.65 (range contracting)
#'     S6: Vol decline < 0.90 (supply drying up)
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
  rs  <- round(last$ret20 - etf_ret, 1)

  # ── SETUP phase (6 criteria) ───────────────────────────────────────────
  S1  <- !is.na(last$ma50)          && price > last$ma50
  S2  <- !is.na(last$ma50_slope)    && last$ma50_slope > 0
  S3  <- rs > 0
  S4  <- !is.na(last$obv_slope)     && last$obv_slope > 0
  S5  <- !is.na(last$squeeze_ratio) && last$squeeze_ratio < 0.65
  S6  <- !is.na(last$vol_decline)   && last$vol_decline < 0.90

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
