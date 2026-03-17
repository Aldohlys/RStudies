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
  F1a <- !is.na(last$obv_slope)  && last$obv_slope > 0
  F1b <- !is.na(last$updn_ratio) && last$updn_ratio > 1.1
  list(score = sum(c(L3, L4a, L4b, L4c, L4d, L5a, L5b, F1a, F1b)), rs = rs, ma50d = ma50d,
       flags = paste0("L3:", ifelse(L3, "+", "-"), " L4:", sum(c(L4a, L4b, L4c, L4d)),
                      "/4 L5:", ifelse(L5a, "+", "-"), ifelse(L5b, "+", "-"),
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
  F2a <- !is.na(last$obv_slope)  && last$obv_slope < 0
  F2b <- !is.na(last$updn_ratio) && last$updn_ratio < 0.9
  list(score = sum(c(S3, S4a, S4b, S4c, S4d, S5a, S5b, F2a, F2b)), rs = rs, ma50d = ma50d,
       flags = paste0("S3:", ifelse(S3, "+", "-"), " S4:", sum(c(S4a, S4b, S4c, S4d)),
                      "/4 S5:", ifelse(S5a, "+", "-"), ifelse(S5b, "+", "-"),
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
