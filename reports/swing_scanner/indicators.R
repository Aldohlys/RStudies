# indicators.R — Technical indicator calculations per stock

#' Calculate all indicators for a single ticker's data
#' @param d data.frame with columns: date, Open, High, Low, Close, Volume
#' @return data.frame with indicator columns added, or NULL if insufficient data
calc_ind <- function(d) {
  d <- d |> dplyr::arrange(date) |>
    dplyr::filter(!is.na(Close), !is.na(High), !is.na(Low), !is.na(Volume))
  if (nrow(d) < 130) return(NULL)

  atr_obj  <- TTR::ATR(cbind(d$High, d$Low, d$Close), n = 14)
  d$atr14  <- as.numeric(atr_obj[, "atr"])

  adx_obj  <- TTR::ADX(cbind(d$High, d$Low, d$Close), n = 10)
  d$adx10  <- as.numeric(adx_obj[, "ADX"])
  d$dip    <- as.numeric(adx_obj[, "DIp"])
  d$din    <- as.numeric(adx_obj[, "DIn"])

  d$ma20   <- as.numeric(TTR::SMA(d$Close, n = 20))
  d$ma50   <- as.numeric(TTR::SMA(d$Close, n = 50))
  d$ma20_slope <- (d$ma20 - dplyr::lag(d$ma20, 5)) / dplyr::lag(d$ma20, 5) * 100
  d$ma50_slope <- (d$ma50 - dplyr::lag(d$ma50, 5)) / dplyr::lag(d$ma50, 5) * 100

  d$high20  <- zoo::rollapply(d$High, width = 20, FUN = max, fill = NA, align = "right")
  d$low20   <- zoo::rollapply(d$Low,  width = 20, FUN = min, fill = NA, align = "right")
  d$rng_pct <- ifelse(d$high20 - d$low20 > 0,
    (d$Close - d$low20) / (d$high20 - d$low20) * 100, 50)

  d$rsi14     <- as.numeric(TTR::RSI(d$Close, n = 14))
  d$rsi_slope <- d$rsi14 - dplyr::lag(d$rsi14, 5)

  direction   <- sign(d$Close - dplyr::lag(d$Close))
  direction[is.na(direction)] <- 0
  d$obv       <- cumsum(direction * d$Volume)
  d$obv_lag20 <- dplyr::lag(d$obv, 20)
  d$obv_slope <- d$obv - d$obv_lag20

  d$up_day     <- d$Close >= dplyr::lag(d$Close)
  d$upvol10    <- zoo::rollapply(ifelse(d$up_day & !is.na(d$up_day), d$Volume, 0),
    width = 10, FUN = sum, fill = NA, align = "right")
  d$dnvol10    <- zoo::rollapply(ifelse(!d$up_day & !is.na(d$up_day), d$Volume, 0),
    width = 10, FUN = sum, fill = NA, align = "right")
  d$updn_ratio <- ifelse(!is.na(d$dnvol10) & d$dnvol10 > 0, d$upvol10 / d$dnvol10, NA)

  d$ret20 <- (d$Close / dplyr::lag(d$Close, 20) - 1) * 100
  d
}

#' Compute indicators for all tickers
#' @param raw data.frame with ticker column
#' @param all_tix character vector of tickers
#' @return Named list of data.frames with indicators
compute_all_indicators <- function(raw, all_tix) {
  computed <- list()
  for (tk in all_tix) {
    res <- calc_ind(raw |> dplyr::filter(ticker == tk))
    if (!is.null(res)) computed[[tk]] <- res
  }
  message("Computed: ", length(computed), "/", length(all_tix), " tickers")
  computed
}

#' Get last row with all required indicators non-NA
get_last <- function(computed, tk) {
  d <- computed[[tk]]
  if (is.null(d)) return(NULL)
  d |> dplyr::filter(!is.na(adx10), !is.na(ma50), !is.na(rsi14),
    !is.na(obv_slope), !is.na(updn_ratio), !is.na(ret20)) |> tail(1)
}
