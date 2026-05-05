# reports/shared/indicators.R — shared technical-indicator computation.
#
# Single source of truth for swing_scanner (universe scan) and /analyze
# (per-ticker drill-down).
#
# Functions:
#   calc_ind(d)              — append technical-indicator columns to OHLCV df
#   compute_all_indicators() — apply calc_ind to a full universe
#   get_last(computed, tk)   — last row with all required indicators non-NA
#   compute_breakdown(last, price, direction)
#       — emit a list of mechanical per-criterion rows for /analyze Phase B
#         (PASS/FAIL per BOT setup criterion S1-S6 + breakout BK1-BK4, mirrored
#         for short direction). No scoring; just the raw ledger.

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

  # ── BOT breakout indicators ────────────────────────────────────────────
  d$high40  <- zoo::rollapply(d$High, width = 40, FUN = max, fill = NA, align = "right")
  d$low40   <- zoo::rollapply(d$Low,  width = 40, FUN = min, fill = NA, align = "right")
  range_20  <- d$high20 - d$low20
  range_40  <- d$high40 - d$low40
  d$squeeze_ratio <- ifelse(!is.na(range_40) & range_40 > 0, range_20 / range_40, NA)

  d$vol_ma20 <- zoo::rollapply(d$Volume, width = 20, FUN = mean, fill = NA, align = "right")
  d$vol_ma50 <- zoo::rollapply(d$Volume, width = 50, FUN = mean, fill = NA, align = "right")
  d$vol_decline <- ifelse(!is.na(d$vol_ma50) & d$vol_ma50 > 0, d$vol_ma20 / d$vol_ma50, NA)
  d$vol_surge <- ifelse(!is.na(d$vol_ma20) & d$vol_ma20 > 0, d$Volume / d$vol_ma20, NA)

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

#' Per-criterion breakdown for /analyze Phase B.
#'
#' Emits one row per technical criterion the scanner uses. Same thresholds as
#' `score_breakout` (S1-S6 setup + BK1-BK4 breakout), mirrored for short.
#'
#' @param last single-row indicators data.frame (output of get_last)
#' @param price current close
#' @param direction "long" or "short"
#' @return data.frame: id, label, value, threshold, pass (logical), note
compute_breakdown <- function(last, price, direction = "long") {
  if (is.null(last) || nrow(last) == 0) {
    return(data.frame(id = character(0), label = character(0),
                      value = character(0), threshold = character(0),
                      pass = logical(0), note = character(0),
                      stringsAsFactors = FALSE))
  }
  ll <- as.list(last[1, , drop = FALSE])
  long <- identical(direction, "long")

  fmt <- function(x, d = 2) {
    if (is.null(x) || length(x) == 0 || is.na(x)) "n/a"
    else if (is.numeric(x)) sprintf(paste0("%.", d, "f"), x)
    else as.character(x)
  }
  # Format OBV slope (cumulative signed volume over 20 sessions) as a
  # readable M-share figure plus % of 20d total volume, instead of a raw
  # 9-digit count. obv_slope is in shares; vol_ma20 is mean daily volume,
  # so total_vol_20d ≈ vol_ma20 * 20.
  .fmt_obv_slope <- function(obv_slope, vol_ma20) {
    if (is.null(obv_slope) || is.na(obv_slope)) return("n/a")
    abs_v <- abs(obv_slope)
    mag <- if (abs_v >= 1e9) sprintf("%+.2fB", obv_slope / 1e9)
           else if (abs_v >= 1e6) sprintf("%+.1fM", obv_slope / 1e6)
           else if (abs_v >= 1e3) sprintf("%+.1fK", obv_slope / 1e3)
           else sprintf("%+.0f", obv_slope)
    pct <- if (!is.null(vol_ma20) && !is.na(vol_ma20) && vol_ma20 > 0)
             sprintf(" (%+.1f%% of 20d vol)", obv_slope / (vol_ma20 * 20) * 100)
           else ""
    paste0(mag, pct)
  }
  row <- function(id, label, value, threshold, pass, note = "") {
    data.frame(id = id, label = label, value = value, threshold = threshold,
               pass = pass, note = note, stringsAsFactors = FALSE)
  }
  na_safe_pass <- function(x) isTRUE(x)

  # ── SETUP (S1-S6) — long thresholds; mirror for short ─────────────────
  S1 <- if (long) (!is.na(ll$ma50) && price > ll$ma50)
        else      (!is.na(ll$ma50) && price < ll$ma50)
  S2 <- if (long) (!is.na(ll$ma50_slope) && ll$ma50_slope > 0)
        else      (!is.na(ll$ma50_slope) && ll$ma50_slope < 0)
  S4 <- if (long) (!is.na(ll$obv_slope) && ll$obv_slope > 0)
        else      (!is.na(ll$obv_slope) && ll$obv_slope < 0)
  S5 <- (!is.na(ll$squeeze_ratio) && ll$squeeze_ratio < 0.65)
  S6 <- (!is.na(ll$vol_decline)   && ll$vol_decline   < 0.95)

  # ── BREAKOUT (BK1-BK4) — long thresholds; mirror for short ────────────
  BK1 <- if (long) (!is.na(ll$rsi14) && ll$rsi14 > 50 && !is.na(ll$rsi_slope) && ll$rsi_slope > 0)
         else      (!is.na(ll$rsi14) && ll$rsi14 < 50 && !is.na(ll$rsi_slope) && ll$rsi_slope < 0)
  BK2 <- if (long) (!is.na(ll$updn_ratio) && ll$updn_ratio > 1.1)
         else      (!is.na(ll$updn_ratio) && ll$updn_ratio < 0.9)
  BK3 <- if (long) (!is.na(ll$rng_pct) && ll$rng_pct >= 70)
         else      (!is.na(ll$rng_pct) && ll$rng_pct <= 30)
  BK4 <- (!is.na(ll$vol_surge) && ll$vol_surge >= 1.2)

  # ── Auxiliary indicators (informational, no PASS gate) ────────────────
  ADX_passes <- !is.na(ll$adx10) && ll$adx10 > 20

  ma50_disp <- if (!is.na(ll$ma50) && ll$ma50 > 0)
                 (price - ll$ma50) / ll$ma50 * 100 else NA_real_
  atr_pct   <- if (!is.na(ll$atr14) && price > 0) ll$atr14 / price * 100 else NA_real_

  rows <- rbind(
    row("S1",  "Price vs MA50",
        sprintf("%s vs %s", fmt(price), fmt(ll$ma50)),
        if (long) "price > MA50" else "price < MA50",
        S1, sprintf("disp = %s%%", fmt(ma50_disp, 1))),
    row("S2",  "MA50 slope",
        sprintf("%s%%", fmt(ll$ma50_slope, 2)),
        if (long) "> 0" else "< 0",
        S2, "5-day slope"),
    row("S4",  "OBV slope (20d)",
        .fmt_obv_slope(ll$obv_slope, ll$vol_ma20),
        if (long) "> 0 (accumulation)" else "< 0 (distribution)",
        S4, "share-volume net of last 20 sessions"),
    row("S5",  "Squeeze ratio (20d/40d)",
        fmt(ll$squeeze_ratio, 3),
        "< 0.65 (range contracting)",
        S5, ""),
    row("S6",  "Volume decline (20/50d)",
        fmt(ll$vol_decline, 3),
        "< 0.95 (supply drying up)",
        S6, ""),
    row("BK1", "RSI(14) + slope",
        sprintf("rsi=%s slope=%s", fmt(ll$rsi14, 1), fmt(ll$rsi_slope, 1)),
        if (long) "> 50 & slope > 0" else "< 50 & slope < 0",
        BK1, ""),
    row("BK2", "Up/Down volume ratio (10d)",
        fmt(ll$updn_ratio, 2),
        if (long) "> 1.1" else "< 0.9",
        BK2, ""),
    row("BK3", "Range position (20d)",
        sprintf("%s%%", fmt(ll$rng_pct, 1)),
        if (long) ">= 70% (near high)" else "<= 30% (near low)",
        BK3, ""),
    row("BK4", "Volume surge today",
        sprintf("%sx", fmt(ll$vol_surge, 2)),
        ">= 1.2x of 20d avg",
        BK4, ""),
    row("AUX_ADX", "ADX(10) (trend strength)",
        fmt(ll$adx10, 1),
        "> 20 = trending",
        ADX_passes, "informational"),
    row("AUX_RET", "20-day return",
        sprintf("%s%%", fmt(ll$ret20, 1)),
        "informational",
        NA, ""),
    row("AUX_ATR", "ATR(14) % of price",
        sprintf("%s%%", fmt(atr_pct, 2)),
        "informational",
        NA, "")
  )

  setup_pass <- sum(rows$pass[rows$id %in% c("S1","S2","S4","S5","S6")], na.rm = TRUE)
  bk_pass    <- sum(rows$pass[rows$id %in% c("BK1","BK2","BK3","BK4")], na.rm = TRUE)
  attr(rows, "setup_count") <- setup_pass     # of 5 (S3 = sector RS, computed elsewhere)
  attr(rows, "breakout_count") <- bk_pass     # of 4
  rows
}

#' Single-ticker OHLCV fetch for /analyze (300d back, daily). Uses Tdata's
#' Yahoo helper. Returns a data.frame compatible with calc_ind().
fetch_single_ohlcv <- function(ticker) {
  tryCatch({
    yn <- tryCatch(Tdata::getYahooName(ticker), error = function(e) ticker)
    if (is.na(yn) || yn == "BASE_CURRENCY") yn <- ticker
    raw <- Tdata::getYahooData(tickers = yn,
                               from_date = Sys.Date() - 300,
                               to_date   = Sys.Date())
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    if (!"date" %in% names(raw) && "Date" %in% names(raw)) names(raw)[names(raw) == "Date"] <- "date"
    raw$ticker <- ticker
    raw
  }, error = function(e) {
    message("fetch_single_ohlcv failed: ", conditionMessage(e)); NULL
  })
}
