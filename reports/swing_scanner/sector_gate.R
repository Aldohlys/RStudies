# sector_gate.R — Sector-level analysis (breadth gate, macro modifiers)

#' Get macro modifier for a sector based on mismatch data
#' @param sec Sector name
#' @param mm_sectors data.frame of sector mismatches from macro_context
#' @param macro_bias Character: "LONG BIAS", "SHORT BIAS", or "NEUTRE"
get_macro_modifier <- function(sec, mm_sectors, macro_bias) {
  mm <- if (!is.null(mm_sectors) && nrow(mm_sectors) > 0) mm_sectors[mm_sectors$sector == sec, ] else data.frame()
  mod <- list(long_boost = 0L, short_boost = 0L, macro_note = "")
  if (nrow(mm) > 0) {
    mtype <- mm$type[1]
    if (mtype %in% c("FAIBLESSE INSOLITE", "SHORT CONFIRME")) {
      mod$short_boost <- 1L
      mod$macro_note <- paste0("Macro: ", mtype)
    } else if (mtype %in% c("RETARD vs MACRO", "FORCE INSOLITE")) {
      mod$long_boost <- 1L
      mod$macro_note <- paste0("Macro: ", mtype)
    }
  }
  if (macro_bias == "SHORT BIAS") mod$short_boost <- mod$short_boost + 1L
  if (macro_bias == "LONG BIAS")  mod$long_boost  <- mod$long_boost + 1L
  mod
}

#' Evaluate sector gate for all sectors
#' @param sectors Character vector of sector names
#' @param sector_etfs Named vector: sector -> ETF symbol
#' @param computed Named list of indicator data.frames
#' @param spy_ret SPY 20d return
#' @param mm_sectors data.frame of sector mismatches
#' @param macro_bias Character bias string
#' @param get_last_fn Function to get last row from computed
#' @return Named list of sector gate results
evaluate_sector_gates <- function(sectors, sector_etfs, computed, spy_ret,
                                   mm_sectors, macro_bias, get_last_fn) {
  sector_ok <- list()
  for (sec in sectors) {
    etf  <- sector_etfs[sec]
    last <- get_last_fn(computed, etf)
    mmod <- get_macro_modifier(sec, mm_sectors, macro_bias)
    if (is.null(last)) {
      sector_ok[[sec]] <- list(long = FALSE, short = FALSE, etf = etf,
        ret20 = NA, rs = NA, macro_mod = mmod)
      next
    }
    above_ma20 <- last$Close > last$ma20
    slope_pos  <- !is.na(last$ma20_slope) && last$ma20_slope > 0
    slope_neg  <- !is.na(last$ma20_slope) && last$ma20_slope < 0
    adx_ok     <- !is.na(last$adx10) && last$adx10 > 20
    rs         <- round(last$ret20 - spy_ret, 1)
    long_ok    <- above_ma20  && slope_pos && adx_ok
    short_ok   <- !above_ma20 && slope_neg && adx_ok

    sector_ok[[sec]] <- list(long = long_ok, short = short_ok, etf = etf,
      ret20 = last$ret20, rs = rs,
      etf_close = last$Close, etf_ma20 = last$ma20,
      adx10 = last$adx10, macro_mod = mmod)
  }
  sector_ok
}
