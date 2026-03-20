# sector_gate.R — Sector-level analysis (breadth gate, macro modifiers)

# Sector macro rules: tailwinds and headwinds per sector
# Mirrors SECTOR_RULES from macro_context/analyze.R
SECTOR_MACRO_RULES <- list(
  Energy          = list(tw = c("oil_surging"), hw = c("dxy_strong","s5fi_bear","vix_stress")),
  PreciousMetals  = list(tw = c("dxy_weak","gold_rising","vix_stress","curve_inv"), hw = c("dxy_strong","rates_high")),
  Technology      = list(tw = c("s5fi_bull"), hw = c("rates_high","curve_inv","vix_stress","backwardation")),
  Financials      = list(tw = c("s5fi_bull"), hw = c("curve_inv","vix_stress","backwardation")),
  Industrials     = list(tw = c("s5fi_bull"), hw = c("rates_high","curve_inv","vix_stress")),
  Materials       = list(tw = c("oil_surging","dxy_weak"), hw = c("dxy_strong","rates_high","s5fi_bear")),
  Agriculture     = list(tw = c("oil_surging","dxy_weak"), hw = c("dxy_strong","vix_stress")),
  Defence         = list(tw = c("vix_stress","backwardation"), hw = c("s5fi_bull")),
  Healthcare      = list(tw = c("vix_stress","s5fi_bear"), hw = c("rates_high","s5fi_bull")),
  ConsumerStaples = list(tw = c("vix_stress","s5fi_bear"), hw = c("rates_high","s5fi_bull"))
)

# Human-readable labels for macro flags
MACRO_FLAG_LABELS <- c(
  vix_stress    = "VIX stress",
  backwardation = "VIX backwardation",
  rates_high    = "high rates",
  curve_inv     = "curve inverted",
  dxy_strong    = "strong USD",
  dxy_weak      = "weak USD",
  oil_surging   = "oil surging",
  gold_rising   = "gold rising",
  s5fi_bear     = "breadth bearish",
  s5fi_bull     = "breadth bullish"
)

#' Get macro modifier for a sector based on mismatch data + macro flags
#' @param sec Sector name
#' @param mm_sectors data.frame of sector mismatches from macro_context
#' @param macro_bias Character: "LONG BIAS", "SHORT BIAS", or "NEUTRAL"
#' @param macro data.frame row of macro_context_results (or NULL)
get_macro_modifier <- function(sec, mm_sectors, macro_bias, macro = NULL) {
  mm <- if (!is.null(mm_sectors) && nrow(mm_sectors) > 0) mm_sectors[mm_sectors$sector == sec, ] else data.frame()
  mod <- list(long_boost = 0L, short_boost = 0L, macro_note = "", tw_active = character(0), hw_active = character(0))

  # Mismatch-driven notes
  if (nrow(mm) > 0) {
    mtype <- mm$type[1]
    if (mtype %in% c("UNUSUAL WEAKNESS", "CONFIRMED SHORT")) {
      mod$short_boost <- 1L
      mod$macro_note <- mtype
    } else if (mtype %in% c("LAGGING vs MACRO", "UNUSUAL STRENGTH")) {
      mod$long_boost <- 1L
      mod$macro_note <- mtype
    } else if (mtype == "FRAGILE RALLY") {
      mod$macro_note <- mtype
    }
  }

  # Compute active tailwinds/headwinds from macro flags
  if (!is.null(macro) && sec %in% names(SECTOR_MACRO_RULES)) {
    rules <- SECTOR_MACRO_RULES[[sec]]
    for (flag in rules$tw) {
      if (flag %in% names(macro) && isTRUE(as.logical(macro[[flag]][1]))) {
        label <- if (flag %in% names(MACRO_FLAG_LABELS)) MACRO_FLAG_LABELS[flag] else flag
        mod$tw_active <- c(mod$tw_active, label)
      }
    }
    for (flag in rules$hw) {
      if (flag %in% names(macro) && isTRUE(as.logical(macro[[flag]][1]))) {
        label <- if (flag %in% names(MACRO_FLAG_LABELS)) MACRO_FLAG_LABELS[flag] else flag
        mod$hw_active <- c(mod$hw_active, label)
      }
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
#' @param macro data.frame of macro_context_results (or NULL)
#' @return Named list of sector gate results
evaluate_sector_gates <- function(sectors, sector_etfs, computed, spy_ret,
                                   mm_sectors, macro_bias, get_last_fn, macro = NULL) {
  sector_ok <- list()
  for (sec in sectors) {
    etf  <- sector_etfs[sec]
    last <- get_last_fn(computed, etf)
    mmod <- get_macro_modifier(sec, mm_sectors, macro_bias, macro)
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
