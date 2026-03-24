# analyze.R — VIX complex, rates, dollar/commodities analysis, mismatches, synthesis
#
# Pure analysis: takes raw data + breadth, returns structured results.
# No I/O — all DB/fetch operations happen in fetch.R and breadth.R.

source(file.path(SCRIPT_DIR, "..", "shared", "html_helpers.R"))

# ── Data accessors ──────────────────────────────────────────────────────────
get_series <- function(raw, tk, n = 60) {
  raw |> dplyr::filter(ticker == tk) |> dplyr::arrange(date) |>
    dplyr::filter(!is.na(Close), !duplicated(date, fromLast = TRUE)) |> tail(n)
}

last_close <- function(raw, tk) {
  d <- get_series(raw, tk, 5)
  if (nrow(d) == 0) NA_real_ else tail(d$Close, 1)
}

prev_close <- function(raw, tk, n = 1) {
  d <- get_series(raw, tk, 30)
  if (nrow(d) < n + 1) NA_real_ else d$Close[nrow(d) - n]
}

ret_n <- function(raw, tk, n) {
  d <- get_series(raw, tk, n + 5)
  if (nrow(d) < n + 1) NA_real_
  else (tail(d$Close, 1) / d$Close[max(1, nrow(d) - n)] - 1) * 100
}

ma_n <- function(series, n) {
  if (length(series) >= n) mean(tail(series, n)) else NA_real_
}

# ── VIX Complex ─────────────────────────────────────────────────────────────
analyze_vix <- function(raw) {
  vix   <- last_close(raw, "^VIX")
  vix9d <- last_close(raw, "^VIX9D")
  vix3m <- last_close(raw, "^VIX3M")
  vvix  <- last_close(raw, "^VVIX")

  vix_zone  <- zone(vix, c(15,20,25,30,40), c("GREEN","GREEN","ORANGE","ORANGE","RED","DARKRED"))
  vix_label <- zone(vix, c(15,20,25,30,40),
    c("Complacency (<15)","Normal (15-20)","Transition (20-25)",
      "Stress (25-30)","Active fear (30-40)","Capitulation (>40)"))

  vvix_zone  <- if (!is.na(vvix)) zone(vvix, c(80,100,120), c("GREEN","ORANGE","RED","DARKRED")) else "ORANGE"
  vvix_label <- if (!is.na(vvix)) zone(vvix, c(80,100,120),
    c("Stable (<80)","Caution (80-100)","Spike likely (100-120)","Crisis (>120)")) else "n/a"

  ts_zone <- "ORANGE"; ts_r <- "n/a"; ts_n <- ""
  ratio <- NA_real_; r_zone <- "ORANGE"; r_note <- ""

  if (!is.na(vix9d) && !is.na(vix) && !is.na(vix3m)) {
    if (vix9d < vix && vix < vix3m) {
      ts_r <- "CONTANGO NORMAL"; ts_zone <- "GREEN"; ts_n <- "Calm — favorable for longs"
    } else if (abs(vix - vix9d) < 1 && abs(vix3m - vix) < 1) {
      ts_r <- "FLAT"; ts_zone <- "ORANGE"; ts_n <- "Rising tension — monitor"
    } else if (vix9d - vix3m > 5) {
      ts_r <- "BACKWARDATION EXT"; ts_zone <- "DARKRED"; ts_n <- "Panic hedging — DO NOT open"
    } else {
      ts_r <- "BACKWARDATION"; ts_zone <- "RED"; ts_n <- "Short-term fear — tight stops"
    }
    ratio  <- vix / vix3m
    r_zone <- ifelse(ratio < 0.85, "RED", ifelse(ratio > 1.10, "GREEN", "ORANGE"))
    r_note <- ifelse(ratio < 0.85, "Mild backwardation", ifelse(ratio > 1.10, "Healthy contango", "Neutral zone"))
  }

  vix_s    <- get_series(raw, "^VIX", 30)
  vix_20d  <- if (nrow(vix_s) >= 21) vix_s$Close[nrow(vix_s) - 20] else NA
  vix_delta <- if (!is.na(vix_20d)) vix - vix_20d else NA
  vix_tend <- if (!is.na(vix_20d)) sprintf("%+.1fpts %s", vix_delta, ifelse(vix > vix_20d, "rising (bearish)", "falling (bullish)")) else "n/a"

  vix_prev <- prev_close(raw, "^VIX")

  list(vix = vix, vix_prev = vix_prev, vix9d = vix9d, vix3m = vix3m, vvix = vvix,
       vix_zone = vix_zone, vix_label = vix_label,
       vvix_zone = vvix_zone, vvix_label = vvix_label,
       ts_zone = ts_zone, ts_r = ts_r, ts_n = ts_n,
       ratio = ratio, r_zone = r_zone, r_note = r_note,
       vix_20d = vix_20d, vix_tend = vix_tend)
}

# ── Rates ────────────────────────────────────────────────────────────────────
analyze_rates <- function(raw) {
  y10 <- last_close(raw, "^TNX")
  y2  <- last_close(raw, "^IRX")
  y30 <- last_close(raw, "^TYX")
  y10_prev <- prev_close(raw, "^TNX")
  y30_prev <- prev_close(raw, "^TYX")

  y10_zone  <- zone(y10, c(3.5,4.5,5.0), c("GREEN","GREEN","ORANGE","RED"))
  y10_label <- zone(y10, c(3.5,4.5,5.0),
    c("Growth-friendly (<3.5%)","Neutral zone (3.5-4.5%)",
      "Valuation pressure (4.5-5%)","Systemic stress (>5%)"))
  y30_zone <- zone(y30, c(3.5,4.5,5.0), c("GREEN","GREEN","ORANGE","RED"))

  sp       <- if (!is.na(y2) && !is.na(y10)) y10 - y2 else NA
  sp_zone  <- if (!is.na(sp)) zone(sp, c(-0.5,0,0.25,1.0), c("DARKRED","RED","ORANGE","GREEN","GREEN")) else "ORANGE"
  sp_label <- if (!is.na(sp)) zone(sp, c(-0.5,0,0.25,1.0),
    c("Deep inversion — recession likely","Mild inversion — slowdown",
      "Flat — transition","Normal steepening — recovery","Strong steepening — reflation")) else "n/a"

  tlt_s <- get_series(raw, "TLT", 30); tlt_pct <- NA; tlt_zone <- "ORANGE"; tlt_n <- ""
  if (nrow(tlt_s) >= 20) {
    tlt_pct  <- (tail(tlt_s$Close, 1) / tlt_s$Close[max(1, nrow(tlt_s) - 19)] - 1) * 100
    tlt_zone <- ifelse(tlt_pct > 1, "GREEN", ifelse(tlt_pct < -1, "RED", "ORANGE"))
    tlt_n    <- ifelse(tlt_pct > 1, "TLT rising: long rates falling",
                 ifelse(tlt_pct < -1, "TLT falling: long rates rising", "TLT stable"))
  }

  tips_s <- get_series(raw, "TIP", 30); tips_pct <- NA; tips_zone <- "ORANGE"; tips_n <- ""
  if (nrow(tips_s) >= 20) {
    tp <- (tail(tips_s$Close, 1) / tips_s$Close[max(1, nrow(tips_s) - 19)] - 1) * 100
    if (!is.na(tp) && abs(tp) <= 15) {
      tips_pct  <- tp
      tips_zone <- ifelse(tp < -0.5, "RED", ifelse(tp > 0.5, "GREEN", "ORANGE"))
      tips_n    <- ifelse(tp < -0.5, "Real yield rising — headwind gold/tech",
                    ifelse(tp > 0.5, "Real yield falling — tailwind gold/tech", "Real yield stable"))
    }
  }

  list(y10 = y10, y2 = y2, y30 = y30, y10_prev = y10_prev, y30_prev = y30_prev,
       y10_zone = y10_zone, y10_label = y10_label, y30_zone = y30_zone,
       sp = sp, sp_zone = sp_zone, sp_label = sp_label,
       tlt_pct = tlt_pct, tlt_zone = tlt_zone, tlt_n = tlt_n,
       tips_pct = tips_pct, tips_zone = tips_zone, tips_n = tips_n)
}

# ── Dollar + Commodities ────────────────────────────────────────────────────
analyze_commodities <- function(raw) {
  dxy_ret20 <- NA; uso_ret20 <- NA; gld_ret20 <- NA; comm_rows <- list()
  for (tk in c("DX-Y.NYB", "USO", "GLD", "XLE")) {
    s <- get_series(raw, tk, 30)
    if (nrow(s) < 20) { comm_rows[[tk]] <- list(tk = tk, cur = NA, z = "ORANGE", note = "NO DATA"); next }
    cur <- tail(s$Close, 1); ma20 <- ma_n(s$Close, 20)
    r5  <- (cur / s$Close[max(1, nrow(s) - 4)] - 1) * 100
    r20 <- (cur / s$Close[max(1, nrow(s) - 19)] - 1) * 100
    above <- cur > ma20
    z <- ifelse(tk == "DX-Y.NYB", ifelse(r20 > 3, "RED", ifelse(r20 > 1, "ORANGE", "GREEN")),
         ifelse(above && r20 > 0, "GREEN", ifelse(!above && r20 < 0, "RED", "ORANGE")))
    note <- sprintf("MA20:%s | 5d:%+.1f%% | 20d:%+.1f%%", ifelse(above, "above", "below"), r5, r20)
    if (tk == "DX-Y.NYB") {
      dxy_ret20 <- r20
      note <- paste0(note, ifelse(r20 > 3, " <- strong: headwind", ifelse(r20 < -2, " <- weak: tailwind", "")))
    }
    if (tk == "USO") uso_ret20 <- r20
    if (tk == "GLD") gld_ret20 <- r20
    comm_rows[[tk]] <- list(tk = tk, cur = cur, z = z, note = note)
  }
  list(dxy_ret20 = dxy_ret20, uso_ret20 = uso_ret20, gld_ret20 = gld_ret20,
       comm_rows = comm_rows)
}

# ── SPY position ────────────────────────────────────────────────────────────
analyze_spy <- function(raw) {
  spy_s <- get_series(raw, "SPY", 60)
  spy_c <- NA; spy_m20 <- NA; spy_m50 <- NA; spy_z <- "ORANGE"; spy_n <- ""
  if (nrow(spy_s) >= 50) {
    spy_c <- tail(spy_s$Close, 1); spy_m20 <- ma_n(spy_s$Close, 20); spy_m50 <- ma_n(spy_s$Close, 50)
    spy_z <- ifelse(spy_c > spy_m20 && spy_c > spy_m50, "GREEN",
               ifelse(spy_c < spy_m20 && spy_c < spy_m50, "RED", "ORANGE"))
    spy_n <- ifelse(spy_c > spy_m20 && spy_c > spy_m50, ">MA20+MA50 uptrend",
               ifelse(spy_c < spy_m20 && spy_c < spy_m50, "<MA20+MA50 downtrend", "between MA20/MA50"))
  }
  list(spy_c = spy_c, spy_m20 = spy_m20, spy_m50 = spy_m50, spy_z = spy_z, spy_n = spy_n)
}

# ── Mismatches ──────────────────────────────────────────────────────────────
SECTOR_ETFS_MAP <- list(
  Energy = "XLE", Agriculture = "MOO", Defence = "ITA", Materials = "XLB",
  Industrials = "XLI", PreciousMetals = "GDX", Technology = "XLK",
  Financials = "XLF", Healthcare = "XLV", ConsumerStaples = "XLP"
)

SECTOR_RULES <- list(
  Energy         = list(tw = c("oil_surging"), hw = c("dxy_strong","s5fi_bear","vix_stress")),
  PreciousMetals = list(tw = c("dxy_weak","gold_rising","vix_stress","curve_inv"), hw = c("dxy_strong","rates_high")),
  Technology     = list(tw = c("s5fi_bull"), hw = c("rates_high","curve_inv","vix_stress","backwardation")),
  Financials     = list(tw = c("s5fi_bull"), hw = c("curve_inv","vix_stress","backwardation")),
  Industrials    = list(tw = c("s5fi_bull"), hw = c("rates_high","curve_inv","vix_stress")),
  Materials      = list(tw = c("oil_surging","dxy_weak"), hw = c("dxy_strong","rates_high","s5fi_bear")),
  Agriculture    = list(tw = c("oil_surging","dxy_weak"), hw = c("dxy_strong","vix_stress")),
  Defence        = list(tw = c("vix_stress","backwardation"), hw = c("s5fi_bull")),
  Healthcare     = list(tw = c("vix_stress","s5fi_bear"), hw = c("rates_high","s5fi_bull")),
  ConsumerStaples = list(tw = c("vix_stress","s5fi_bear"), hw = c("rates_high","s5fi_bull"))
)

analyze_mismatches <- function(raw, macro_env) {
  get_etf_profile <- function(tk) {
    s <- get_series(raw, tk, 60); if (nrow(s) < 25) return(NULL)
    cur <- tail(s$Close, 1); ma20 <- ma_n(s$Close, 20)
    r20 <- (cur / s$Close[max(1, nrow(s) - 19)] - 1) * 100
    rs  <- if (!is.na(ret_n(raw, "SPY", 20))) r20 - ret_n(raw, "SPY", 20) else NA
    slope20 <- if (length(s$Close) >= 25)
      (ma20 - mean(tail(head(s$Close, nrow(s) - 5), 5))) / mean(tail(head(s$Close, nrow(s) - 5), 5)) * 100 else 0
    trend <- if (cur > ma20 && slope20 > 0) "UP" else if (cur < ma20 && slope20 < 0) "DOWN" else "FLAT"
    list(cur = cur, r20 = r20, rs = rs, trend = trend)
  }

  mismatches <- list()
  for (sec in names(SECTOR_RULES)) {
    etf <- SECTOR_ETFS_MAP[[sec]]; prof <- get_etf_profile(etf); rules <- SECTOR_RULES[[sec]]
    if (is.null(prof)) next
    tw_active <- sapply(rules$tw, function(c) isTRUE(macro_env[[c]]))
    hw_active <- sapply(rules$hw, function(c) isTRUE(macro_env[[c]]))
    n_tw <- sum(tw_active); n_hw <- sum(hw_active)
    tw_names <- rules$tw[tw_active]; hw_names <- rules$hw[hw_active]
    type <- NULL; note <- NULL; signal <- NULL
    if (prof$trend == "UP" && n_hw >= 2 && n_tw == 0) {
      type <- "UNUSUAL STRENGTH"
      note <- sprintf("%s rising with no tailwinds, %d headwinds (%s)", etf, n_hw, paste(hw_names, collapse = "+"))
      signal <- "LONG high conviction | SHORT if RSI>75"
    } else if (prof$trend == "UP" && n_hw >= 2 && n_tw >= 1) {
      type <- "FRAGILE RALLY"
      note <- sprintf("%s rising (%s) but %d headwinds (%s)", etf, paste(tw_names, collapse = "+"), n_hw, paste(hw_names, collapse = "+"))
      signal <- "LONG with tight stop"
    } else if (prof$trend == "DOWN" && n_tw >= 2 && n_hw == 0) {
      type <- "UNUSUAL WEAKNESS"
      note <- sprintf("%s falling despite %d tailwinds (%s)", etf, n_tw, paste(tw_names, collapse = "+"))
      signal <- "SHORT strong — structural weakness"
    } else if (prof$trend == "FLAT" && n_tw >= 2 && n_hw == 0 && !is.na(prof$rs) && prof$rs < -3) {
      type <- "LAGGING vs MACRO"
      note <- sprintf("%s flat RS:%+.1f%% despite %d tailwinds (%s)", etf, prof$rs, n_tw, paste(tw_names, collapse = "+"))
      signal <- "LONG upcoming — wait for MA20 gate"
    } else if (prof$trend == "DOWN" && n_hw >= 2 && !is.na(prof$rs) && prof$rs < -4) {
      type <- "CONFIRMED SHORT"
      note <- sprintf("%s downtrend + %d headwinds (%s) RS:%+.1f%%", etf, n_hw, paste(hw_names, collapse = "+"), prof$rs)
      signal <- "SHORT high conviction"
    }
    if (!is.null(type))
      mismatches[[sec]] <- list(sector = sec, etf = etf, trend = prof$trend, r20 = prof$r20, rs = prof$rs,
        n_tw = n_tw, n_hw = n_hw, tw_names = tw_names, hw_names = hw_names,
        type = type, note = note, signal = signal)
  }
  mismatches
}

# ── Synthesis ───────────────────────────────────────────────────────────────
synthesize <- function(vix_res, rates_res, breadth, commodities_res, mismatches) {
  sl <- 0; ss <- 0; synth_notes <- list()
  add_note <- function(t, z) { synth_notes[[length(synth_notes) + 1]] <<- list(t = t, z = z) }

  vix <- vix_res$vix; vix9d <- vix_res$vix9d; vix3m <- vix_res$vix3m
  y10 <- rates_res$y10; S5FI_VALUE <- breadth$pct

  if (!is.na(vix)) {
    if (vix < 20) { sl <- sl + 2; add_note(sprintf("VIX %.1f <20 — longs favored", vix), "GREEN") }
    else if (vix < 30) { add_note(sprintf("VIX %.1f 20-30 — neutral", vix), "ORANGE") }
    else { ss <- ss + 1; add_note(sprintf("VIX %.1f >30 — active fear", vix), "RED") }
  }
  if (!is.na(vix9d) && !is.na(vix3m)) {
    if (vix9d > vix3m) { ss <- ss + 2; add_note(sprintf("Backwardation (9d:%.1f > 3m:%.1f) — high risk", vix9d, vix3m), "RED") }
    else { sl <- sl + 1; add_note(sprintf("Contango (9d:%.1f < 3m:%.1f) — normal structure", vix9d, vix3m), "GREEN") }
  }
  if (!is.na(y10)) {
    if (y10 < 4.5) { sl <- sl + 1; add_note(sprintf("10Y %.2f%% <4.5 — manageable", y10), "GREEN") }
    else if (y10 < 5) { add_note(sprintf("10Y %.2f%% — valuation pressure", y10), "ORANGE") }
    else { ss <- ss + 2; add_note(sprintf("10Y %.2f%% >5 — systemic stress", y10), "RED") }
  }
  if (!is.na(S5FI_VALUE)) {
    if (S5FI_VALUE > 50) { sl <- sl + 2; add_note(sprintf("S5FI %.1f%% >50 — breadth bullish", S5FI_VALUE), "GREEN") }
    else if (S5FI_VALUE > 35) { add_note(sprintf("S5FI %.1f%% 35-50 — transition", S5FI_VALUE), "ORANGE") }
    else if (S5FI_VALUE > 20) { ss <- ss + 1; add_note(sprintf("S5FI %.1f%% 20-35 — oversold", S5FI_VALUE), "ORANGE") }
    else { ss <- ss + 2; add_note(sprintf("S5FI %.1f%% <20 — capitulation", S5FI_VALUE), "DARKRED") }
  }

  n_short_mm <- if (length(mismatches) == 0) 0L else sum(vapply(mismatches, function(m)
    m$type %in% c("UNUSUAL WEAKNESS", "CONFIRMED SHORT"), logical(1)))
  n_long_mm  <- if (length(mismatches) == 0) 0L else sum(vapply(mismatches, function(m)
    m$type == "LAGGING vs MACRO", logical(1)))

  if (n_short_mm > 0) { ss <- ss + n_short_mm; add_note(sprintf("Mismatch SHORT x%d", n_short_mm), "RED") }
  if (n_long_mm  > 0) { sl <- sl + n_long_mm;  add_note(sprintf("Mismatch LONG x%d",  n_long_mm),  "GREEN") }

  bias      <- ifelse(sl > ss + 1, "LONG BIAS", ifelse(ss > sl + 1, "SHORT BIAS", "NEUTRAL"))
  bias_zone <- ifelse(bias == "LONG BIAS", "GREEN", ifelse(bias == "SHORT BIAS", "RED", "ORANGE"))

  # Build brief explanation of what drives the bias
  long_drivers <- c()
  short_drivers <- c()
  if (!is.na(vix)) {
    if (vix < 20) long_drivers <- c(long_drivers, sprintf("VIX %.0f", vix))
    else if (vix >= 30) short_drivers <- c(short_drivers, sprintf("VIX %.0f", vix))
  }
  if (!is.na(vix9d) && !is.na(vix3m)) {
    if (vix9d > vix3m) short_drivers <- c(short_drivers, "backwardation")
    else long_drivers <- c(long_drivers, "contango")
  }
  if (!is.na(y10)) {
    if (y10 < 4.5) long_drivers <- c(long_drivers, sprintf("10Y %.1f%%", y10))
    else if (y10 >= 5) short_drivers <- c(short_drivers, sprintf("10Y %.1f%%", y10))
  }
  if (!is.na(S5FI_VALUE)) {
    if (S5FI_VALUE > 50) long_drivers <- c(long_drivers, sprintf("breadth %.0f%%", S5FI_VALUE))
    else if (S5FI_VALUE <= 35) short_drivers <- c(short_drivers, sprintf("breadth %.0f%%", S5FI_VALUE))
  }
  if (n_short_mm > 0) short_drivers <- c(short_drivers, sprintf("%d short mismatch", n_short_mm))
  if (n_long_mm > 0)  long_drivers  <- c(long_drivers, sprintf("%d long mismatch", n_long_mm))

  explain_parts <- c()
  if (length(long_drivers) > 0) explain_parts <- c(explain_parts, paste0("Long: ", paste(long_drivers, collapse = ", ")))
  if (length(short_drivers) > 0) explain_parts <- c(explain_parts, paste0("Short: ", paste(short_drivers, collapse = ", ")))
  bias_explain <- paste(explain_parts, collapse = " | ")

  list(sl = sl, ss = ss, bias = bias, bias_zone = bias_zone, bias_explain = bias_explain, synth_notes = synth_notes)
}

# ── Build macro environment flags ───────────────────────────────────────────
build_macro_env <- function(vix_res, rates_res, breadth, commodities_res) {
  list(
    vix_stress    = !is.na(vix_res$vix)           && vix_res$vix > 25,
    backwardation = !is.na(vix_res$vix9d)          && !is.na(vix_res$vix3m) && vix_res$vix9d > vix_res$vix3m,
    rates_high    = !is.na(rates_res$y10)          && rates_res$y10 > 4.5,
    curve_inv     = !is.na(rates_res$y2)           && !is.na(rates_res$y10) && (rates_res$y10 - rates_res$y2) < 0,
    dxy_strong    = !is.na(commodities_res$dxy_ret20) && commodities_res$dxy_ret20 > 2,
    dxy_weak      = !is.na(commodities_res$dxy_ret20) && commodities_res$dxy_ret20 < -2,
    oil_surging   = !is.na(commodities_res$uso_ret20) && commodities_res$uso_ret20 > 10,
    gold_rising   = !is.na(commodities_res$gld_ret20) && commodities_res$gld_ret20 > 5,
    s5fi_bear     = !is.na(breadth$pct)            && breadth$pct < 35,
    s5fi_bull     = !is.na(breadth$pct)            && breadth$pct > 50
  )
}
