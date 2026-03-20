# backtest_regimes.R — Sigmoid parameter calibration from BOT trade history
#
# Purpose: Match BOT trades (entry date + P&L) against macro conditions at entry,
# then find optimal sigmoid centers/scales where win rate transitions sharply.
#
# Outputs:
#   - Console: calibrated parameters vs current ones
#   - DB table: backtest_regime_signals (trade-level signal values + outcome)
#   - File: backtest_results.md (full report)
#
# Usage: source("backtest_regimes.R") from RStudio or Rscript

library(DBI)
library(RSQLite)
library(dplyr)
library(quantmod)
library(tidyr)
library(purrr)

# ── Config ────────────────────────────────────────────────────────────────────

DB_PATH   <- "C:/Users/aldoh/Documents/RApplication/data/mydb.db"
OUT_DIR   <- "C:/Users/aldoh/Documents/NewTrading"
STRATEGY  <- "BOT"

# Yahoo tickers for macro signals
MACRO_TICKERS <- c(
  vix    = "^VIX",
  vix3m  = "^VIX3M",
  tnx    = "^TNX",      # 10Y yield (in %)
  dxy    = "DX-Y.NYB",
  tlt    = "TLT",
  hyg    = "HYG",
  lqd    = "LQD",
  cper   = "CPER",
  gld    = "GLD",
  uso    = "USO"
)

# Current sigmoid parameters from scenarios.R (to compare against)
CURRENT_PARAMS <- list(
  vix_stress    = list(center = 25,   scale = 4),
  vix_calm      = list(center = 20,   scale = 3),
  backwardation = list(center = 0.10, scale = 0.05),
  breadth_bull  = list(center = 50,   scale = 10),
  breadth_bear  = list(center = 35,   scale = 8),
  rates_press   = list(center = 4.5,  scale = 0.3),
  dxy_strength  = list(center = 2,    scale = 1.5),
  tlt_bid       = list(center = 1,    scale = 1.5),
  credit_stress = list(center = 1,    scale = 1),
  copper_gold   = list(center = 0,    scale = 2)
)

# ── Helpers ───────────────────────────────────────────────────────────────────

sig <- function(x, center, scale) {
  ifelse(is.na(x), 0.5, 1 / (1 + exp(-(x - center) / scale)))
}

# ── Step 1: Extract matched BOT trades ────────────────────────────────────────

extract_bot_trades <- function(conn) {
  sql <- "
    SELECT TradeNr,
      MIN(TradeDate) as entry_date,
      MAX(TradeDate) as exit_date,
      SUM(PnL) as total_pnl,
      GROUP_CONCAT(DISTINCT Ssjacent) as underlying
    FROM Trades
    WHERE Strategy = ?
    GROUP BY TradeNr
    HAVING SUM(CASE WHEN Pos < 0 THEN 1 ELSE 0 END) > 0
  "
  trades <- dbGetQuery(conn, sql, params = list(STRATEGY))
  trades$entry_date <- as.Date(as.character(trades$entry_date), format = "%Y%m%d")
  trades$exit_date  <- as.Date(as.character(trades$exit_date), format = "%Y%m%d")
  trades$outcome    <- ifelse(trades$total_pnl > 0, "WIN", "LOSS")
  trades$hold_days  <- as.integer(trades$exit_date - trades$entry_date)

  message(sprintf("Extracted %d closed BOT trades: %d wins, %d losses",
    nrow(trades), sum(trades$outcome == "WIN"), sum(trades$outcome == "LOSS")))
  trades
}

# ── Step 2: Fetch Yahoo macro history ─────────────────────────────────────────

fetch_macro_history <- function(from_date, to_date) {
  message("Fetching macro history from Yahoo...")
  all_data <- list()

  for (nm in names(MACRO_TICKERS)) {
    tk <- MACRO_TICKERS[nm]
    message(sprintf("  %s (%s)...", nm, tk))
    tryCatch({
      data <- getSymbols(tk, src = "yahoo",
        from = from_date - 30,  # extra lookback for 20d returns
        to = to_date + 1,
        auto.assign = FALSE)
      # Convert to data.frame
      df <- data.frame(
        date  = index(data),
        close = as.numeric(Cl(data)),
        stringsAsFactors = FALSE
      )
      df$ticker <- nm
      all_data[[nm]] <- df
    }, error = function(e) {
      message(sprintf("    WARN: failed to fetch %s: %s", tk, e$message))
    })
  }

  bind_rows(all_data)
}

# ── Step 3: Compute signals at each trade entry date ──────────────────────────

compute_signals_at_date <- function(target_date, macro) {
  # Helper: get value at date (or nearest prior)
  get_val <- function(ticker_name) {
    d <- macro %>%
      filter(ticker == ticker_name, date <= target_date, !is.na(close)) %>%
      arrange(desc(date)) %>%
      slice(1)
    if (nrow(d) == 0) return(NA_real_)
    d$close
  }

  # Helper: 20-day return (%)
  ret20 <- function(ticker_name) {
    d <- macro %>%
      filter(ticker == ticker_name, date <= target_date, !is.na(close)) %>%
      arrange(date) %>%
      tail(25)  # some buffer for weekends
    if (nrow(d) < 15) return(NA_real_)  # need at least ~15 trading days
    today_price <- tail(d$close, 1)
    past_price  <- d$close[max(1, nrow(d) - 19)]
    (today_price / past_price - 1) * 100
  }

  vix     <- get_val("vix")
  vix3m   <- get_val("vix3m")
  y10     <- get_val("tnx")
  dxy_ret <- ret20("dxy")
  tlt_ret <- ret20("tlt")
  hyg_ret <- ret20("hyg")
  lqd_ret <- ret20("lqd")
  cper_ret <- ret20("cper")
  gld_ret <- ret20("gld")
  uso_ret <- ret20("uso")

  ratio <- if (!is.na(vix) && !is.na(vix3m) && vix3m > 0) vix / vix3m else NA
  copper_gold_ret <- if (!is.na(cper_ret) && !is.na(gld_ret)) cper_ret - gld_ret else NA
  credit_health <- if (!is.na(hyg_ret)) sig(hyg_ret, 0, 1.5) else 0.5

  # Raw inputs (for calibration analysis)
  raw <- list(
    vix = vix, vix3m = vix3m, ratio = ratio, y10 = y10,
    dxy_ret = dxy_ret, tlt_ret = tlt_ret, hyg_ret = hyg_ret,
    uso_ret = uso_ret, gld_ret = gld_ret,
    cper_ret = cper_ret, copper_gold_ret = copper_gold_ret
  )

  # Computed signals (using CURRENT parameters)
  signals <- list(
    vix_stress    = sig(vix, 25, 4),
    vix_calm      = 1 - sig(vix, 20, 3),
    backwardation = if (!is.na(ratio)) sig(1 - ratio, 0.10, 0.05) else 0,
    rates_press   = sig(y10, 4.5, 0.3),
    dxy_strength  = sig(dxy_ret, 2, 1.5),
    reflation     = mean(c(sig(uso_ret, 8, 4), sig(gld_ret, 4, 3),
                           1 - sig(dxy_ret, 2, 1.5)), na.rm = TRUE),
    tlt_bid       = sig(tlt_ret, 1, 1.5),
    credit_stress = if (!is.na(hyg_ret)) sig(-hyg_ret, 1, 1) else 0.3,
    copper_gold   = sig(copper_gold_ret, 0, 2)
  )
  signals$sentiment <- mean(c(signals$vix_calm, credit_health), na.rm = TRUE)

  list(raw = raw, signals = signals)
}

# ── Step 4: Calibrate sigmoid parameters ──────────────────────────────────────
# For each raw input, find the center where win rate crosses 50%
# and the scale from the sharpness of the transition.

calibrate_sigmoid <- function(values, outcomes, signal_name) {
  # Remove NAs
  valid <- !is.na(values)
  v <- values[valid]
  o <- outcomes[valid]
  n <- length(v)

  if (n < 10) {
    return(list(center = NA, scale = NA, n = n, note = "insufficient data"))
  }

  wins <- o == "WIN"
  win_rate_overall <- mean(wins)

  # Sort by value
  ord <- order(v)
  v_sorted <- v[ord]
  w_sorted <- wins[ord]

  # Sliding window win rate (window = max(5, n/5))
  window <- max(5, floor(n / 5))
  half_w <- floor(window / 2)

  centers <- numeric(0)
  rates   <- numeric(0)

  for (i in (half_w + 1):(n - half_w)) {
    idx <- max(1, i - half_w):min(n, i + half_w)
    centers <- c(centers, v_sorted[i])
    rates   <- c(rates, mean(w_sorted[idx]))
  }

  if (length(centers) < 3) {
    return(list(center = NA, scale = NA, n = n, note = "window too small"))
  }

  # Find where win rate crosses 50% (or nearest approach)
  # If win rate is always above or below 50%, use the median of values
  crossings <- which(diff(sign(rates - 0.5)) != 0)

  if (length(crossings) > 0) {
    # Use the crossing closest to the middle of the data
    mid_idx <- crossings[which.min(abs(crossings - length(centers) / 2))]
    center <- (centers[mid_idx] + centers[mid_idx + 1]) / 2
  } else {
    # No crossing: use the value where win rate is closest to overall average
    center <- centers[which.min(abs(rates - win_rate_overall))]
  }

  # Scale: measure how quickly win rate changes around center
  # Approximate: distance over which win rate goes from 25% to 75%
  # (or whatever range we can observe)
  low_idx  <- which(rates <= 0.30)
  high_idx <- which(rates >= 0.70)

  if (length(low_idx) > 0 && length(high_idx) > 0) {
    # Transition zone width
    transition_width <- abs(centers[min(high_idx)] - centers[max(low_idx)])
    # sigmoid goes from ~0.12 to ~0.88 over 4*scale, so 25%→75% is roughly 2*scale
    scale <- max(transition_width / 2, 0.01)
  } else {
    # Fallback: use IQR / 4 as scale estimate
    scale <- IQR(v, na.rm = TRUE) / 4
    if (scale == 0) scale <- sd(v, na.rm = TRUE) / 2
  }

  # Win rates by tercile for reporting
  terciles <- quantile(v, c(1/3, 2/3), na.rm = TRUE)
  t1 <- mean(wins[v <= terciles[1]])
  t2 <- mean(wins[v > terciles[1] & v <= terciles[2]])
  t3 <- mean(wins[v > terciles[2]])

  list(
    center = round(center, 3),
    scale  = round(scale, 3),
    n      = n,
    range  = round(range(v), 2),
    win_rate_overall = round(win_rate_overall * 100, 1),
    tercile_win_rates = sprintf("low=%.0f%% mid=%.0f%% high=%.0f%%",
      t1 * 100, t2 * 100, t3 * 100),
    note = "OK"
  )
}

# ── Step 5: Regime score analysis ─────────────────────────────────────────────
# Compute regime scores for each trade and compare win rates by dominant regime.

compute_regime_at_trade <- function(signals) {
  # Use REGIME_WEIGHTS from scenarios.R
  liq_weights <- list(
    vix_stress = 0.25, backwardation = 0.25, breadth_bear = 0.20,
    credit_stress = 0.20, vix_calm = -0.20, sentiment = -0.15
  )
  dir_weights <- list(
    breadth_bull = 0.20, vix_calm = 0.15, copper_gold = 0.15,
    sentiment = 0.20, credit_stress = -0.15, vix_stress = -0.15,
    backwardation = -0.10
  )

  score_regime <- function(weights) {
    s <- 0
    for (nm in names(weights)) {
      val <- signals[[nm]]
      if (!is.null(val) && !is.na(val)) {
        s <- s + weights[[nm]] * val
      }
    }
    s
  }

  liq_score <- score_regime(liq_weights)
  dir_score <- score_regime(dir_weights)

  # Neutral as residual (simplified — no breadth available)
  vix_calm <- signals$vix_calm %||% 0.5
  neutral_score <- 0.5 * (1 - max(liq_score, dir_score) / 0.5) + 0.3 * vix_calm + 0.2 * 0.5

  scores <- c(liquidity_stress = liq_score,
              directional_flow = dir_score,
              neutral = neutral_score)

  # Softmax
  exp_vals <- exp(scores / 2)
  probs <- exp_vals / sum(exp_vals)

  list(
    scores = scores,
    probs  = probs,
    dominant = names(which.max(probs))
  )
}

# ── Main ──────────────────────────────────────────────────────────────────────

run_backtest <- function() {
  conn <- dbConnect(SQLite(), DB_PATH)
  on.exit(dbDisconnect(conn), add = TRUE)

  # 1. Extract trades
  trades <- extract_bot_trades(conn)

  # 2. Fetch macro history
  from_date <- min(trades$entry_date) - 40  # extra for 20d returns
  to_date   <- max(trades$entry_date) + 1
  macro <- fetch_macro_history(from_date, to_date)

  if (nrow(macro) == 0) {
    stop("No macro data fetched. Check Yahoo connectivity.")
  }

  message(sprintf("\nMacro data: %d rows, %s to %s",
    nrow(macro), min(macro$date), max(macro$date)))
  message(sprintf("Tickers fetched: %s", paste(unique(macro$ticker), collapse = ", ")))

  # 3. Compute signals for each trade entry date
  message("\nComputing signals at each entry date...")
  results <- list()

  for (i in seq_len(nrow(trades))) {
    dt <- trades$entry_date[i]
    r <- compute_signals_at_date(dt, macro)

    row <- data.frame(
      TradeNr    = trades$TradeNr[i],
      entry_date = as.character(dt),
      exit_date  = as.character(trades$exit_date[i]),
      underlying = trades$underlying[i],
      total_pnl  = trades$total_pnl[i],
      outcome    = trades$outcome[i],
      hold_days  = trades$hold_days[i],
      # Raw inputs
      vix        = r$raw$vix,
      vix3m      = r$raw$vix3m,
      vix_ratio  = r$raw$ratio,
      y10        = r$raw$y10,
      dxy_ret    = r$raw$dxy_ret,
      tlt_ret    = r$raw$tlt_ret,
      hyg_ret    = r$raw$hyg_ret,
      uso_ret    = r$raw$uso_ret,
      gld_ret    = r$raw$gld_ret,
      copper_gold_ret = r$raw$copper_gold_ret,
      # Signals
      sig_vix_stress    = r$signals$vix_stress,
      sig_vix_calm      = r$signals$vix_calm,
      sig_backwardation = r$signals$backwardation,
      sig_rates_press   = r$signals$rates_press,
      sig_dxy_strength  = r$signals$dxy_strength,
      sig_reflation     = r$signals$reflation,
      sig_tlt_bid       = r$signals$tlt_bid,
      sig_credit_stress = r$signals$credit_stress,
      sig_copper_gold   = r$signals$copper_gold,
      sig_sentiment     = r$signals$sentiment,
      stringsAsFactors = FALSE
    )
    results[[i]] <- row
  }

  bt <- bind_rows(results)
  message(sprintf("Computed signals for %d trades", nrow(bt)))

  # 4. Compute regime at each entry
  message("Computing regime scores...")
  regime_results <- lapply(seq_len(nrow(bt)), function(i) {
    sigs <- list(
      vix_stress    = bt$sig_vix_stress[i],
      vix_calm      = bt$sig_vix_calm[i],
      backwardation = bt$sig_backwardation[i],
      rates_press   = bt$sig_rates_press[i],
      dxy_strength  = bt$sig_dxy_strength[i],
      reflation     = bt$sig_reflation[i],
      tlt_bid       = bt$sig_tlt_bid[i],
      credit_stress = bt$sig_credit_stress[i],
      copper_gold   = bt$sig_copper_gold[i],
      sentiment     = bt$sig_sentiment[i]
    )
    compute_regime_at_trade(sigs)
  })

  bt$regime_dominant <- vapply(regime_results, function(r) r$dominant, character(1))
  bt$prob_liq_stress <- vapply(regime_results, function(r) r$probs["liquidity_stress"], numeric(1))
  bt$prob_dir_flow   <- vapply(regime_results, function(r) r$probs["directional_flow"], numeric(1))
  bt$prob_neutral    <- vapply(regime_results, function(r) r$probs["neutral"], numeric(1))

  # 5. Save to DB
  tryCatch(dbExecute(conn, "DROP TABLE IF EXISTS backtest_regime_signals"), error = function(e) NULL)
  dbWriteTable(conn, "backtest_regime_signals", bt)
  message("Saved backtest_regime_signals to DB")

  # 6. Calibrate sigmoid parameters
  message("\n══════════════════════════════════════════════════════════")
  message("SIGMOID CALIBRATION RESULTS")
  message("══════════════════════════════════════════════════════════\n")

  # Map raw input columns to signal names
  calibration_map <- list(
    vix_stress    = list(col = "vix",             invert = FALSE),
    vix_calm      = list(col = "vix",             invert = TRUE),
    backwardation = list(col = "vix_ratio",       invert = TRUE,  transform = function(x) 1 - x),
    rates_press   = list(col = "y10",             invert = FALSE),
    dxy_strength  = list(col = "dxy_ret",         invert = FALSE),
    tlt_bid       = list(col = "tlt_ret",         invert = FALSE),
    credit_stress = list(col = "hyg_ret",         invert = TRUE,  transform = function(x) -x),
    copper_gold   = list(col = "copper_gold_ret", invert = FALSE)
  )

  calibrated <- list()
  report_lines <- c(
    "# Regime Backtest — Sigmoid Calibration Results",
    sprintf("\nDate: %s", Sys.Date()),
    sprintf("Strategy: %s | Trades: %d (W:%d L:%d) | Period: %s to %s",
      STRATEGY, nrow(bt),
      sum(bt$outcome == "WIN"), sum(bt$outcome == "LOSS"),
      min(bt$entry_date), max(bt$entry_date)),
    sprintf("Overall win rate: %.1f%%\n", mean(bt$outcome == "WIN") * 100),
    "## Signal Calibration\n",
    "| Signal | Current Center | Cal. Center | Current Scale | Cal. Scale | Tercile Win Rates | N | Note |",
    "|--------|---------------|-------------|---------------|------------|-------------------|---|------|"
  )

  for (sig_name in names(calibration_map)) {
    cm <- calibration_map[[sig_name]]
    vals <- bt[[cm$col]]

    # Apply transform if needed (e.g., for backwardation: 1-ratio, for credit: -hyg_ret)
    if (!is.null(cm$transform)) vals <- cm$transform(vals)

    cal <- calibrate_sigmoid(vals, bt$outcome, sig_name)
    calibrated[[sig_name]] <- cal

    cur <- CURRENT_PARAMS[[sig_name]]
    cur_c <- if (!is.null(cur)) cur$center else NA
    cur_s <- if (!is.null(cur)) cur$scale else NA

    line <- sprintf("| %s | %.2f | %s | %.2f | %s | %s | %d | %s |",
      sig_name,
      cur_c, if (is.na(cal$center)) "N/A" else sprintf("%.2f", cal$center),
      cur_s, if (is.na(cal$scale)) "N/A" else sprintf("%.2f", cal$scale),
      cal$tercile_win_rates, cal$n, cal$note)

    report_lines <- c(report_lines, line)
    message(sprintf("%-15s  current: center=%-6.2f scale=%-5.2f  →  calibrated: center=%-8s scale=%-8s  [%s]  n=%d",
      sig_name, cur_c, cur_s,
      if (is.na(cal$center)) "N/A" else sprintf("%.3f", cal$center),
      if (is.na(cal$scale)) "N/A" else sprintf("%.3f", cal$scale),
      cal$tercile_win_rates, cal$n))
  }

  # 7. Regime analysis
  message("\n══════════════════════════════════════════════════════════")
  message("REGIME PERFORMANCE ANALYSIS")
  message("══════════════════════════════════════════════════════════\n")

  regime_perf <- bt %>%
    group_by(regime_dominant) %>%
    summarise(
      n = n(),
      wins = sum(outcome == "WIN"),
      win_rate = round(mean(outcome == "WIN") * 100, 1),
      avg_pnl = round(mean(total_pnl), 2),
      total_pnl = round(sum(total_pnl), 2),
      avg_hold = round(mean(hold_days), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(win_rate))

  message("Regime         |  N  | Wins | Win% | Avg P&L | Total P&L | Avg Hold")
  message("---------------|-----|------|------|---------|-----------|----------")
  for (i in seq_len(nrow(regime_perf))) {
    r <- regime_perf[i,]
    message(sprintf("%-15s| %3d | %4d | %4.1f | %7.2f | %9.2f | %4.1f days",
      r$regime_dominant, r$n, r$wins, r$win_rate, r$avg_pnl, r$total_pnl, r$avg_hold))
  }

  report_lines <- c(report_lines, "",
    "## Regime Performance\n",
    "| Regime | N | Wins | Win% | Avg P&L | Total P&L | Avg Hold |",
    "|--------|---|------|------|---------|-----------|----------|")

  for (i in seq_len(nrow(regime_perf))) {
    r <- regime_perf[i,]
    report_lines <- c(report_lines,
      sprintf("| %s | %d | %d | %.1f%% | $%.2f | $%.2f | %.1f days |",
        r$regime_dominant, r$n, r$wins, r$win_rate, r$avg_pnl, r$total_pnl, r$avg_hold))
  }

  # 8. Probability band analysis (does higher directional_flow prob → better BOT results?)
  message("\n── Directional Flow probability bands ──")
  bt$dir_band <- cut(bt$prob_dir_flow,
    breaks = c(0, 0.30, 0.40, 0.50, 1),
    labels = c("<30%", "30-40%", "40-50%", ">50%"),
    include.lowest = TRUE)

  band_perf <- bt %>%
    group_by(dir_band) %>%
    summarise(
      n = n(),
      win_rate = round(mean(outcome == "WIN") * 100, 1),
      avg_pnl = round(mean(total_pnl), 2),
      .groups = "drop"
    )

  report_lines <- c(report_lines, "",
    "## Directional Flow Probability Bands\n",
    "| Band | N | Win% | Avg P&L |",
    "|------|---|------|---------|")

  for (i in seq_len(nrow(band_perf))) {
    b <- band_perf[i,]
    message(sprintf("  %-8s  n=%3d  win=%.1f%%  avg_pnl=$%.2f", b$dir_band, b$n, b$win_rate, b$avg_pnl))
    report_lines <- c(report_lines,
      sprintf("| %s | %d | %.1f%% | $%.2f |", b$dir_band, b$n, b$win_rate, b$avg_pnl))
  }

  # 9. Liquidity stress filter validation
  message("\n── Liquidity Stress filter (should BOT avoid high stress?) ──")
  bt$liq_band <- cut(bt$prob_liq_stress,
    breaks = c(0, 0.25, 0.35, 0.50, 1),
    labels = c("<25%", "25-35%", "35-50%", ">50%"),
    include.lowest = TRUE)

  liq_perf <- bt %>%
    group_by(liq_band) %>%
    summarise(
      n = n(),
      win_rate = round(mean(outcome == "WIN") * 100, 1),
      avg_pnl = round(mean(total_pnl), 2),
      .groups = "drop"
    )

  report_lines <- c(report_lines, "",
    "## Liquidity Stress Probability Bands\n",
    "| Band | N | Win% | Avg P&L |",
    "|------|---|------|---------|")

  for (i in seq_len(nrow(liq_perf))) {
    b <- liq_perf[i,]
    message(sprintf("  %-8s  n=%3d  win=%.1f%%  avg_pnl=$%.2f", b$liq_band, b$n, b$win_rate, b$avg_pnl))
    report_lines <- c(report_lines,
      sprintf("| %s | %d | %.1f%% | $%.2f |", b$liq_band, b$n, b$win_rate, b$avg_pnl))
  }

  # 10. Per-signal win rate by quintile (detailed)
  message("\n══════════════════════════════════════════════════════════")
  message("SIGNAL QUINTILE ANALYSIS")
  message("══════════════════════════════════════════════════════════\n")

  report_lines <- c(report_lines, "", "## Signal Quintile Analysis\n")

  sig_cols <- grep("^sig_", names(bt), value = TRUE)
  for (sc in sig_cols) {
    sig_label <- sub("^sig_", "", sc)
    vals <- bt[[sc]]
    valid <- !is.na(vals)
    if (sum(valid) < 10) next

    # Create quintile bins
    qt <- tryCatch(
      cut(vals[valid], breaks = quantile(vals[valid], probs = seq(0, 1, 0.2)),
          include.lowest = TRUE, labels = paste0("Q", 1:5)),
      error = function(e) NULL
    )
    if (is.null(qt)) next

    tmp <- data.frame(q = qt, outcome = bt$outcome[valid], pnl = bt$total_pnl[valid])
    q_perf <- tmp %>%
      group_by(q) %>%
      summarise(n = n(), win_rate = round(mean(outcome == "WIN") * 100, 1),
                avg_pnl = round(mean(pnl), 2), .groups = "drop")

    message(sprintf("%-18s", sig_label))
    report_lines <- c(report_lines, sprintf("### %s\n", sig_label),
      "| Quintile | N | Win% | Avg P&L |", "|----------|---|------|---------|")

    for (j in seq_len(nrow(q_perf))) {
      qr <- q_perf[j,]
      message(sprintf("  %-4s n=%3d  win=%5.1f%%  avg=$%8.2f", qr$q, qr$n, qr$win_rate, qr$avg_pnl))
      report_lines <- c(report_lines,
        sprintf("| %s | %d | %.1f%% | $%.2f |", qr$q, qr$n, qr$win_rate, qr$avg_pnl))
    }
    report_lines <- c(report_lines, "")
  }

  # 11. Regime weight optimization (simple grid search)
  message("\n══════════════════════════════════════════════════════════")
  message("REGIME WEIGHT SENSITIVITY")
  message("══════════════════════════════════════════════════════════\n")

  # Test: which signals best predict BOT success?
  # Correlation of each signal with win/loss
  report_lines <- c(report_lines, "## Signal Predictive Power\n",
    "| Signal | Corr with Win | Avg (Win) | Avg (Loss) | Diff |",
    "|--------|--------------|-----------|------------|------|")

  win_flag <- as.numeric(bt$outcome == "WIN")
  for (sc in sig_cols) {
    sig_label <- sub("^sig_", "", sc)
    vals <- bt[[sc]]
    valid <- !is.na(vals) & !is.na(win_flag)
    if (sum(valid) < 10) next

    corr <- cor(vals[valid], win_flag[valid])
    avg_w <- mean(vals[valid & bt$outcome == "WIN"], na.rm = TRUE)
    avg_l <- mean(vals[valid & bt$outcome == "LOSS"], na.rm = TRUE)

    message(sprintf("  %-18s  corr=%+.3f  avg_win=%.3f  avg_loss=%.3f  diff=%+.3f",
      sig_label, corr, avg_w, avg_l, avg_w - avg_l))
    report_lines <- c(report_lines,
      sprintf("| %s | %+.3f | %.3f | %.3f | %+.3f |",
        sig_label, corr, avg_w, avg_l, avg_w - avg_l))
  }

  # 12. Limitations note
  report_lines <- c(report_lines, "",
    "## Limitations\n",
    "- **No S5FI (breadth) data**: breadth_bull and breadth_bear signals could not be calibrated.",
    "  The breadth calculation requires live scraping of 500+ tickers and is not available historically.",
    "  Proxy: consider using RSP/SPY relative performance or MMFI index if available.",
    "- **No COT/positioning data**: positioning stress modifier not backtested.",
    "- **No catalyst/event data**: catalyst boost not backtested.",
    "- **Regime inertia**: not applied in backtest (no sequential regime state).",
    "- **Sample size**: 98 trades is small for 10 parameter calibrations. Treat results as directional, not definitive.",
    "- **Survivorship**: only closed trades included; open trades excluded."
  )

  # Write report
  report_path <- file.path(OUT_DIR, "backtest_results.md")
  writeLines(report_lines, report_path)
  message(sprintf("\n✓ Report written to %s", report_path))

  # Return for interactive use
  invisible(list(
    trades = bt,
    calibrated = calibrated,
    regime_perf = regime_perf,
    band_perf = band_perf,
    macro = macro
  ))
}

# ── Run ───────────────────────────────────────────────────────────────────────
if (interactive() || !is.null(sys.calls())) {
  message("Run with: result <- run_backtest()")
} else {
  result <- run_backtest()
}
