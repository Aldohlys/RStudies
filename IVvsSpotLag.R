library(data.table)
library(ggplot2)
library(arrow)

# ============================================================
# Multi-ticker IV-vs-Spot lag event study
# ============================================================
# Hypothesis: after strong upside spot momentum (top decile 10d return),
# retail call buying lifts IV30 / call skew / RR25 with a lag of a few days.
# Method: signal-vs-control event study, bootstrap on RR25 peak.
# Data:    IVolatility EOD (Backtest API Plus trial); written to
#          ivol_probe_out/<TICKER>/{stock_prices,ivx,ivs_25delta_30d_wide}.parquet
# ============================================================

OUT_BASE  <- "C:/Users/aldoh/Documents/RApplication/RStudies/ivol_probe_out"
HORIZONS  <- 0:15
LOOKBACK  <- 10        # momentum window (trading days)
DECILE_Q  <- 0.90      # signal threshold quantile on mom<LOOKBACK>
EARN_PAD  <- 14        # exclude ±14 calendar days around earnings
N_BOOT    <- 1000

# Approximate quarterly earnings dates — sourced from public IR pages.
# AAPL exact; WMT/TGT/DOW best-effort. ±14 calendar-day window forgives ±few days.
EARNINGS <- list(
  AAPL = as.Date(c(
    "2021-01-27","2021-04-28","2021-07-27","2021-10-28",
    "2022-01-27","2022-04-28","2022-07-28","2022-10-27",
    "2023-02-02","2023-05-04","2023-08-03","2023-11-02",
    "2024-02-01","2024-05-02","2024-08-01","2024-10-31",
    "2025-01-30","2025-05-01","2025-07-31","2025-10-30",
    "2026-01-29")),
  WMT  = as.Date(c(
    "2021-02-18","2021-05-18","2021-08-17","2021-11-16",
    "2022-02-17","2022-05-17","2022-08-16","2022-11-15",
    "2023-02-21","2023-05-18","2023-08-17","2023-11-16",
    "2024-02-20","2024-05-16","2024-08-15","2024-11-19",
    "2025-02-20","2025-05-15","2025-08-21","2025-11-20",
    "2026-02-19")),
  TGT  = as.Date(c(
    "2021-03-02","2021-05-19","2021-08-18","2021-11-17",
    "2022-03-01","2022-05-18","2022-08-17","2022-11-16",
    "2023-02-28","2023-05-17","2023-08-16","2023-11-15",
    "2024-03-05","2024-05-22","2024-08-21","2024-11-20",
    "2025-03-04","2025-05-21","2025-08-20","2025-11-19",
    "2026-03-03")),
  DOW  = as.Date(c(
    "2021-01-28","2021-04-22","2021-07-22","2021-10-21",
    "2022-01-27","2022-04-21","2022-07-21","2022-10-20",
    "2023-01-26","2023-04-25","2023-07-25","2023-10-24",
    "2024-01-25","2024-04-25","2024-07-25","2024-10-24",
    "2025-01-30","2025-04-24","2025-07-24","2025-10-23",
    "2026-01-29","2026-04-23")),
  GOLD = as.Date(c(
    "2021-02-09","2021-05-12","2021-09-09","2021-11-04",
    "2022-02-08","2022-05-05","2022-08-30","2022-11-08",
    "2023-02-06","2023-05-09","2023-08-31","2023-11-07",
    "2024-02-06","2024-05-07","2024-08-29","2024-11-06",
    "2025-02-06","2025-05-07","2025-09-09","2025-11-06",
    "2026-02-05")),
  XOM  = as.Date(c(
    "2021-02-02","2021-04-30","2021-07-30","2021-10-29",
    "2022-02-01","2022-04-29","2022-07-29","2022-10-28",
    "2023-01-31","2023-04-28","2023-07-28","2023-10-27",
    "2024-02-02","2024-04-26","2024-08-02","2024-11-01",
    "2025-01-31","2025-05-02","2025-08-01","2025-10-31",
    "2026-01-30")),
  JNJ  = as.Date(c(
    "2021-01-26","2021-04-20","2021-07-21","2021-10-19",
    "2022-01-25","2022-04-19","2022-07-19","2022-10-18",
    "2023-01-24","2023-04-18","2023-07-20","2023-10-17",
    "2024-01-23","2024-04-16","2024-07-17","2024-10-15",
    "2025-01-22","2025-04-15","2025-07-16","2025-10-14",
    "2026-01-21","2026-04-14")),
  OXY  = as.Date(c(
    "2021-02-22","2021-05-10","2021-08-03","2021-11-04",
    "2022-02-24","2022-05-10","2022-08-02","2022-11-08",
    "2023-02-27","2023-05-09","2023-08-02","2023-11-07",
    "2024-02-14","2024-05-07","2024-08-07","2024-11-12",
    "2025-02-18","2025-05-07","2025-08-06","2025-11-10",
    "2026-02-18")),
  NFLX = as.Date(c(
    "2021-01-19","2021-04-20","2021-07-20","2021-10-19",
    "2022-01-20","2022-04-19","2022-07-19","2022-10-18",
    "2023-01-19","2023-04-18","2023-07-19","2023-10-18",
    "2024-01-23","2024-04-18","2024-07-18","2024-10-17",
    "2025-01-21","2025-04-17","2025-07-17","2025-10-21",
    "2026-01-20","2026-04-16")),
  # Index/sector ETFs — no individual earnings; included so panel runners
  # iterate over them. Empty vector means no near_earnings exclusion applied.
  SPY = as.Date(character(0)),
  QQQ = as.Date(character(0)),
  IWM = as.Date(character(0)),
  XLK = as.Date(character(0)),
  XLE = as.Date(character(0)),
  XLF = as.Date(character(0)),
  XLV = as.Date(character(0)),
  XLU = as.Date(character(0))
)

# ----- helpers --------------------------------------------------------------

load_ticker <- function(ticker, lookback = LOOKBACK, decile_q = DECILE_Q, earn_pad = EARN_PAD) {
  d <- file.path(OUT_BASE, ticker)
  sp    <- as.data.table(arrow::read_parquet(file.path(d, "stock_prices.parquet")))
  ivx   <- as.data.table(arrow::read_parquet(file.path(d, "ivx.parquet")))
  ivs25 <- as.data.table(arrow::read_parquet(file.path(d, "ivs_25delta_30d_wide.parquet")))
  for (x in list(sp, ivx, ivs25)) x[, date := as.Date(date)]

  panel <- sp[, .(date, spot = close)] |>
    merge(ivx[, .(date, atm = `30d IV Mean`)], by = "date") |>
    merge(ivs25[, .(date, c25, p25)],          by = "date")
  setorder(panel, date)

  panel[, ret1      := c(NA, diff(log(spot)))]
  panel[, mom      := spot / shift(spot, lookback) - 1]
  panel[, rv20      := frollapply(ret1, 20, sd) * sqrt(252)]
  panel[, skew_call := c25 - atm]
  panel[, skew_put  := p25 - atm]
  panel[, rr25      := c25 - p25]

  earn <- EARNINGS[[ticker]]
  excl <- if (length(earn)) unique(unlist(lapply(earn, \(d) seq(d - earn_pad, d + earn_pad, by = "day")))) else as.Date(integer(0))
  panel[, near_earnings := date %in% as.Date(excl)]

  thr <- quantile(panel$mom, decile_q, na.rm = TRUE)
  panel[, signal_up := mom > thr & !near_earnings & is.finite(mom)]
  panel
}

build_paths <- function(df, var, signal_col, horizons) {
  event_idx <- which(df[[signal_col]])
  # Allow negative horizons (pre-event window) — event must be far enough from both panel ends
  lo <- min(horizons); hi <- max(horizons)
  event_idx <- event_idx[event_idx + hi <= nrow(df) & event_idx + lo >= 1]
  if (!length(event_idx)) return(NULL)
  paths <- sapply(event_idx, function(i) df[[var]][i + horizons] - df[[var]][i])
  if (is.null(dim(paths))) paths <- matrix(paths, ncol = 1)
  data.table(
    horizon = horizons,
    mean    = rowMeans(paths, na.rm = TRUE),
    se      = apply(paths, 1, sd, na.rm = TRUE) / sqrt(rowSums(!is.na(paths))),
    median  = apply(paths, 1, median, na.rm = TRUE),
    n       = rowSums(!is.na(paths))
  )
}

run_study <- function(ticker, n_boot = N_BOOT, verbose = TRUE,
                      lookback = LOOKBACK, decile_q = DECILE_Q,
                      horizons = HORIZONS, earn_pad = EARN_PAD) {
  if (verbose) cat("\n========== ", ticker, " (LB=", lookback, " Q=", decile_q,
                   " H=0:", max(horizons), ") ==========\n", sep = "")
  df <- load_ticker(ticker, lookback = lookback, decile_q = decile_q, earn_pad = earn_pad)
  n_events <- sum(df$signal_up, na.rm = TRUE)
  if (verbose) cat("rows:", nrow(df), " | up-signal events:", n_events,
                   " | sample:", as.character(df$date[1]), "..", as.character(df$date[nrow(df)]), "\n")
  if (n_events < 20) {
    if (verbose) cat("  too few events; skipping\n")
    return(NULL)
  }

  set.seed(42)
  eligible <- which(!df$signal_up & !df$near_earnings & is.finite(df$mom) &
                    seq_len(nrow(df)) + max(horizons) <= nrow(df))
  rand <- sample(eligible, n_events)
  df[, signal_random := FALSE][rand, signal_random := TRUE]

  paths_sig  <- lapply(c("atm", "skew_call", "rr25"), \(v) build_paths(df, v, "signal_up",     horizons))
  paths_ctrl <- lapply(c("atm", "skew_call", "rr25"), \(v) build_paths(df, v, "signal_random", horizons))
  names(paths_sig) <- names(paths_ctrl) <- c("atm", "skewC", "rr25")

  excess <- data.table(
    horizon      = horizons,
    atm_excess   = paths_sig$atm$mean   - paths_ctrl$atm$mean,
    skewC_excess = paths_sig$skewC$mean - paths_ctrl$skewC$mean,
    rr25_excess  = paths_sig$rr25$mean  - paths_ctrl$rr25$mean
  )

  peak_atm   <- excess[which.max(atm_excess)]
  peak_skewC <- excess[which.max(skewC_excess)]
  peak_rr25  <- excess[which.max(rr25_excess)]

  # Bootstrap RR25 peak
  boot <- replicate(n_boot, {
    ri <- sample(eligible, n_events)
    df[, signal_boot := FALSE][ri, signal_boot := TRUE]
    bp <- build_paths(df, "rr25", "signal_boot", horizons)
    if (is.null(bp)) NA_real_ else max(bp$mean - paths_ctrl$rr25$mean)
  })
  p_rr25 <- mean(boot >= peak_rr25$rr25_excess, na.rm = TRUE)

  if (verbose) {
    print(excess)
    cat(sprintf("Peak ATM @ d%d  +%.4f\n",   peak_atm$horizon,   peak_atm$atm_excess))
    cat(sprintf("Peak SkewC @ d%d  +%.4f\n", peak_skewC$horizon, peak_skewC$skewC_excess))
    cat(sprintf("Peak RR25 @ d%d  +%.4f  (bootstrap p=%.3f)\n",
                peak_rr25$horizon, peak_rr25$rr25_excess, p_rr25))
  }

  list(
    ticker     = ticker,
    n_events   = n_events,
    excess     = excess,
    peak_atm   = peak_atm,
    peak_skewC = peak_skewC,
    peak_rr25  = peak_rr25,
    p_rr25     = p_rr25
  )
}

summarise <- function(results) {
  rbindlist(lapply(results, function(r) {
    if (is.null(r)) return(NULL)
    data.table(
      ticker        = r$ticker,
      n_events      = r$n_events,
      atm_peak_d    = r$peak_atm$horizon,
      atm_peak_val  = round(r$peak_atm$atm_excess, 4),
      skewC_peak_d  = r$peak_skewC$horizon,
      skewC_peak_val= round(r$peak_skewC$skewC_excess, 4),
      rr25_peak_d   = r$peak_rr25$horizon,
      rr25_peak_val = round(r$peak_rr25$rr25_excess, 4),
      rr25_p_value  = round(r$p_rr25, 3)
    )
  }))
}

# ----- run all available tickers (skipped when sourced from another script) -

if (!exists("SUPPRESS_PANEL")) {
  available <- list.files(OUT_BASE, full.names = FALSE)
  available <- available[available %in% names(EARNINGS)]
  cat("Will run:", paste(available, collapse = ", "), "\n")

  results <- lapply(available, run_study)
  summary_dt <- summarise(results)
  cat("\n============================================================\n")
  cat("SUMMARY\n")
  cat("============================================================\n")
  print(summary_dt)

  saveRDS(list(results = results, summary = summary_dt),
          file.path(OUT_BASE, "study_results.rds"))
  fwrite(summary_dt, file.path(OUT_BASE, "study_summary.csv"))
  cat("\nSaved -> study_results.rds, study_summary.csv\n")
}
