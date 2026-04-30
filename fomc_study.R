# ============================================================
# Hypothesis A — FOMC vol-surface dynamics
# ============================================================
# Mechanism (Lucca-Moench, dealer-positioning literature):
#   Pre-FOMC (t-5 .. t-1): IV30 climbs (event-mode hedge bid),
#                          term structure compresses, put skew steepens.
#   Post-FOMC (t+0 .. t+5): IV crush, surface normalises, dealer hedges unwind.
#
# Two specific tests, both ATM IV30 and 25Δ Put Skew:
#   (1) Pre-FOMC peak  — max excess in t-5..t-1, p=Pr(boot peak >= obs peak)
#   (2) Post-FOMC trough — min excess in t+0..t+5, p=Pr(boot trough <= obs trough)
#
# Path study runs from t=-5 to t=+10; baseline anchor at t=0 (announcement day).

SUPPRESS_PANEL <- TRUE
source("C:/Users/aldoh/Documents/RApplication/RStudies/IVvsSpotLag.R", echo = FALSE)

FOMC_DATES <- as.Date(c(
  "2021-01-27","2021-03-17","2021-04-28","2021-06-16","2021-07-28",
  "2021-09-22","2021-11-03","2021-12-15",
  "2022-01-26","2022-03-16","2022-05-04","2022-06-15","2022-07-27",
  "2022-09-21","2022-11-02","2022-12-14",
  "2023-02-01","2023-03-22","2023-05-03","2023-06-14","2023-07-26",
  "2023-09-20","2023-11-01","2023-12-13",
  "2024-01-31","2024-03-20","2024-05-01","2024-06-12","2024-07-31",
  "2024-09-18","2024-11-07","2024-12-18",
  "2025-01-29","2025-03-19","2025-05-07","2025-06-18","2025-07-30",
  "2025-09-17","2025-10-29","2025-12-10",
  "2026-01-28","2026-03-18"
))

FOMC_HORIZONS <- -5:10
FOMC_PAD      <- 7         # exclude controls within ±7 days of any FOMC
N_BOOT_FOMC   <- 1000

run_fomc_study <- function(ticker, horizons = FOMC_HORIZONS,
                           excl_pad = FOMC_PAD, n_boot = N_BOOT_FOMC,
                           verbose = TRUE) {
  if (verbose) cat("\n========== ", ticker, "  (FOMC, h=", min(horizons),
                   "..", max(horizons), ") ==========\n", sep = "")

  d <- file.path(OUT_BASE, ticker)
  sp    <- as.data.table(arrow::read_parquet(file.path(d, "stock_prices.parquet")))
  ivx   <- as.data.table(arrow::read_parquet(file.path(d, "ivx.parquet")))
  ivs25 <- as.data.table(arrow::read_parquet(file.path(d, "ivs_25delta_30d_wide.parquet")))
  for (x in list(sp, ivx, ivs25)) x[, date := as.Date(date)]

  panel <- sp[, .(date, spot = close)] |>
    merge(ivx[, .(date,
                  iv30 = `30d IV Mean`,
                  iv90 = `90d IV Mean`,
                  iv7  = `7d IV Mean`)],   by = "date") |>
    merge(ivs25[, .(date, c25, p25)],     by = "date")
  setorder(panel, date)
  panel[, atm        := iv30]
  panel[, skew_put   := p25 - iv30]   # put skew (richer puts = larger value)
  panel[, skew_call  := c25 - iv30]
  panel[, rr25       := c25 - p25]
  panel[, ts_slope   := iv90 - iv30]  # term-structure slope; negative = inverted

  # FOMC anchor = first trading day on/after the announcement date
  fomc_idx <- vapply(FOMC_DATES, \(e) {
    cands <- which(panel$date >= e); if (!length(cands)) NA_integer_ else cands[1]
  }, integer(1))
  fomc_idx <- fomc_idx[!is.na(fomc_idx)]
  panel[, signal_event := FALSE][fomc_idx, signal_event := TRUE]

  # Control eligibility: not within ±excl_pad of any FOMC, and not an FOMC itself
  near_fomc <- unique(unlist(lapply(FOMC_DATES, \(d) seq(d - excl_pad, d + excl_pad, by = "day"))))
  panel[, near_fomc := date %in% as.Date(near_fomc)]

  set.seed(42)
  hi <- max(horizons); lo <- min(horizons)
  eligible <- which(!panel$near_fomc & !panel$signal_event &
                    seq_len(nrow(panel)) + hi <= nrow(panel) &
                    seq_len(nrow(panel)) + lo >= 1)
  if (length(eligible) < length(fomc_idx)) {
    if (verbose) cat("  not enough eligible control days; skipping\n"); return(NULL)
  }
  rand <- sample(eligible, length(fomc_idx))
  panel[, signal_random := FALSE][rand, signal_random := TRUE]

  vars <- c("atm", "skew_put", "skew_call", "rr25", "ts_slope")
  psig  <- setNames(lapply(vars, \(v) build_paths(panel, v, "signal_event",  horizons)), vars)
  pctrl <- setNames(lapply(vars, \(v) build_paths(panel, v, "signal_random", horizons)), vars)
  if (any(sapply(psig, is.null)) || any(sapply(pctrl, is.null))) {
    if (verbose) cat("  build_paths returned NULL; skipping\n"); return(NULL)
  }

  excess <- data.table(
    horizon       = horizons,
    atm_excess    = psig$atm$mean       - pctrl$atm$mean,
    putSkew_excess  = psig$skew_put$mean  - pctrl$skew_put$mean,
    callSkew_excess = psig$skew_call$mean - pctrl$skew_call$mean,
    rr25_excess    = psig$rr25$mean      - pctrl$rr25$mean,
    tsSlope_excess = psig$ts_slope$mean  - pctrl$ts_slope$mean
  )

  pre_idx  <- excess$horizon < 0
  post_idx <- excess$horizon > 0

  # Pre-FOMC peak (max ATM rise in days t-5..t-1)
  obs_pre_atm  <- max(excess$atm_excess[pre_idx])
  obs_pre_h    <- excess$horizon[pre_idx][which.max(excess$atm_excess[pre_idx])]
  # Post-FOMC trough (min ATM excess in days t+1..t+5 — IV crush)
  obs_post_atm <- min(excess$atm_excess[post_idx])
  obs_post_h   <- excess$horizon[post_idx][which.min(excess$atm_excess[post_idx])]

  # Bootstrap both
  boot_pre  <- numeric(n_boot)
  boot_post <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    ri <- sample(eligible, length(fomc_idx))
    panel[, signal_boot := FALSE][ri, signal_boot := TRUE]
    bp <- build_paths(panel, "atm", "signal_boot", horizons)
    if (is.null(bp)) { boot_pre[b]  <- NA; boot_post[b] <- NA; next }
    diff <- bp$mean - pctrl$atm$mean
    boot_pre[b]  <- max(diff[pre_idx])
    boot_post[b] <- min(diff[post_idx])
  }
  p_pre  <- mean(boot_pre  >= obs_pre_atm,  na.rm = TRUE)
  p_post <- mean(boot_post <= obs_post_atm, na.rm = TRUE)

  if (verbose) {
    print(excess)
    cat(sprintf("Pre-FOMC ATM peak:  d%d  %+.4f vp  (boot p=%.3f)\n",
                obs_pre_h,  obs_pre_atm,  p_pre))
    cat(sprintf("Post-FOMC ATM trough: d%d  %+.4f vp  (boot p=%.3f)\n",
                obs_post_h, obs_post_atm, p_post))
  }

  list(ticker = ticker, n_events = length(fomc_idx),
       excess = excess,
       pre_atm_peak  = obs_pre_atm,  pre_atm_h  = obs_pre_h,  p_pre  = p_pre,
       post_atm_trough = obs_post_atm, post_atm_h = obs_post_h, p_post = p_post)
}

# Panel runner — index/sector ETFs only
TICKERS <- c("SPY","QQQ","IWM","XLK","XLE","XLF","XLV","XLU")
available <- intersect(TICKERS, list.files(OUT_BASE))
cat("FOMC study on tickers: ", paste(available, collapse=", "), "\n", sep="")

results <- lapply(available, run_fomc_study)
summary_dt <- rbindlist(lapply(results, function(r) {
  if (is.null(r)) return(NULL)
  data.table(
    ticker        = r$ticker,
    n_events      = r$n_events,
    pre_peak_d    = r$pre_atm_h,
    pre_peak_val  = round(r$pre_atm_peak, 4),
    pre_p_value   = round(r$p_pre, 3),
    post_trough_d = r$post_atm_h,
    post_trough_val = round(r$post_atm_trough, 4),
    post_p_value  = round(r$p_post, 3)
  )
}))

cat("\n============================================================\n")
cat("FOMC ATM-IV30 SUMMARY\n")
cat("============================================================\n")
print(summary_dt)

saveRDS(list(results = results, summary = summary_dt), file.path(OUT_BASE, "fomc_results.rds"))
fwrite(summary_dt, file.path(OUT_BASE, "fomc_summary.csv"))
cat("\nSaved -> fomc_results.rds, fomc_summary.csv\n")
