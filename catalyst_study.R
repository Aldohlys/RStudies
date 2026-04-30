# ============================================================
# Variant A — Post-earnings positive-surprise drift
# ============================================================
# Hypothesis: after an earnings announcement that gaps the stock up >= GAP_MIN,
# retail call-buying drives RR25 / call skew up over the following days.
#
# Difference vs IVvsSpotLag.R:
#   - Events are *real news shocks*, not noisy momentum.
#   - We don't exclude earnings from the panel — they are the events.
#   - Anchor t=0 at announcement_day + ANCHOR_OFFSET (1 by default) so the
#     IV-crush mechanics on the announcement day don't dominate paths.
#   - Control: random non-earnings, non-event days (matched by count).

SUPPRESS_PANEL <- TRUE
source("C:/Users/aldoh/Documents/RApplication/RStudies/IVvsSpotLag.R", echo = FALSE)

GAP_MIN        <- 0.03      # +3% surprise threshold (was 0.05; relaxed for mega-cap n)
ANCHOR_OFFSET  <- 1         # study starts at announcement_day + 1
HORIZONS_CAT   <- 0:20      # 20 trading days post-anchor
EXCL_PAD_CAT   <- 14        # ±14d earnings exclusion for CONTROL eligibility

run_earnings_study <- function(ticker, gap_min = GAP_MIN,
                               horizons = HORIZONS_CAT,
                               anchor_offset = ANCHOR_OFFSET,
                               excl_pad = EXCL_PAD_CAT,
                               n_boot = 1000, verbose = TRUE) {
  if (verbose) cat("\n========== ", ticker, "  gap >= ", round(gap_min*100,1), "%  ==========\n", sep = "")

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
  panel[, skew_call := c25 - atm]
  panel[, rr25      := c25 - p25]

  earn <- EARNINGS[[ticker]]
  if (is.null(earn) || !length(earn)) {
    if (verbose) cat("  no earnings dates\n"); return(NULL)
  }

  # Map calendar earnings dates to first panel trading day on/after each event
  earn_idx <- vapply(earn, function(e) {
    cands <- which(panel$date >= e)
    if (!length(cands)) NA_integer_ else cands[1]
  }, integer(1))
  earn_idx <- earn_idx[!is.na(earn_idx) & earn_idx >= 2 & earn_idx + 1 <= nrow(panel)]

  # 2-day log return spanning the announcement (handles BMO and AMC)
  gaps <- log(panel$spot[earn_idx + 1] / panel$spot[earn_idx - 1])
  qual <- earn_idx[gaps >= gap_min]
  qual_gaps <- gaps[gaps >= gap_min]

  if (verbose) {
    cat(sprintf("  earnings in panel: %d  |  with gap >= %.0f%%: %d  (mean=%.2f%% max=%.2f%%)\n",
                length(earn_idx), gap_min*100, length(qual),
                if (length(qual_gaps)) mean(qual_gaps)*100 else NA_real_,
                if (length(qual_gaps)) max(qual_gaps)*100 else NA_real_))
  }

  event_idx <- qual + anchor_offset
  event_idx <- event_idx[event_idx + max(horizons) <= nrow(panel)]
  if (length(event_idx) < 5) {
    if (verbose) cat("  fewer than 5 events; skipping\n"); return(NULL)
  }

  panel[, signal_event := FALSE][event_idx, signal_event := TRUE]

  # Control eligibility: not in any earnings window, not an event itself
  excl_dates <- unique(unlist(lapply(earn, \(d) seq(d - excl_pad, d + excl_pad, by = "day"))))
  panel[, near_earnings := date %in% as.Date(excl_dates)]
  set.seed(42)
  eligible <- which(!panel$near_earnings & !panel$signal_event &
                    seq_len(nrow(panel)) + max(horizons) <= nrow(panel))
  if (length(eligible) < length(event_idx)) return(NULL)
  rand <- sample(eligible, length(event_idx))
  panel[, signal_random := FALSE][rand, signal_random := TRUE]

  vars  <- c("atm", "skew_call", "rr25")
  psig  <- setNames(lapply(vars, \(v) build_paths(panel, v, "signal_event",  horizons)), c("atm","skewC","rr25"))
  pctrl <- setNames(lapply(vars, \(v) build_paths(panel, v, "signal_random", horizons)), c("atm","skewC","rr25"))

  excess <- data.table(
    horizon      = horizons,
    atm_excess   = psig$atm$mean   - pctrl$atm$mean,
    skewC_excess = psig$skewC$mean - pctrl$skewC$mean,
    rr25_excess  = psig$rr25$mean  - pctrl$rr25$mean
  )
  peak_atm   <- excess[which.max(atm_excess)]
  peak_skewC <- excess[which.max(skewC_excess)]
  peak_rr25  <- excess[which.max(rr25_excess)]

  boot <- replicate(n_boot, {
    ri <- sample(eligible, length(event_idx))
    panel[, signal_boot := FALSE][ri, signal_boot := TRUE]
    bp <- build_paths(panel, "rr25", "signal_boot", horizons)
    if (is.null(bp)) NA_real_ else max(bp$mean - pctrl$rr25$mean)
  })
  p_rr25 <- mean(boot >= peak_rr25$rr25_excess, na.rm = TRUE)

  if (verbose) {
    cat(sprintf("Peak ATM @ d%d  +%.4f\n",   peak_atm$horizon,   peak_atm$atm_excess))
    cat(sprintf("Peak SkewC @ d%d  +%.4f\n", peak_skewC$horizon, peak_skewC$skewC_excess))
    cat(sprintf("Peak RR25 @ d%d  +%.4f  (bootstrap p=%.3f)\n",
                peak_rr25$horizon, peak_rr25$rr25_excess, p_rr25))
  }

  list(
    ticker = ticker, n_events = length(event_idx),
    mean_gap = if (length(qual_gaps)) mean(qual_gaps) else NA_real_,
    excess = excess,
    peak_atm = peak_atm, peak_skewC = peak_skewC, peak_rr25 = peak_rr25,
    p_rr25 = p_rr25
  )
}

# ----- panel runner ---------------------------------------------------------

available <- list.files(OUT_BASE, full.names = FALSE)
available <- available[available %in% names(EARNINGS)]
cat("Catalyst study (post-earnings gap >= ", GAP_MIN*100, "%) on tickers: ",
    paste(available, collapse = ", "), "\n", sep = "")

results <- lapply(available, run_earnings_study)

summary_dt <- rbindlist(lapply(results, function(r) {
  if (is.null(r)) return(NULL)
  data.table(
    ticker        = r$ticker,
    n_events      = r$n_events,
    mean_gap_pct  = round(r$mean_gap * 100, 2),
    atm_peak_d    = r$peak_atm$horizon,
    atm_peak_val  = round(r$peak_atm$atm_excess, 4),
    skewC_peak_d  = r$peak_skewC$horizon,
    skewC_peak_val= round(r$peak_skewC$skewC_excess, 4),
    rr25_peak_d   = r$peak_rr25$horizon,
    rr25_peak_val = round(r$peak_rr25$rr25_excess, 4),
    rr25_p_value  = round(r$p_rr25, 3)
  )
}))

cat("\n============================================================\n")
cat("CATALYST SUMMARY (gap >= ", GAP_MIN*100, "%)\n", sep = "")
cat("============================================================\n")
print(summary_dt)

saveRDS(list(results = results, summary = summary_dt, gap_min = GAP_MIN),
        file.path(OUT_BASE, "catalyst_results.rds"))
fwrite(summary_dt, file.path(OUT_BASE, "catalyst_summary.csv"))
cat("\nSaved -> catalyst_results.rds, catalyst_summary.csv\n")
