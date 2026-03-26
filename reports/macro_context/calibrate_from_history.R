# calibrate_from_history.R — Derive sigmoid center/scale from market history
#
# Approach: center = median, scale = SD of each indicator over 10 years.
# This makes sigmoid output interpretable:
#   0.5 = historically normal
#   >0.85 = rare/extreme (top ~15%)
#   <0.15 = rare/extreme (bottom ~15%)
#
# Usage: source("calibrate_from_history.R") from RStudio or Rscript

library(Tdata)
library(dplyr)

if (!exists("SCRIPT_DIR")) {
  SCRIPT_DIR <- tryCatch(
    dirname(rstudioapi::getActiveDocumentContext()$path),
    error = function(e) {
      args <- commandArgs(trailingOnly = FALSE)
      file_arg <- grep("--file=", args, value = TRUE)
      if (length(file_arg) > 0) dirname(normalizePath(sub("--file=", "", file_arg[1])))
      else getwd()
    }
  )
}

# ── Fetch historical data ─────────────────────────────────────────────────────
message("Fetching 10 years of historical data...")

from_date <- Sys.Date() - 365 * 10
to_date   <- Sys.Date()

tickers <- c("^VIX", "^VIX3M", "^TNX", "DX-Y.NYB", "TLT", "HYG", "CPER", "GLD", "USO")

raw <- list()
for (tk in tickers) {
  message(sprintf("  Fetching %s ...", tk))
  tryCatch({
    d <- getYahooData(tickers = tk, from_date = from_date, to_date = to_date)
    if (!is.null(d) && nrow(d) > 0) {
      raw[[tk]] <- d |> arrange(date) |> filter(!is.na(Close))
      message(sprintf("    → %d rows (%s to %s)", nrow(raw[[tk]]),
                       min(raw[[tk]]$date), max(raw[[tk]]$date)))
    }
  }, error = function(e) message(sprintf("    FAILED: %s", e$message)))
}

# ── Compute derived series ────────────────────────────────────────────────────
message("\nComputing derived indicators...")

# Helper: rolling 20-day return (%)
rolling_ret20 <- function(prices) {
  n <- length(prices)
  if (n < 21) return(rep(NA_real_, n))
  ret <- rep(NA_real_, n)
  for (i in 21:n) {
    ret[i] <- (prices[i] / prices[i - 20] - 1) * 100
  }
  ret
}

indicators <- list()

# 1. VIX spot (level, not return)
if ("^VIX" %in% names(raw)) {
  indicators$vix <- raw[["^VIX"]]$Close
  message(sprintf("  vix: %d values", sum(!is.na(indicators$vix))))
}

# 2. VIX/VIX3M ratio → 1 - ratio (backwardation measure)
if (all(c("^VIX", "^VIX3M") %in% names(raw))) {
  vix_df  <- raw[["^VIX"]] |> select(date, vix = Close)
  v3m_df  <- raw[["^VIX3M"]] |> select(date, v3m = Close)
  merged  <- inner_join(vix_df, v3m_df, by = "date") |> filter(!is.na(vix), !is.na(v3m), v3m > 0)
  indicators$backwardation <- 1 - merged$vix / merged$v3m
  message(sprintf("  backwardation (1 - VIX/VIX3M): %d values", length(indicators$backwardation)))
}

# 3. 10Y yield (level)
if ("^TNX" %in% names(raw)) {
  indicators$y10 <- raw[["^TNX"]]$Close
  message(sprintf("  y10: %d values", sum(!is.na(indicators$y10))))
}

# 4. DXY 20d return
if ("DX-Y.NYB" %in% names(raw)) {
  indicators$dxy_ret20 <- rolling_ret20(raw[["DX-Y.NYB"]]$Close)
  indicators$dxy_ret20 <- indicators$dxy_ret20[!is.na(indicators$dxy_ret20)]
  message(sprintf("  dxy_ret20: %d values", length(indicators$dxy_ret20)))
}

# 5. TLT 20d return
if ("TLT" %in% names(raw)) {
  indicators$tlt_ret20 <- rolling_ret20(raw[["TLT"]]$Close)
  indicators$tlt_ret20 <- indicators$tlt_ret20[!is.na(indicators$tlt_ret20)]
  message(sprintf("  tlt_ret20: %d values", length(indicators$tlt_ret20)))
}

# 6. HYG 20d return (negated for credit_stress)
if ("HYG" %in% names(raw)) {
  hyg_ret <- rolling_ret20(raw[["HYG"]]$Close)
  indicators$neg_hyg_ret20 <- -hyg_ret[!is.na(hyg_ret)]
  message(sprintf("  neg_hyg_ret20: %d values", length(indicators$neg_hyg_ret20)))
}

# 7. CPER - GLD 20d return differential
if (all(c("CPER", "GLD") %in% names(raw))) {
  cper_df <- raw[["CPER"]] |> select(date, cper = Close)
  gld_df  <- raw[["GLD"]] |> select(date, gld = Close)
  merged  <- inner_join(cper_df, gld_df, by = "date") |> filter(!is.na(cper), !is.na(gld))
  cper_ret <- rolling_ret20(merged$cper)
  gld_ret  <- rolling_ret20(merged$gld)
  diff_ret <- cper_ret - gld_ret
  indicators$copper_gold_ret <- diff_ret[!is.na(diff_ret)]
  message(sprintf("  copper_gold_ret: %d values", length(indicators$copper_gold_ret)))
}

# 8. USO 20d return (for reflation)
if ("USO" %in% names(raw)) {
  indicators$uso_ret20 <- rolling_ret20(raw[["USO"]]$Close)
  indicators$uso_ret20 <- indicators$uso_ret20[!is.na(indicators$uso_ret20)]
  message(sprintf("  uso_ret20: %d values", length(indicators$uso_ret20)))
}

# 9. GLD 20d return (for reflation)
if ("GLD" %in% names(raw)) {
  indicators$gld_ret20 <- rolling_ret20(raw[["GLD"]]$Close)
  indicators$gld_ret20 <- indicators$gld_ret20[!is.na(indicators$gld_ret20)]
  message(sprintf("  gld_ret20: %d values", length(indicators$gld_ret20)))
}

# ── Calibrate: center = median, scale = SD ────────────────────────────────────
message("\n── Calibration Results ──")

# Signal → indicator mapping + current parameters
signal_map <- list(
  vix_stress    = list(data = "vix",             current_c = 25,   current_s = 4),
  vix_calm      = list(data = "vix",             current_c = 20,   current_s = 3),
  backwardation = list(data = "backwardation",   current_c = 0.10, current_s = 0.05),
  breadth_bull  = list(data = NULL,              current_c = 50,   current_s = 10,  note = "S5FI not in Yahoo — keep manual"),
  breadth_bear  = list(data = NULL,              current_c = 35,   current_s = 8,   note = "S5FI not in Yahoo — keep manual"),
  rates_press   = list(data = "y10",             current_c = 4.5,  current_s = 0.3),
  dxy_strength  = list(data = "dxy_ret20",       current_c = 2,    current_s = 1.5),
  tlt_bid       = list(data = "tlt_ret20",       current_c = 1,    current_s = 1.5),
  credit_stress = list(data = "neg_hyg_ret20",   current_c = 1,    current_s = 1),
  copper_gold   = list(data = "copper_gold_ret", current_c = 0,    current_s = 2)
)

# Also for reflation sub-components
reflation_map <- list(
  uso_reflation = list(data = "uso_ret20", current_c = 8, current_s = 4),
  gld_reflation = list(data = "gld_ret20", current_c = 4, current_s = 3)
)

results <- data.frame(
  signal = character(), n = integer(),
  current_center = numeric(), cal_center = numeric(),
  current_scale = numeric(), cal_scale = numeric(),
  p5 = numeric(), p25 = numeric(), p50 = numeric(), p75 = numeric(), p95 = numeric(),
  note = character(),
  stringsAsFactors = FALSE
)

all_maps <- c(signal_map, reflation_map)

for (sig_name in names(all_maps)) {
  m <- all_maps[[sig_name]]

  if (is.null(m$data) || !(m$data %in% names(indicators))) {
    results <- rbind(results, data.frame(
      signal = sig_name, n = 0,
      current_center = m$current_c, cal_center = NA,
      current_scale = m$current_s, cal_scale = NA,
      p5 = NA, p25 = NA, p50 = NA, p75 = NA, p95 = NA,
      note = if (!is.null(m$note)) m$note else "no data",
      stringsAsFactors = FALSE
    ))
    next
  }

  vals <- indicators[[m$data]]
  cal_center <- median(vals)
  cal_scale  <- sd(vals)

  pcts <- quantile(vals, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))

  results <- rbind(results, data.frame(
    signal = sig_name, n = length(vals),
    current_center = m$current_c, cal_center = round(cal_center, 4),
    current_scale = m$current_s, cal_scale = round(cal_scale, 4),
    p5 = round(pcts[1], 4), p25 = round(pcts[2], 4),
    p50 = round(pcts[3], 4), p75 = round(pcts[4], 4), p95 = round(pcts[5], 4),
    note = "",
    stringsAsFactors = FALSE
  ))
}

# ── Print comparison table ────────────────────────────────────────────────────
cat("\n")
cat(sprintf("%-17s %7s  %8s → %-8s  %8s → %-8s  [%8s %8s %8s %8s %8s]  %s\n",
            "SIGNAL", "N", "CUR_C", "CAL_C", "CUR_S", "CAL_S",
            "P5", "P25", "P50", "P75", "P95", "NOTE"))
cat(paste(rep("-", 120), collapse = ""), "\n")

for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("%-17s %7d  %8.3f → %-8s  %8.3f → %-8s  [%8s %8s %8s %8s %8s]  %s\n",
              r$signal, r$n,
              r$current_center, ifelse(is.na(r$cal_center), "n/a", sprintf("%.3f", r$cal_center)),
              r$current_scale,  ifelse(is.na(r$cal_scale),  "n/a", sprintf("%.3f", r$cal_scale)),
              ifelse(is.na(r$p5),  "n/a", sprintf("%.3f", r$p5)),
              ifelse(is.na(r$p25), "n/a", sprintf("%.3f", r$p25)),
              ifelse(is.na(r$p50), "n/a", sprintf("%.3f", r$p50)),
              ifelse(is.na(r$p75), "n/a", sprintf("%.3f", r$p75)),
              ifelse(is.na(r$p95), "n/a", sprintf("%.3f", r$p95)),
              r$note))
}

# ── Generate update snippet ───────────────────────────────────────────────────
cat("\n\n── Copy-paste for scenarios.R (compute_signals) ──\n\n")

for (sig_name in names(signal_map)) {
  m <- signal_map[[sig_name]]
  r <- results[results$signal == sig_name, ]
  if (is.na(r$cal_center)) {
    cat(sprintf("# %-15s → keep current (no data)\n", sig_name))
  } else {
    cat(sprintf("# %-15s → center=%.2f (was %.2f), scale=%.2f (was %.2f)\n",
                sig_name, r$cal_center, r$current_center, r$cal_scale, r$current_scale))
  }
}

cat("\n# Reflation sub-components:\n")
for (sig_name in names(reflation_map)) {
  r <- results[results$signal == sig_name, ]
  if (!is.na(r$cal_center)) {
    cat(sprintf("# %-15s → center=%.2f (was %.2f), scale=%.2f (was %.2f)\n",
                sig_name, r$cal_center, r$current_center, r$cal_scale, r$current_scale))
  }
}

message("\nDone. Review the calibrated values, then update scenarios.R if they make sense.")
