# ============================================================
# Pooled catalyst study — both directions
# ============================================================
# (1) Positive surprise pool: events where 2-day earnings gap >= +3%.
#     Test: does pooled RR25 lift in days 1..20 post-anchor? Peak = max(excess).
# (2) Negative surprise pool: events where 2-day earnings gap <= -3%.
#     Test: does pooled RR25 *drop* (puts richen)? Peak = min(excess); we test
#     whether observed minimum is significantly negative.
#
# Pooling gives us ~50 events per direction across 9 tickers — much more power
# than per-ticker n=6-8, at the cost of a homogeneity assumption.

SUPPRESS_PANEL <- TRUE
source("C:/Users/aldoh/Documents/RApplication/RStudies/IVvsSpotLag.R", echo = FALSE)

GAP_THRESH    <- 0.03
ANCHOR_OFFSET <- 1
HORIZONS_P    <- 0:20
EXCL_PAD_P    <- 14
N_BOOT        <- 1000

# Returns per-ticker event/control path matrices and the eligibility index for
# bootstrap. `direction` = "up" picks gap >= +thresh, "down" picks gap <= -thresh.
extract_paths <- function(ticker, direction = c("up","down"),
                          thresh = GAP_THRESH, horizons = HORIZONS_P,
                          anchor_offset = ANCHOR_OFFSET, excl_pad = EXCL_PAD_P) {
  direction <- match.arg(direction)
  d <- file.path(OUT_BASE, ticker)
  sp    <- as.data.table(arrow::read_parquet(file.path(d, "stock_prices.parquet")))
  ivx   <- as.data.table(arrow::read_parquet(file.path(d, "ivx.parquet")))
  ivs25 <- as.data.table(arrow::read_parquet(file.path(d, "ivs_25delta_30d_wide.parquet")))
  for (x in list(sp, ivx, ivs25)) x[, date := as.Date(date)]
  panel <- sp[, .(date, spot = close)] |>
    merge(ivx[, .(date, atm = `30d IV Mean`)], by = "date") |>
    merge(ivs25[, .(date, c25, p25)], by = "date")
  setorder(panel, date)
  panel[, skew_call := c25 - atm]
  panel[, rr25      := c25 - p25]

  earn <- EARNINGS[[ticker]]
  if (is.null(earn)) return(NULL)

  earn_idx <- vapply(earn, \(e) {
    cands <- which(panel$date >= e); if (!length(cands)) NA_integer_ else cands[1]
  }, integer(1))
  earn_idx <- earn_idx[!is.na(earn_idx) & earn_idx >= 2 & earn_idx + 1 <= nrow(panel)]
  gaps <- log(panel$spot[earn_idx + 1] / panel$spot[earn_idx - 1])

  qual <- if (direction == "up") earn_idx[gaps >= thresh] else earn_idx[gaps <= -thresh]
  if (!length(qual)) return(NULL)
  event_idx <- qual + anchor_offset
  event_idx <- event_idx[event_idx + max(horizons) <= nrow(panel)]
  if (!length(event_idx)) return(NULL)

  excl_dates <- unique(unlist(lapply(earn, \(d) seq(d - excl_pad, d + excl_pad, by = "day"))))
  near_earn  <- panel$date %in% as.Date(excl_dates)
  eligible <- which(!near_earn & !(seq_len(nrow(panel)) %in% event_idx) &
                    seq_len(nrow(panel)) + max(horizons) <= nrow(panel))
  if (length(eligible) < length(event_idx)) return(NULL)

  set.seed(42 + nchar(ticker))
  rand <- sample(eligible, length(event_idx))

  build_matrix <- function(var, idx) {
    sapply(idx, \(i) panel[[var]][i + horizons] - panel[[var]][i])
  }
  list(
    ticker = ticker,
    n      = length(event_idx),
    n_qual = length(qual),
    panel  = panel,
    event_idx = event_idx,
    eligible  = eligible,
    paths_event = list(
      atm  = build_matrix("atm",       event_idx),
      skew = build_matrix("skew_call", event_idx),
      rr25 = build_matrix("rr25",      event_idx)),
    paths_ctrl = list(
      atm  = build_matrix("atm",       rand),
      skew = build_matrix("skew_call", rand),
      rr25 = build_matrix("rr25",      rand)),
    horizons = horizons
  )
}

run_pooled <- function(direction = c("up","down"), n_boot = N_BOOT) {
  direction <- match.arg(direction)
  available <- list.files(OUT_BASE, full.names = FALSE)
  available <- available[available %in% names(EARNINGS)]
  cat("\n========== POOLED ", toupper(direction), "  gap ",
      ifelse(direction == "up", ">= +", "<= -"), GAP_THRESH*100, "%  ==========\n", sep = "")

  per_ticker <- lapply(available, extract_paths, direction = direction)
  per_ticker <- Filter(Negate(is.null), per_ticker)
  if (!length(per_ticker)) { cat("  no qualifying events\n"); return(NULL) }

  cat("Per-ticker counts:\n")
  for (pt in per_ticker) cat(sprintf("  %-6s n=%d (qualifying earnings: %d)\n",
                                     pt$ticker, pt$n, pt$n_qual))
  total_n <- sum(sapply(per_ticker, \(x) x$n))
  cat(sprintf("Pooled events: %d\n", total_n))
  if (total_n < 15) { cat("  too few; skipping\n"); return(NULL) }

  pool <- function(var, which) {
    do.call(cbind, lapply(per_ticker, \(pt) pt[[which]][[var]]))
  }
  ev_atm  <- pool("atm",  "paths_event"); ct_atm  <- pool("atm",  "paths_ctrl")
  ev_skew <- pool("skew", "paths_event"); ct_skew <- pool("skew", "paths_ctrl")
  ev_rr   <- pool("rr25", "paths_event"); ct_rr   <- pool("rr25", "paths_ctrl")

  excess <- data.table(
    horizon      = HORIZONS_P,
    atm_excess   = rowMeans(ev_atm,  na.rm = TRUE) - rowMeans(ct_atm,  na.rm = TRUE),
    skewC_excess = rowMeans(ev_skew, na.rm = TRUE) - rowMeans(ct_skew, na.rm = TRUE),
    rr25_excess  = rowMeans(ev_rr,   na.rm = TRUE) - rowMeans(ct_rr,   na.rm = TRUE),
    n_event      = rowSums(!is.na(ev_rr)),
    rr25_se      = sqrt(apply(ev_rr,  1, var, na.rm = TRUE) / rowSums(!is.na(ev_rr)) +
                        apply(ct_rr,  1, var, na.rm = TRUE) / rowSums(!is.na(ct_rr)))
  )

  obs_peak <- if (direction == "up") max(excess$rr25_excess) else min(excess$rr25_excess)
  obs_h    <- if (direction == "up") excess$horizon[which.max(excess$rr25_excess)] else excess$horizon[which.min(excess$rr25_excess)]

  # Bootstrap: per ticker resample controls, build new pooled excess path, take peak
  boot_peaks <- replicate(n_boot, {
    pooled_event_mean <- rowMeans(ev_rr, na.rm = TRUE)   # event mean fixed
    boot_ctrl <- do.call(cbind, lapply(per_ticker, function(pt) {
      ri <- sample(pt$eligible, pt$n)
      sapply(ri, \(i) pt$panel[["rr25"]][i + pt$horizons] - pt$panel[["rr25"]][i])
    }))
    pooled_ctrl_mean <- rowMeans(boot_ctrl, na.rm = TRUE)
    diff <- pooled_event_mean - pooled_ctrl_mean
    if (direction == "up") max(diff) else min(diff)
  })
  p_value <- if (direction == "up") mean(boot_peaks >= obs_peak)
             else                   mean(boot_peaks <= obs_peak)

  cat("\nExcess paths (pooled):\n"); print(excess)
  cat(sprintf("\nObserved RR25 peak (%s): d%d  %+.4f  vol pts\n",
              direction, obs_h, obs_peak))
  cat(sprintf("Bootstrap p-value (%d iters): %.4f\n", n_boot, p_value))

  list(direction = direction, total_n = total_n, excess = excess,
       obs_peak = obs_peak, obs_h = obs_h, p_value = p_value)
}

# Run both directions
res_up   <- run_pooled("up")
res_down <- run_pooled("down")

# Combined summary
cat("\n============================================================\n")
cat("POOLED SUMMARY\n")
cat("============================================================\n")
cat(sprintf("Up   (gap >= +%.0f%%, n=%d):  RR25 peak d%d  %+.4f  p=%.4f\n",
            GAP_THRESH*100, res_up$total_n, res_up$obs_h, res_up$obs_peak, res_up$p_value))
cat(sprintf("Down (gap <= -%.0f%%, n=%d): RR25 peak d%d  %+.4f  p=%.4f\n",
            GAP_THRESH*100, res_down$total_n, res_down$obs_h, res_down$obs_peak, res_down$p_value))

saveRDS(list(up = res_up, down = res_down),
        file.path(OUT_BASE, "pooled_catalyst_results.rds"))
fwrite(res_up$excess,   file.path(OUT_BASE, "pooled_catalyst_up.csv"))
fwrite(res_down$excess, file.path(OUT_BASE, "pooled_catalyst_down.csv"))
cat("\nSaved -> pooled_catalyst_results.rds, pooled_catalyst_{up,down}.csv\n")
