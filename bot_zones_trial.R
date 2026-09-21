# bot_zones_trial.R — reproduces the zone/Fibonacci trial recorded in
# docs/BOT_TOOLS_DESIGN.md §4.
#
# It exists because TODO #82's measurement scripts were lost with a session
# scratchpad, leaving every figure in that item unreproducible. Rule since:
# a study whose conclusion lands in a design document keeps its script here.
#
# Run from the RStudies project root (renv):
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" bot_zones_trial.R [SYM ...]

suppressPackageStartupMessages({ library(Tdata) })
source(file.path("reports", "shared", "zones.R"))

EM_DAYS <- 10
YEARS   <- 2
SYMS <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) a else c("AAPL", "SMH", "GLD", "NVDA", "XOM", "TTE.PA", "MSFT", "SLV")
})

fetch <- function(sym) {
  d <- try(getSymIntervalDate(sym, Sys.Date() - round(YEARS * 365), Sys.Date()), silent = TRUE)
  if (inherits(d, "try-error") || is.null(d) || nrow(d) < 120) return(NULL)
  d <- d[!is.na(d$Close), ]
  # Same ATR as calc_ind()'s atr14, so the prototype cannot drift from it.
  d$atr14 <- as.numeric(TTR::ATR(cbind(d$High, d$Low, d$Close), n = 14)[, "atr"])
  d
}

report <- function(sym) {
  d <- fetch(sym)
  if (is.null(d)) { cat(sprintf("\n%-7s NO DATA\n", sym)); return(invisible()) }
  atr <- tail(d$atr14, 1); px <- tail(d$Close, 1)

  em <- try(atr_expected_move(sym, EM_DAYS, conf = 0.80, spot = px), silent = TRUE)
  em_pct <- if (!inherits(em, "try-error") && length(em$move_upper_pct)) em$move_upper_pct else NA_real_
  em_abs <- if (is.finite(em_pct)) px * em_pct / 100 else NA_real_
  regime <- if (!inherits(em, "try-error") && length(em$regime_divergence)) em$regime_divergence else NA_real_

  r <- level_read(d, atr, em_abs)
  pe <- function(x) if (is.finite(em_abs)) sprintf("%3.0f%%", x / em_abs * 100) else " n/a"

  cat(sprintf("\n%-7s px %8.2f  ATR %.2f (%.2f%%)  zz_th %.1f%%  EM%dd +%.2f%%  regime %.2f  pivots %d\n",
              sym, px, atr, atr / px * 100, r$zz_th * 100, EM_DAYS, em_pct, regime, r$n_pivots))

  if (!is.null(r$res)) {
    rr <- zone_ref(r$res, "res")
    cat(sprintf("  RES  %8.2f -%8.2f  mid %8.2f  %2d touches  %s .. %s%s\n",
                r$res$lo, r$res$hi, r$res$mid, r$res$touches, r$res$first, r$res$last,
                if (isTRUE(r$res$in_zone)) "  [spot INSIDE - distance is to the upper edge]" else ""))
    cat(sprintf("       +%.2f (+%.2f%%)  %.2f ATR  %s of EM%dd\n",
                rr - px, (rr / px - 1) * 100, (rr - px) / atr, pe(rr - px), EM_DAYS))
  } else {
    cat(sprintf("  RES  none overhead (at/near highs) -> target = fib 1.272 %.2f\n", r$target))
    cat(sprintf("       +%.2f (+%.2f%%)  %.2f ATR  %s of EM%dd\n",
                r$target - px, (r$target / px - 1) * 100, (r$target - px) / atr, pe(r$target - px), EM_DAYS))
  }
  if (!is.null(r$sup)) {
    sr <- zone_ref(r$sup, "sup")
    cat(sprintf("  SUP  %8.2f -%8.2f  mid %8.2f  %2d touches  %s .. %s%s\n",
                r$sup$lo, r$sup$hi, r$sup$mid, r$sup$touches, r$sup$first, r$sup$last,
                if (isTRUE(r$sup$in_zone)) "  [spot INSIDE - distance is to the lower edge]" else ""))
    cat(sprintf("       -%.2f (-%.2f%%)  %.2f ATR  %s of EM%dd\n",
                px - sr, (1 - sr / px) * 100, (px - sr) / atr, pe(px - sr), EM_DAYS))
  } else cat("  SUP  none\n")

  cat(sprintf("  TARGET zone %s / fib1.272 %s / fib1.618 %s  -> using %.2f (%s)\n",
              if (is.finite(r$target_zone)) sprintf("%.2f", r$target_zone) else "none",
              if (is.finite(r$target_fib)) sprintf("%.2f", r$target_fib) else "none",
              if (is.finite(r$target_fib_far)) sprintf("%.2f", r$target_fib_far) else "none",
              r$target, r$target_source))
  cat(sprintf("  STOP   zone %s / fib.618 %s  -> using %.2f (%s)\n",
              if (is.finite(r$stop_zone)) sprintf("%.2f", r$stop_zone) else "none",
              if (is.finite(r$stop_fib)) sprintf("%.2f", r$stop_fib) else "none",
              r$stop_px, r$stop_source))
  cat(sprintf("  RNG(dyn) %s   ASYM %s   ASYM(fib-only) %s   zone window -> %s, %s sessions\n",
              if (is.finite(r$rng_dyn)) sprintf("%.0f%%", r$rng_dyn) else "n/a",
              if (is.finite(r$asym)) sprintf("%.2f:1", r$asym) else "n/a",
              if (is.finite(r$asym_fib)) sprintf("%.2f:1", r$asym_fib) else "n/a",
              r$zone_window_from, r$zone_window_sessions))

  if (!is.null(r$fib)) {
    f <- r$fib
    cat(sprintf("  LEG  %.2f -> %.2f (anchor %s, +%.1f%%)   retr .382 %.2f  .500 %.2f  .618 %.2f\n",
                f$leg_low, f$leg_high, f$anchor_date, f$leg / f$leg_low * 100, f$ret[1], f$ret[2], f$ret[3]))
    cat(sprintf("  EXT  1.272 %.2f (%+.1f%%, %.1f ATR, %s of EM%dd)   1.618 %.2f (%+.1f%%)%s%s\n",
                f$ext[1], (f$ext[1] / px - 1) * 100, (f$ext[1] - px) / atr, pe(f$ext[1] - px), EM_DAYS,
                f$ext[2], (f$ext[2] / px - 1) * 100,
                if (isTRUE(r$fib_confirms_res)) "  [1.272 CONFIRMS res]" else if (isFALSE(r$fib_confirms_res)) "  [1.272 outside res]" else "",
                if (isTRUE(r$fib_confirms_sup)) "  [retr CONFIRMS sup]" else ""))
  }
  invisible()
}

for (s in SYMS) try(report(s))
cat("\ndone\n")
