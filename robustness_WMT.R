#  Robustness tests for the WMT positive result.
#  Re-runs the WMT event study under several knob variants and tabulates the
#  RR25 peak day, peak excess, and bootstrap p-value for each. If WMT survives
#  all variants the result is robust; if a single combination kills it,
#  the baseline run is fragile.

SUPPRESS_PANEL <- TRUE
source("C:/Users/aldoh/Documents/RApplication/RStudies/IVvsSpotLag.R", echo = FALSE)

variants <- list(
  list(label = "baseline (LB10 Q.90 H0:15)",     lookback = 10, decile_q = 0.90, horizons = 0:15),
  list(label = "longer horizons   H0:25",        lookback = 10, decile_q = 0.90, horizons = 0:25),
  list(label = "longer lookback   LB12",         lookback = 12, decile_q = 0.90, horizons = 0:15),
  list(label = "looser threshold  Q.85",         lookback = 10, decile_q = 0.85, horizons = 0:15),
  list(label = "tighter threshold Q.95",         lookback = 10, decile_q = 0.95, horizons = 0:15),
  list(label = "shorter lookback  LB5",          lookback = 5,  decile_q = 0.90, horizons = 0:15),
  list(label = "all combined      LB12 Q.85 H0:25", lookback = 12, decile_q = 0.85, horizons = 0:25)
)

results <- lapply(variants, function(v) {
  r <- run_study("WMT", n_boot = 1000, verbose = FALSE,
                 lookback = v$lookback, decile_q = v$decile_q, horizons = v$horizons)
  if (is.null(r)) return(data.table(variant = v$label, n = 0L, peak_d = NA_integer_,
                                    peak_val = NA_real_, p_rr25 = NA_real_))
  data.table(
    variant  = v$label,
    n        = r$n_events,
    peak_d   = r$peak_rr25$horizon,
    peak_val = round(r$peak_rr25$rr25_excess, 4),
    p_rr25   = round(r$p_rr25, 3)
  )
})
out <- rbindlist(results)
cat("\n========================================================\n")
cat("WMT robustness — RR25 peak across knob variants\n")
cat("========================================================\n")
print(out)
fwrite(out, "C:/Users/aldoh/Documents/RApplication/RStudies/ivol_probe_out/wmt_robustness.csv")
