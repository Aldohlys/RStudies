# positioning.R -- weekly COT positioning summary
#
# GENERATED FILE -- do not hand-edit. Regenerate with:
#   Rscript RStudies/reports/macro_context/refresh_cot.R
# (scheduled Saturday 08:00, Windows task \RApplication\RefreshCOT). Hand
# edits are silently overwritten on the next run; to change what it says,
# change refresh_cot.R.
#
# Built from the CFTC annual archives (fut_disagg_txt / fut_fin_txt), which
# carry both the newest release and the multi-year history, so the current
# numbers and the percentiles come from one source. Series are Managed Money
# net, except USD which is Leveraged Funds + Asset Manager net (TFF report).
#
# extreme = TRUE when the net is at or beyond the 5-year 90th percentile
# (crowded long) or 10th percentile (crowded short). Reproducible from public
# data -- no narrative judgement, no Saxo digest dependency.
#
# This feeds compute_positioning_stress() in scenarios.R, which also warns when
# COT_AS_OF has gone stale (a missed weekly release).
#
# Generated 2026-11-14 10:40 from CFTC data as of 2026-11-03.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = TRUE,
       note = "MM net long 21.7k (long 147.4k / short 125.7k). 5y percentile 6, 1y percentile 17 (5y range -38.2k to +301.7k, n=261). WoW: -9.4k."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "Lev+AM net long 20.1k (long 37.3k / short 17.2k). 5y percentile 75, 1y percentile 83 (5y range -17.5k to +36.6k, n=261). WoW: +0.3k."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 103.6k (long 122.3k / short 18.7k). 5y percentile 46, 1y percentile 36 (5y range -43.1k to +219.0k, n=261). WoW: -1.9k."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = FALSE,
       note = "MM net long 64.4k (long 495.8k / short 431.4k). 5y percentile 53, 1y percentile 32 (5y range -608.7k to +676.6k, n=261). WoW: -135.9k. Legs: Corn +8.8k, Soy +142.7k, SRW wheat -87.1k; WoW Corn -72.3k, Soy -51.1k, SRW wheat -12.5k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = FALSE,
       note = "MM net long 34.9k (long 58.6k / short 23.7k). 5y percentile 66, 1y percentile 0 (5y range -43.9k to +82.5k, n=261). WoW: -5.7k.")
)

# Last updated: 2026-11-14 (COT data week ending 2026-11-03)
COT_DATE <- "2026-11-14"
COT_AS_OF <- "2026-11-03"   # Tuesday-close date the positions refer to

