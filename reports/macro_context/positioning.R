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
# Generated 2026-09-20 05:58 from CFTC data as of 2026-09-15.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 106.3k (long 221.9k / short 115.6k). 5y percentile 36, 1y percentile 96 (5y range -38.2k to +301.7k, n=261). WoW: -5.5k. Since 2026-09-01: +12.0k."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "Lev+AM net long 10.9k (long 29.6k / short 18.7k). 5y percentile 56, 1y percentile 74 (5y range -17.5k to +36.6k, n=261). WoW: -11.6k. Since 2026-09-01: -12.5k."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 133.1k (long 142.4k / short 9.3k). 5y percentile 70, 1y percentile 77 (5y range -43.1k to +219.0k, n=261). WoW: -1.9k. Since 2026-09-01: -3.7k."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "MM net long 652.3k (long 862.1k / short 209.8k). 5y percentile 99, 1y percentile 96 (5y range -608.7k to +676.6k, n=261). WoW: -24.3k. Since 2026-09-01: +1.5k. Legs: Corn +414.5k, Soy +241.5k, SRW wheat -3.7k; WoW Corn +0.0k, Soy -15.8k, SRW wheat -8.5k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = TRUE,
       note = "MM net long 65.1k (long 83.7k / short 18.6k). 5y percentile 92, 1y percentile 62 (5y range -43.9k to +82.2k, n=261). WoW: -17.0k. Since 2026-09-01: -7.8k.")
)

# Last updated: 2026-09-20 (COT data week ending 2026-09-15)
COT_DATE <- "2026-09-20"
COT_AS_OF <- "2026-09-15"   # Tuesday-close date the positions refer to

