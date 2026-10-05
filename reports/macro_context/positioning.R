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
# Generated 2026-10-03 08:00 from CFTC data as of 2026-09-29.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 79.6k (long 209.0k / short 129.4k). 5y percentile 19, 1y percentile 57 (5y range -38.2k to +301.7k, n=261). WoW: -22.2k. Since 2026-09-22: -22.2k."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "Lev+AM net long 18.4k (long 36.2k / short 17.8k). 5y percentile 67, 1y percentile 85 (5y range -17.5k to +36.6k, n=261). WoW: +6.1k. Since 2026-09-22: +6.1k."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 120.3k (long 131.7k / short 11.4k). 5y percentile 61, 1y percentile 60 (5y range -43.1k to +219.0k, n=261). WoW: -7.1k. Since 2026-09-22: -7.1k."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "MM net long 605.7k (long 803.5k / short 197.8k). 5y percentile 98, 1y percentile 91 (5y range -608.7k to +676.6k, n=261). WoW: -51.6k. Since 2026-09-22: -51.6k. Legs: Corn +381.2k, Soy +246.6k, SRW wheat -22.1k; WoW Corn -22.9k, Soy -18.6k, SRW wheat -10.1k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = TRUE,
       note = "MM net long 78.1k (long 92.1k / short 14.0k). 5y percentile 98, 1y percentile 91 (5y range -43.9k to +82.5k, n=261). WoW: -4.5k. Since 2026-09-22: -4.5k.")
)

# Last updated: 2026-10-03 (COT data week ending 2026-09-29)
COT_DATE <- "2026-10-03"
COT_AS_OF <- "2026-09-29"   # Tuesday-close date the positions refer to

