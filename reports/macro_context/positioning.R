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
# Generated 2026-09-03 10:28 from CFTC data as of 2026-08-25.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 84.0k (long 196.9k / short 112.9k). 5y percentile 22, 1y percentile 75 (5y range -38.2k to +301.7k, n=261). WoW: -3.5k."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "Lev+AM net long 23.2k (long 32.2k / short 9.0k). 5y percentile 81, 1y percentile 98 (5y range -17.5k to +36.6k, n=261). WoW: +0.1k."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 144.7k (long 159.8k / short 15.1k). 5y percentile 79, 1y percentile 89 (5y range -43.1k to +219.0k, n=261). WoW: +3.1k."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "MM net long 504.5k (long 738.6k / short 234.1k). 5y percentile 96, 1y percentile 96 (5y range -608.7k to +558.9k, n=261). WoW: +196.4k. Legs: Corn +317.4k, Soy +200.7k, SRW wheat -13.6k; WoW Corn +135.8k, Soy +48.9k, SRW wheat +11.7k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = TRUE,
       note = "MM net long 76.3k (long 92.1k / short 15.8k). 5y percentile 99, 1y percentile 94 (5y range -43.9k to +79.0k, n=261). WoW: -2.4k.")
)

# Last updated: 2026-09-03 (COT data week ending 2026-08-25)
COT_DATE <- "2026-09-03"
COT_AS_OF <- "2026-08-25"   # Tuesday-close date the positions refer to

