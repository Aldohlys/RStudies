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
# Generated 2026-09-26 08:00 from CFTC data as of 2026-09-22.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 101.8k (long 223.2k / short 121.4k). 5y percentile 34, 1y percentile 94 (5y range -38.2k to +301.7k, n=261). WoW: -4.5k. Since 2026-09-15: -4.5k."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "Lev+AM net long 12.3k (long 31.9k / short 19.6k). 5y percentile 57, 1y percentile 74 (5y range -17.5k to +36.6k, n=261). WoW: +1.4k. Since 2026-09-15: +1.4k."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 127.4k (long 135.7k / short 8.3k). 5y percentile 66, 1y percentile 72 (5y range -43.1k to +219.0k, n=261). WoW: -5.7k. Since 2026-09-15: -5.7k."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "MM net long 657.2k (long 861.0k / short 203.8k). 5y percentile 99, 1y percentile 96 (5y range -608.7k to +676.6k, n=261). WoW: +5.0k. Since 2026-09-15: +5.0k. Legs: Corn +404.1k, Soy +265.2k, SRW wheat -12.0k; WoW Corn -10.4k, Soy +23.7k, SRW wheat -8.3k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = TRUE,
       note = "MM net long 82.5k (long 96.4k / short 13.9k). 5y percentile 100, 1y percentile 98 (5y range -43.9k to +82.5k, n=261). WoW: +17.4k. Since 2026-09-15: +17.4k.")
)

# Last updated: 2026-09-26 (COT data week ending 2026-09-22)
COT_DATE <- "2026-09-26"
COT_AS_OF <- "2026-09-22"   # Tuesday-close date the positions refer to

