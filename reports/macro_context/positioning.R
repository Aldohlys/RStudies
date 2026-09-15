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
# Generated 2026-09-05 23:13 from CFTC data as of 2026-09-01.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 94.3k (long 205.3k / short 111.0k). 5y percentile 28, 1y percentile 85 (5y range -38.2k to +301.7k, n=261). WoW: +10.3k. Since 2026-08-25: +10.3k."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "Lev+AM net long 23.4k (long 33.7k / short 10.3k). 5y percentile 81, 1y percentile 98 (5y range -17.5k to +36.6k, n=261). WoW: +0.2k. Since 2026-08-25: +0.2k."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 136.8k (long 149.7k / short 12.9k). 5y percentile 73, 1y percentile 81 (5y range -43.1k to +219.0k, n=261). WoW: -8.0k. Since 2026-08-25: -8.0k."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "MM net long 650.8k (long 847.9k / short 197.1k). 5y percentile 100, 1y percentile 98 (5y range -608.7k to +650.8k, n=261). WoW: +146.3k. Since 2026-08-25: +146.3k. Legs: Corn +401.0k, Soy +234.9k, SRW wheat +14.9k; WoW Corn +83.6k, Soy +34.2k, SRW wheat +28.5k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = TRUE,
       note = "MM net long 72.9k (long 91.4k / short 18.5k). 5y percentile 96, 1y percentile 81 (5y range -43.9k to +79.0k, n=261). WoW: -3.4k. Since 2026-08-25: -3.4k.")
)

# Last updated: 2026-09-05 (COT data week ending 2026-09-01)
COT_DATE <- "2026-09-05"
COT_AS_OF <- "2026-09-01"   # Tuesday-close date the positions refer to

