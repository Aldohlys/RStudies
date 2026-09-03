# positioning.R — Weekly COT positioning summary
#
# EDIT THIS FILE EVERY MONDAY after reading Ole Hansen's COT report.
# Source: https://www.home.saxo/en-ch/content/articles/commodities/
# Each entry: asset, net direction, extreme flag, note.
# This feeds into regime positioning stress computation.
#
# Refresh of 2026-09-03 was populated from raw CFTC reports (week ending
# 2026-08-25, released 2026-08-28). Unlike the 2026-05-27 raw refresh, the
# extreme flags are NOT defaulted to FALSE — they are set from the CFTC
# historical archive (fut_disagg_txt / fut_fin_txt, 2021-2026, 295 weekly
# observations per asset):
#
#   extreme = TRUE when the current net is at or beyond the 5-year 90th
#   percentile (crowded long) or 10th percentile (crowded short).
#
# That rule replaces the narrative judgement the Saxo digest used to supply,
# so the flags can be reproduced from public data. Percentiles are recorded
# in each note so the next refresh can see what moved. Series are Managed
# Money net (disaggregated report), except USD which is Leveraged Funds +
# Asset Manager net (TFF report) — the metric the earlier entries used.
#
# Reading order if refreshing by hand: see the cftc-cot-urls memory. Note
# that financial_lf.htm DOES carry the ICE USD INDEX contract (098662).

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 84.0k (long 196.9k / short 112.9k). 5y percentile 20 - length is in the BOTTOM fifth of the 5-year range (5y max 383k), so crude is the one uncrowded market here; 1y percentile 76. WoW: -3.5k. Since 19-May: -14.2k, the slow unwind from the Mar-10 466k extreme continues."),
  list(asset = "USD", sector = "Macro",
       net = "long", extreme = FALSE,
       note = "DXY Lev+AM combined net LONG 23.2k (long 32.2k / short 9.0k) - DIRECTION FLIPPED since the last refresh, which recorded 'short'/flat. 5y percentile 83, 1y percentile 98 = a one-year high in USD length. WoW: +0.1k (flat, but the build ran 17.9k -> 23.2k over six weeks). DXY OI thin (~48k)."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 144.7k (long 159.8k / short 15.1k). 5y percentile 81, 1y percentile 89 - elevated, not extreme (5y max 219k). WoW: +3.1k. Since 19-May: +51.2k, a steady 55% build in length."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "Combined corn+soy+SRW wheat MM net long 504.5k (long 738.6k / short 234.1k). 5y percentile 92, 1y percentile 96, vs a 5y max of 559k = crowded long. WoW: +196.4k in ONE week (corn +135.8k, soy +48.9k, wheat +11.7k) - verified against the CFTC historical archive, not a parse artifact. Corn alone is 317.4k net long (1y percentile 96); SRW wheat is still net short 13.6k."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = TRUE,
       note = "MM net long 76.3k (long 92.1k / short 15.8k). 5y percentile 97 and within 12% of the 5-year record (86.9k) = the most crowded position on this list. WoW: -2.4k. Since 19-May: +2.1k - it has simply held near the highs for three months rather than pushing on.")
)

# Last updated: 2026-09-03 (COT data week ending 2026-08-25, released 2026-08-28)
COT_DATE <- "2026-09-03"
COT_AS_OF <- "2026-08-25"   # Tuesday-close date the positions refer to
