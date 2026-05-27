# positioning.R — Weekly COT positioning summary
#
# EDIT THIS FILE EVERY MONDAY after reading Ole Hansen's COT report.
# Source: https://www.home.saxo/en-ch/content/articles/commodities/
# Each entry: asset, net direction, extreme flag, note.
# This feeds into regime positioning stress computation.
#
# Refresh of 2026-05-27 was populated from raw CFTC reports (week ending
# 2026-05-19, released 2026-05-22) rather than Ole Hansen's narrative —
# extreme flags default to FALSE without multi-year context. Set to TRUE
# only when reading the Saxo digest confirms extreme positioning vs
# multi-year history (e.g., "15-month high").

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = FALSE,
       note = "MM net long 98k (long 207k / short 109k); down sharply from 466k Mar-10 extreme. WoW: +25k net (added length on the dip)."),
  list(asset = "USD", sector = "Macro",
       net = "short", extreme = FALSE,
       note = "DXY Lev+AM combined net -0.5k (essentially flat). WoW: shorts added (+6.2k) vs longs (+2.5k). Note: DXY OI thin (~40k); aggregate-across-currencies is the Saxo-style metric."),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "MM net long 93.5k (long 122.9k / short 29.4k). WoW: -4.3k net (mild trimming)."),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = FALSE,
       note = "Combined corn+soy+SRW wheat MM net long 488k (long 773k / short 285k); down from 592k Mar-10 extreme. Corn +14k WoW, soy -6k, wheat +9k. Still elevated absolute size."),
  list(asset = "Copper", sector = "Materials",
       net = "long", extreme = FALSE,
       note = "MM net long 74k (long 87k / short 13k). Major regime flip: prior reading was 'net selling 11th consecutive week, multi-decade stockpile highs' — now net long. WoW: -1k net (steady).")
)

# Last updated: 2026-05-27 (COT data week ending 2026-05-19)
COT_DATE <- "2026-05-27"
