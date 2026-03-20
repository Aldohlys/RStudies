# positioning.R — Weekly COT positioning summary
#
# EDIT THIS FILE EVERY MONDAY after reading Ole Hansen's COT report.
# Source: https://www.home.saxo/en-ch/content/articles/commodities/
# Each entry: asset, net direction, extreme flag, note.
# This feeds into regime positioning stress computation.

COT_POSITIONING <- list(
  list(asset = "Crude Oil", sector = "Energy",
       net = "long", extreme = TRUE,
       note = "466k contracts, 15-month high, fresh longs + short covering"),
  list(asset = "USD", sector = "Macro",
       net = "short", extreme = FALSE,
       note = "80% of shorts unwound in 3 weeks, USD 4.7B remaining"),
  list(asset = "Gold", sector = "PreciousMetals",
       net = "long", extreme = FALSE,
       note = "Short covering not fresh longs — hesitant positioning"),
  list(asset = "Grains", sector = "Agriculture",
       net = "long", extreme = TRUE,
       note = "592k combined, June 2022 highs, ethanol/food security"),
  list(asset = "Copper", sector = "Materials",
       net = "short", extreme = FALSE,
       note = "Net selling 11th consecutive week, stockpiles at multi-decade highs")
)

# Last updated: 2026-03-16 (COT data week ending 2026-03-10)
COT_DATE <- "2026-03-16"
