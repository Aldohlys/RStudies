# intermarket_xlsx.R — daily BOT sector map workbook: Reports/intermarket_sectors_<date>.xlsx
#
# Sheet "Data" holds analyze_sectors() as is; sheet "Legend" defines every column in plain
# words, with no reference to other documents. The definitions describe intermarket.R
# (series_metrics, rotation, driver_score, bot_verdict, ew_close, group_map): change them
# together. write_sectors_xlsx() stops if a column has no legend row.

# Column, unit / values, definition. Order = column order of analyze_sectors().
SECTORS_LEGEND <- data.frame(stringsAsFactors = FALSE, check.names = FALSE, matrix(byrow = TRUE, ncol = 3, dimnames = list(NULL, c("Column", "Unit / values", "Definition")), c(
  "group", "text",
  "Name of the sector group: a set of stocks from the scanner universe that tend to move together (same industry or same economic driver). Only groups containing at least one stock eligible for the breakout (BOT) strategy are listed. All metrics in a row are computed on the group's equal-weight index, never on a single ETF.",

  "bench", "text",
  "How the group's price series is built. EW = equal-weight index: each day, the average daily return of the members that traded that day (dividend-adjusted prices), compounded from 100. A day is counted only if at least half the members have a price; a member's daily move larger than 50% is treated as a bad price and dropped.",

  "trend", "UP / DOWN / MIXED",
  "Trend state of the group index from two exponential moving averages (EMA) of the daily close. UP = close above the 50-day EMA and 20-day EMA above the 50-day EMA. DOWN = close below the 50-day EMA and 20-day EMA below the 50-day EMA. MIXED = any other combination.",

  "c1m", "% change",
  "Price change of the group index over the last 21 trading days (about 1 month), in percent.",

  "c3m", "% change",
  "Price change of the group index over the last 63 trading days (about 3 months), in percent.",

  "pos52", "0 to 100",
  "Position of today's close inside the range of the last 252 trading days (about 52 weeks). 0 = at the 52-week low, 100 = at the 52-week high, 50 = midway. Formula: 100 x (close - lowest close) / (highest close - lowest close).",

  "above200", "TRUE / FALSE",
  "TRUE if today's close of the group index is above its 200-day simple moving average, FALSE if below.",

  "rs_ratio", "index, 100 = neutral",
  "Relative strength level versus the S&P 500 index. First the ratio group index / S&P 500 is computed each day; rs_ratio = 100 x today's ratio / the 50-day simple average of that ratio. Above 100 = the group is currently stronger against the S&P 500 than over the last 50 days on average; below 100 = weaker. Rows are sorted on this value (highest first) within each verdict.",

  "rs_mom", "points of rs_ratio",
  "Relative strength momentum: change of rs_ratio over the last 10 trading days (today's rs_ratio minus rs_ratio 10 days ago). Positive = relative strength is improving; negative = fading.",

  "quadrant", "Leading / Weakening / Improving / Lagging",
  "Relative rotation quadrant from rs_ratio and rs_mom. Leading = rs_ratio >= 100 and rs_mom >= 0 (outperforming and still gaining). Weakening = rs_ratio >= 100 and rs_mom < 0 (outperforming but losing ground). Improving = rs_ratio < 100 and rs_mom >= 0 (underperforming but catching up). Lagging = rs_ratio < 100 and rs_mom < 0 (underperforming and falling further behind).",

  "rs_3m", "% change",
  "Change over the last 63 trading days (about 3 months) of the ratio group index / S&P 500, in percent. Approximately the group's 3-month outperformance (positive) or underperformance (negative) against the S&P 500.",

  "driver_score", "-1 to +1",
  "Agreement of the group's macro drivers with a rising group. Each group has a fixed list of market series that historically push it (shown in the drivers column). For each driver: its trend (same UP/DOWN/MIXED rule as the trend column) is scored +1 UP, -1 DOWN, 0 MIXED, and flipped in sign when the driver works inversely. The score is the average over the drivers. +1 = all drivers favour the group rising; -1 = all favour it falling; 0 = neutral or split. Empty if the group has no drivers defined.",

  "drivers", "text",
  "The driver series behind driver_score, each followed by its current trend. A pair written A/B is the ratio of A to B (e.g. HO=F/CL=F = heating oil divided by crude oil, a refining-margin proxy). '(inverse)' marks a driver whose rise is bad for the group, so its trend counts with the opposite sign. Symbols are Yahoo Finance codes: =F futures, =X currency pairs, ^ indices (e.g. ^TNX = US 10-year Treasury yield, ^NDX = Nasdaq 100, BZ=F = Brent crude, NG=F = natural gas, HG=F = copper, HYG/IEF = high-yield bonds vs Treasuries, a credit-risk appetite gauge).",

  "verdict", "LONG / SHORT / WATCH / AVOID",
  "Suggested direction for breakout trades in this group, applied in this order. AVOID = trend is MIXED, or drivers clearly oppose the trend (trend UP with driver_score <= -0.5, or trend DOWN with driver_score >= +0.5). LONG = trend UP and quadrant Leading or Improving. SHORT = trend DOWN and quadrant Lagging or Weakening. WATCH = all other cases (trend and relative rotation disagree). Rows are ordered LONG, SHORT, WATCH, AVOID.",

  "n_members", "count",
  "Number of stocks in the group (all members, whether or not eligible for the breakout strategy).",

  "n_quoted", "count",
  "Number of members with price data available in this run. If lower than n_members, the equal-weight index was built from fewer stocks.",

  "n_bot", "count",
  "Number of members eligible for the breakout (BOT) strategy, i.e. the stocks that can actually be traded from this row.",

  "members", "text",
  "Ticker symbols of all group members, space-separated (Yahoo Finance codes; non-US listings carry an exchange suffix such as .PA Paris, .DE Xetra, .SW Swiss, .L London, .MI Milan, .TO Toronto).",

  "tags", "text",
  "ETF tickers associated with the group: the group's reference ETF and the benchmark ETF most members are usually compared with. Used to link a macro scenario that names an ETF to the matching group. Informational only; no metric in the row is computed from these ETFs."
)))

SECTORS_NUM_FMT <- c(c1m = "0.0", c3m = "0.0", pos52 = "0", rs_ratio = "0.0", rs_mom = "0.00",
                     rs_3m = "0.0", driver_score = "0.00")
SECTORS_COL_WIDTH <- c(group = 42, drivers = 45, members = 40, tags = 18)

#' Write the sector map workbook (sheets Data and Legend)
#' @param sectors data.frame from analyze_sectors()
#' @param path output .xlsx path
#' @param run_date date shown in the legend header
write_sectors_xlsx <- function(sectors, path, run_date = Sys.Date()) {
  sectors <- as.data.frame(sectors)
  attr(sectors, "ungrouped") <- NULL
  undefined <- setdiff(names(sectors), SECTORS_LEGEND$Column)
  if (length(undefined)) stop("SECTORS_LEGEND (intermarket_xlsx.R) has no definition for column(s): ",
                              paste(undefined, collapse = ", "))
  legend <- SECTORS_LEGEND[match(names(sectors), SECTORS_LEGEND$Column), ]

  hdr <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDEBF7")
  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Data")
  openxlsx::writeData(wb, "Data", sectors, headerStyle = hdr, withFilter = TRUE)
  for (col in intersect(names(SECTORS_NUM_FMT), names(sectors)))
    openxlsx::addStyle(wb, "Data", openxlsx::createStyle(numFmt = SECTORS_NUM_FMT[[col]]),
                       rows = seq_len(nrow(sectors)) + 1, cols = match(col, names(sectors)))
  widths <- ifelse(names(sectors) %in% names(SECTORS_COL_WIDTH),
                   SECTORS_COL_WIDTH[names(sectors)], pmax(9, nchar(names(sectors)) + 2))
  openxlsx::setColWidths(wb, "Data", cols = seq_along(sectors), widths = widths)
  openxlsx::freezePane(wb, "Data", firstActiveRow = 2, firstActiveCol = 2)

  openxlsx::addWorksheet(wb, "Legend")
  openxlsx::writeData(wb, "Legend", "Intermarket sector groups - column definitions", startRow = 1)
  openxlsx::addStyle(wb, "Legend", openxlsx::createStyle(textDecoration = "bold", fontSize = 12), rows = 1, cols = 1)
  openxlsx::writeData(wb, "Legend", sprintf(paste(
    "One row per sector group, from the macro context report run of %s.",
    "All metrics use daily closes; 'trading days' are days with a price.",
    "Benchmark for relative strength: S&P 500 index."), format(run_date, "%Y-%m-%d")), startRow = 2)
  openxlsx::mergeCells(wb, "Legend", cols = 1:3, rows = 2)
  openxlsx::addStyle(wb, "Legend", wrap, rows = 2, cols = 1)
  openxlsx::setRowHeights(wb, "Legend", rows = 2, heights = 32)
  openxlsx::writeData(wb, "Legend", legend, startRow = 4, headerStyle = hdr)
  body_rows <- seq_len(nrow(legend)) + 4
  openxlsx::addStyle(wb, "Legend", wrap, rows = body_rows, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Legend", openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "top"),
                     rows = body_rows, cols = 1)
  openxlsx::setColWidths(wb, "Legend", cols = 1:3, widths = c(14, 22, 110))
  openxlsx::freezePane(wb, "Legend", firstActiveRow = 5)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}
