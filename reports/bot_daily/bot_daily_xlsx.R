# bot_daily_xlsx.R — BOT_daily workbook: Reports/bot_daily_<yyyymmdd>_<hhmm>.xlsx
#
# Sheet "Data": a tier column, then the columns read every day (BOT_READ_DEFAULT),
# rows shaded by tier, filter on the header row. Sheet "Detail": the tier and every
# field (BOT_READ_COLS); the BOT forward test (reports/bot_fwd) reads it. Sheet
# "Legend": the tier rules with today's counts, then every column defined in plain
# words, with no reference to other documents. The definitions describe
# shared/bot_read.R, zones.R and gates.R (docs/BOT_TOOLS_DESIGN.md section 3):
# change them together. write_bot_daily_xlsx() stops if a column has no legend row.
#
# Tiers are a highlighting rule chosen by the user on 2026-09-23 ("trend first"),
# not a ranking. Same rule as bot_fwd/common.py::tier_reason. Colours are blue / amber /
# orange, not green / red: the user is colour-blind.
#
# No COUNTER-TREND tier since 2026-10-08 (user: a falling name with a far target is
# not a BOT long entry without positive price action — "don't catch a falling
# knife"); those rows are WATCH. The forward test still simulates them, by reason.

BOT_TIERS <- data.frame(stringsAsFactors = FALSE,
  tier = c("BOT", "BOT-", "WATCH", "LOW", "VETO"),
  fill = c("#9DC3E6", "#DDEBF7", "#FFF2CC", "#F2F2F2", "#F4B183"),
  rule = c(
    "tradable = 1, trend_state at least 4/6, asym at least 1.5",
    paste("tradable = 1, asym at least 1, and either trend_state at least 4/6 with asym below 1.5,",
          "or a daily trend paused inside an intact trend: trend_state at most 3/6, w_trend_state at least 4/6,",
          "close above a rising 50-day EMA (ema50_disp_pct and ema50_slope above 0; below a falling one for a short)"),
    paste("tradable = 1, asym at least 1, none of the above: reward/risk is there but the trend is not.",
          "A level to watch, not an entry: a long needs positive price action first (a higher low, a reclaimed level)"),
    "tradable = 1, asym below 1 or empty",
    "tradable = 0: see veto_reason"))
TIER_TREND_MIN <- 4; TIER_ASYM_BOT <- 1.5; TIER_ASYM_BOT_MINUS <- 1

trend_n <- function(s) suppressWarnings(as.integer(sub("^\\s*(\\d+)\\s*/\\s*6.*$", "\\1", s)))

# Daily trend paused inside an intact trend (user, 2026-10-08: NET, a flag breakout
# drifting sideways above the broken level, read 2/6 daily and was then COUNTER-TREND):
# weekly trend at least 4/6 and close above a rising daily EMA50 (short: mirrored).
# Such a row is BOT- whatever its asym above 1. Same rule as
# bot_fwd/common.py::weekly_trend_hold, which also records the reason per signal.
weekly_trend_hold <- function(df) {
  w <- trend_n(df$w_trend_state)
  sgn <- ifelse(df$direction == "short", -1, 1)
  disp <- sgn * suppressWarnings(as.numeric(df$ema50_disp_pct))
  slope <- sgn * suppressWarnings(as.numeric(df$ema50_slope))
  !is.na(w) & w >= TIER_TREND_MIN & !is.na(disp) & disp > 0 & !is.na(slope) & slope > 0
}

bot_tier <- function(df) {
  n <- trend_n(df$trend_state)
  a <- suppressWarnings(as.numeric(df$asym))
  trend <- !is.na(n) & n >= TIER_TREND_MIN
  ok <- !is.na(a)
  hold <- weekly_trend_hold(df)
  ifelse(df$tradable == 0, "VETO",
  ifelse(trend & ok & a >= TIER_ASYM_BOT, "BOT",
  ifelse(ok & a >= TIER_ASYM_BOT_MINUS & (trend | hold), "BOT-",
  ifelse(ok & a >= TIER_ASYM_BOT_MINUS, "WATCH", "LOW"))))
}

# Column, unit / values, definition. Order = BOT_READ_COLS, tier first.
# "Target side" / "stop side": above / below the price for a long, mirrored for a short.
BOT_DAILY_LEGEND <- data.frame(stringsAsFactors = FALSE, check.names = FALSE, matrix(byrow = TRUE, ncol = 3, dimnames = list(NULL, c("Column", "Unit / values", "Definition")), c(
  "tier", "BOT / BOT- / WATCH / LOW / VETO",
  "Highlight tier of the row, from the rules in the table above. A colour code for reading, not a ranking.",

  # Identity and price
  "date", "date",
  "Session the row refers to: the last completed daily bar of the name.",
  "bar_lag", "trading days",
  "Trading days of the name's exchange between date and today, both excluded. 0 = the last bar is today's or the previous session's; above 0 = the price history is stale.",
  "bar_source", "yahoo / ibkr",
  "Where the last daily bar comes from: yahoo, or ibkr when Yahoo Finance had not yet delivered that session and the bar was taken from Interactive Brokers.",
  "name", "text",
  "Ticker of the name as used in the trading database and at Interactive Brokers.",
  "yahoo", "text",
  "Ticker of the name on Yahoo Finance, the source of the price history.",
  "direction", "long / short",
  "Direction the row is read for. Every direction-dependent column (target, stop, distances, gates) is mirrored for a short.",
  "tradable", "1 / 0",
  "1 = the row can be traded today. 0 = vetoed; the reason is in veto_reason.",
  "veto_reason", "text",
  "Why tradable = 0. gap_through_stop = a large overnight gap (95th percentile, see gap_vs_stop) would jump over the whole stop distance. against_trend = for a long, the price is below both the daily and the weekly 50-period exponential moving average (for a short, above both). Both reasons joined with + when both apply; empty when tradable = 1.",
  "zone_state", "in_resistance / in_support / in_both / empty",
  "Whether today's price stands inside a resistance zone, a support zone or both (zones: see res_zone_lo). Informational; it no longer vetoes.",
  "px", "price",
  "Last daily close.",
  "atr", "price",
  "Average true range over 14 sessions (Wilder smoothing) on the dividend-adjusted daily prices: the typical daily range, in price units. Distances in this sheet are often expressed in multiples of it.",
  "atr_pct", "%",
  "atr as a percentage of px.",
  "atr_pctile", "0 to 100",
  "Where today's atr_pct ranks within the name's own last 504 sessions (about 2 years). 100 = the name has never been this volatile over that period. Empty with fewer than 250 sessions of history.",
  "prior20_atr", "ATR multiples",
  "Price change over the last 20 sessions divided by atr, signed: how far the name already ran before today, in ATR.",
  "entry_factors", "0 to 2",
  "Count of two conditions: atr_pctile at least 75 (volatility high for the name) and prior20_atr at least 2 in absolute value (a large run over 20 sessions).",

  # Pivots and range position
  "zz_th", "fraction",
  "Reversal threshold used to find turning points (pivots): 2 x atr / px, held between 0.025 and 0.100. A pivot is confirmed when the price reverses by this fraction from a running high or low.",
  "n_pivots", "count",
  "Number of confirmed pivots (turning points, taken on daily highs and lows) in the price history read.",
  "rng_pct_20", "0 to 100",
  "Position of the close inside the high-low range of the last 20 sessions. 0 = at the 20-session low, 100 = at the 20-session high.",
  "rng_dyn", "0 to 100",
  "Position of px between the centre of the stop-side zone (0) and the centre of the target-side zone (100); the target replaces the target-side zone when there is none. Empty when there is no stop-side zone.",
  "zone_window_sessions", "sessions",
  "Sessions from the oldest pivot of the two zones (res_first, sup_first) to date: the lookback behind rng_dyn.",

  # Resistance zone (target side for a long)
  "res_zone_lo", "price",
  "Lower edge of the selected resistance zone: the lowest pivot of a cluster of pivots at similar prices, minus 0.35 x atr. For a long, the first price met on the way up. Resistance and support columns follow the trade's axis: for a short, res_ columns describe the zone below the price, which the short targets.",
  "res_zone_hi", "price",
  "Upper edge of that zone: the highest pivot of the cluster plus 0.35 x atr.",
  "res_touches", "count",
  "Number of pivots in that zone (2 or more).",
  "res_first", "date",
  "Date of the oldest pivot in that zone.",
  "res_last", "date",
  "Date of the newest pivot in that zone.",
  "res_dist_pct", "%",
  "Distance from px to the edge of the zone the target sits on, in percent of px.",
  "res_dist_atr", "ATR multiples",
  "The same distance in multiples of atr.",
  "res_pct_of_em10", "%",
  "The same distance as a percentage of the name's large 10-session move (px x em10_hi / 100). 100 = the target needs a 90th-percentile 10-session move.",
  "res2_zone_lo", "price",
  "Lower edge of the next zone beyond the selected one on the target side. Empty when there is none.",
  "res2_zone_hi", "price",
  "Upper edge of that next zone.",
  "res2_touches", "count",
  "Number of pivots in that next zone.",
  "n_res_to_ext", "count",
  "Number of resistance zones between px and the 1.272 Fibonacci extension (fib_ext_1272), the selected zone included. Empty when there is no extension.",

  # Support zone (stop side for a long)
  "sup_zone_lo", "price",
  "Lower edge of the selected support zone: lowest pivot of the cluster minus 0.35 x atr. For a long, the stop side.",
  "sup_zone_hi", "price",
  "Upper edge of that zone: highest pivot plus 0.35 x atr. For a long, the first price met on the way down.",
  "sup_touches", "count",
  "Number of pivots in that zone.",
  "sup_first", "date",
  "Date of the oldest pivot in that zone.",
  "sup_last", "date",
  "Date of the newest pivot in that zone.",
  "sup_dist_pct", "%",
  "Distance from px down to the near edge of that zone, in percent of px.",
  "sup_dist_atr", "ATR multiples",
  "The same distance in multiples of atr.",
  "sup_pct_of_em10", "%",
  "The same distance as a percentage of the name's large 10-session move (px x em10_hi / 100).",

  # Fibonacci, current leg
  "leg_low", "price",
  "Price of the most recent confirmed pivot low: the start of the current up-leg.",
  "leg_anchor_date", "date",
  "Date of that pivot low.",
  "leg_high", "price",
  "Highest high since leg_anchor_date: the top of the current leg so far.",
  "fib_ret_382", "price",
  "38.2% retracement of the leg: leg_high - 0.382 x (leg_high - leg_low).",
  "fib_ret_500", "price",
  "50% retracement of the leg.",
  "fib_ret_618", "price",
  "61.8% retracement of the leg: the deepest retracement, used as a stop candidate.",
  "fib_ext_1272", "price",
  "1.272 extension of the leg, measured from the leg low: leg_low + 1.272 x (leg_high - leg_low). Used as the target when no zone lies ahead.",
  "fib_ext_1618", "price",
  "1.618 extension of the leg: leg_low + 1.618 x (leg_high - leg_low).",

  # Target, stop, asymmetry
  "target", "price",
  "Price objective of the trade, from target_source, and never farther than the name's large 10-session move from px (90th percentile, see em10_hi; 10th percentile em10_lo for a short).",
  "target_source", "zone / flip_zone / in_zone / fib_ext / em10_cap / none",
  "Where the target comes from. zone = near edge of the nearest zone ahead built from target-side pivots (highs for a long). flip_zone = near edge of the nearest zone ahead built from the other side's pivots (an old support above the price for a long). in_zone = far edge of the zone the price stands inside. fib_ext = fib_ext_1272, when no zone lies ahead. em10_cap = px plus the large 10-session move, used when any of the above lies farther. none = no level found.",
  "target_agree", "1 / 0",
  "1 when fib_ext_1272 falls inside the selected target-side zone (two methods agree on the target). Empty when either is missing.",
  "stop", "price",
  "Stop level of the trade, from stop_source, then held between 1.0 and 2.5 atr from px. Candidates in order: far edge of the nearest stop-side zone within 3 atr, else fib_ret_618 within 3 atr, else px - 3 x atr; a level closer than 0.5 atr is not used.",
  "stop_source", "zone_stop / flip_zone_stop / in_zone_stop / fib_retr_stop / atr_stop / max_stop / min_stop",
  "Where the stop comes from. zone_stop = stop-side zone built from stop-side pivots (lows for a long). flip_zone_stop = stop-side zone built from the other side's pivots (an old resistance below the price for a long). in_zone_stop = beyond the far edge of the zone the price stands inside. fib_retr_stop = fib_ret_618. atr_stop = px - 3 x atr. max_stop = px - 2.5 x atr, replacing a farther level. min_stop = px - 1.0 x atr, replacing a nearer level. Mirrored for a short.",
  "stop_agree", "1 / 0",
  "1 when one of the Fibonacci retracements (38.2, 50, 61.8%) falls inside the selected stop-side zone.",
  "level_basis", "zone / geometric / mixed",
  "zone = both target and stop rest on a price zone (target_source zone, flip_zone or in_zone and stop_source zone_stop, flip_zone_stop or in_zone_stop). geometric = neither does (Fibonacci or ATR levels only). mixed = one of the two.",
  "gap_p95_pct", "%",
  "Large overnight gap of the name: 95th percentile of the absolute move from the previous close to the next open over the last 504 sessions, in percent of price. Empty with fewer than 200 observations.",
  "gap_vs_stop", "ratio",
  "Share of the stop distance (px - stop) that a large overnight gap (gap_p95_pct) covers. At 1 or more, a gap can jump the whole stop and the row is vetoed (veto_reason gap_through_stop).",
  "asym", "ratio",
  "Reward-to-risk of the trade: (target - px) / (px - stop); for a short (px - target) / (stop - px). The column the sheet is sorted on. Empty when target or stop is on the wrong side of px.",
  "asym_fib", "ratio",
  "The same ratio with Fibonacci levels only: (fib_ext_1272 - px) / (px - fib_ret_618). Empty when fib_ret_618 is at or above px.",
  "sess_target_p90", "sessions",
  "Sessions after which a winning trade has touched the target: 9 out of 10 winners of this name get there by then, from the name's history of moves measured in ATR. An option expiring earlier leaves no winning path.",
  "sess_target_p75", "sessions",
  "Sessions after which a good trade has touched the target: 3 out of 4 winners get there by then. An option expiring before this leaves only the fast winners.",
  "em10_lo", "%",
  "Large down-move of the name over 10 sessions: the 10th percentile of its 10-session moves over 8 years, rescaled to today's volatility (atr_pct). Negative.",
  "em10_hi", "%",
  "Large up-move of the name over 10 sessions: the 90th percentile, same construction. px x em10_hi / 100 is the move in price that caps the target.",

  # Moving averages
  "ema50", "price",
  "50-day exponential moving average of the close.",
  "ema50_disp_pct", "%",
  "Distance of px from ema50, in percent of ema50. Positive = above.",
  "ema50_slope", "%",
  "Change of ema50 over the last 5 sessions, in percent. Positive = rising.",
  "w_ema50", "price",
  "50-week exponential moving average, on weekly bars built from the daily prices.",
  "w_ema50_disp_pct", "%",
  "Distance of px from w_ema50, in percent of w_ema50.",

  # Gate inputs
  "d_squeeze", "ratio",
  "Daily range compression: high-low range of the last 20 sessions divided by that of the last 40. Below 0.65 = compressed (gate S5).",
  "w_squeeze", "ratio",
  "The same on weekly bars (20 and 40 weeks).",
  "d_vol_decline", "ratio",
  "Average volume of the last 20 sessions divided by that of the last 50. Below 0.95 = volume drying up (gate S6).",
  "w_vol_decline", "ratio",
  "The same on weekly bars.",
  "d_vol_surge", "ratio",
  "Volume of the last session divided by its 20-session average. 1.2 or more = volume surge (gate BK4).",
  "w_vol_surge", "ratio",
  "The same on weekly bars.",
  "obv_slope", "shares",
  "Net signed volume over the last 20 sessions: volume counted positive on up days and negative on down days (on-balance volume, change over 20 sessions). Positive for a long = gate S4.",
  "obv_slope_days", "days of volume",
  "obv_slope divided by the 20-session average volume: the same quantity in days of average volume, comparable across names. Range roughly -20 to +20.",
  "rsi14", "0 to 100",
  "Relative strength index over 14 sessions (Wilder smoothing). Above 50 = more up than down momentum.",
  "rsi_slope", "points",
  "Change of rsi14 over the last 5 sessions.",
  "updn_ratio", "ratio",
  "Volume on up days divided by volume on down days over the last 10 sessions. Above 1.1 for a long = gate BK2. Empty when there was no down day.",
  "ret20", "%",
  "Price change over the last 20 sessions, on dividend-adjusted closes, in percent.",
  "rs20", "percentage points",
  "Relative strength: ret20 minus the 20-session return of the benchmark in rs_bench. Positive = the name outperformed (gate S3, reported but not counted in trend_state).",
  "rs_bench", "text",
  "Benchmark of rs20. peers:<group> = the median 20-session return of the other members of the name's correlation group (a member moving more than 50% is left out). Otherwise a fixed benchmark symbol for a name without a group. Empty when none applies.",
  "group", "text",
  "The name's correlation group: a set of names whose daily returns move together. Empty when the name has no group.",
  "grp_rs", "percentage points",
  "20-session return of the name's group (median of the members) minus that of SPY (S&P 500 ETF).",
  "grp_rank", "1 to n",
  "Rank of grp_rs across all groups, 1 = strongest group over 20 sessions.",
  "adx10", "0 to 100",
  "Average directional index over 10 sessions: trend strength regardless of direction. Above about 25 = trending.",

  # Gates and states
  "trend_state", "n/6",
  "How many of six trend conditions hold, as \"4/6\". For a long: S1 close above ema50; S2 ema50 rising (ema50_slope > 0); S4 obv_slope > 0; BK1 rsi14 above 50 and rising; BK2 updn_ratio above 1.1; BK3 rng_pct_20 at least 70. Mirrored for a short (below, falling, under 0.9, at most 30).",
  "w_trend_state", "n/6",
  "The same six conditions on weekly bars. n/a when weekly bars are unavailable.",
  "compression_state", "n/1",
  "Range compression condition S5 (d_squeeze below 0.65), as \"1/1\" when it holds, \"0/1\" when not.",
  "supply_state", "n/2",
  "Volume conditions: S6 (d_vol_decline below 0.95, volume drying up) and BK4 (d_vol_surge at least 1.2, volume surge), as the count holding out of 2.",
  "rs_state", "+ / - / n/a",
  "+ = the name outperformed its benchmark over 20 sessions (rs20 above 0 for a long, below 0 for a short); - = it did not; n/a = no benchmark return.",
  "confluence", "k/3",
  "On how many of the three conditions S5, S6 and BK4 the daily and the weekly reading agree, as \"k/3\". n/a when weekly bars are unavailable.",

  # Carried from the ticker table
  "atr_band", "text",
  "Volatility class of the name from its 5-year median ATR%, as stored in the ticker table.",
  "gap_tercile", "text",
  "Tercile (low / mid / high) of the name's share of its daily move that happens overnight, as stored in the ticker table.",
  "note", "text",
  "Free-text remark on a condition worth noticing on this row."
)))

BOT_DAILY_NUM_FMT <- c(px = "#,##0.00", atr = "#,##0.00", target = "#,##0.00", stop = "#,##0.00",
                       asym = "0.00", sess_target_p90 = "0.0", sess_target_p75 = "0.0",
                       atr_pctile = "0", tradable = "0")

#' Write the BOT_daily workbook (sheets Data, Detail, Legend)
#' @param df data.frame with BOT_READ_COLS, in reading order
#' @param path output .xlsx path
#' @param data_cols columns of the Data sheet (tier is added first)
#' @param run_time time shown in the legend header
write_bot_daily_xlsx <- function(df, path, data_cols = BOT_READ_DEFAULT, run_time = Sys.time()) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- cbind(tier = bot_tier(df), df, stringsAsFactors = FALSE)
  undefined <- setdiff(names(df), BOT_DAILY_LEGEND$Column)
  if (length(undefined)) stop("BOT_DAILY_LEGEND (bot_daily_xlsx.R) has no definition for column(s): ",
                              paste(undefined, collapse = ", "))
  data <- df[, c("tier", data_cols), drop = FALSE]

  hdr <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#404040",
                               halign = "center")
  wrap <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  wb <- openxlsx::createWorkbook()

  add_sheet <- function(sheet, x) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, x, headerStyle = hdr, withFilter = TRUE)
    rows <- seq_len(nrow(x)) + 1
    for (t in BOT_TIERS$tier) {
      r <- rows[x$tier == t]
      if (length(r)) openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fgFill = BOT_TIERS$fill[BOT_TIERS$tier == t]),
                                        rows = r, cols = seq_along(x), gridExpand = TRUE, stack = TRUE)
    }
    for (col in intersect(names(BOT_DAILY_NUM_FMT), names(x)))
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = BOT_DAILY_NUM_FMT[[col]]),
                         rows = rows, cols = match(col, names(x)), stack = TRUE)
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(textDecoration = "bold"),
                       rows = rows, cols = match(c("tier", "name"), names(x)), gridExpand = TRUE, stack = TRUE)
    widths <- vapply(names(x), function(c)
      min(32, max(7, nchar(c) + 2, max(nchar(utils::head(format(x[[c]]), 200)), 0) + 2)), numeric(1))
    widths[c("tier", "name")] <- widths[c("tier", "name")] + 4
    openxlsx::setColWidths(wb, sheet, cols = seq_along(x), widths = widths)
    openxlsx::freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = match("name", names(x)) + 1)
  }
  add_sheet("Data", data)
  add_sheet("Detail", df)

  openxlsx::addWorksheet(wb, "Legend")
  session <- if (nrow(df) && "date" %in% names(df)) paste0(", session ", df$date[1]) else ""
  openxlsx::writeData(wb, "Legend", sprintf("BOT daily - run %s%s - column definitions",
                                            format(run_time, "%Y-%m-%d %H:%M"), session), startRow = 1)
  openxlsx::addStyle(wb, "Legend", openxlsx::createStyle(textDecoration = "bold", fontSize = 12), rows = 1, cols = 1)
  openxlsx::writeData(wb, "Legend", paste(
    "One row per name and direction from the BOT breakout scan, on daily prices (price and volume only, no option data).",
    "Sheet Data holds the columns read every day; sheet Detail holds every column. Rows are in reading order:",
    "tradable rows first, then asym from highest to lowest, then sess_target_p75 from lowest. This is a reading order,",
    "not a ranking: no combination of the conditions below has shown measurable power to rank outcomes.",
    "The strategy's edge is reward-to-risk (asym) over many trades, with a win rate below 50%."), startRow = 2)
  openxlsx::mergeCells(wb, "Legend", cols = 1:3, rows = 2)
  openxlsx::addStyle(wb, "Legend", wrap, rows = 2, cols = 1)
  openxlsx::setRowHeights(wb, "Legend", rows = 2, heights = 62)

  tiers <- data.frame(Tier = BOT_TIERS$tier, Rows = vapply(BOT_TIERS$tier, function(t) sum(df$tier == t), numeric(1)),
                      Rule = BOT_TIERS$rule, check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Legend", tiers, startRow = 4, headerStyle = hdr)
  # The rule runs over the two right-hand columns (unit and definition below).
  for (i in 0:nrow(tiers)) openxlsx::mergeCells(wb, "Legend", cols = 3:4, rows = 4 + i)
  for (i in seq_len(nrow(tiers)))
    openxlsx::addStyle(wb, "Legend", openxlsx::createStyle(fgFill = BOT_TIERS$fill[i], wrapText = TRUE, valign = "top"),
                       rows = 4 + i, cols = 1:4, gridExpand = TRUE)

  legend <- BOT_DAILY_LEGEND[match(names(df), BOT_DAILY_LEGEND$Column), ]
  legend <- cbind(legend[, 1, drop = FALSE],
                  Sheet = ifelse(legend$Column %in% names(data), "Data, Detail", "Detail"),
                  legend[, 2:3], stringsAsFactors = FALSE)
  start <- 4 + nrow(tiers) + 2
  openxlsx::writeData(wb, "Legend", legend, startRow = start, headerStyle = hdr)
  body_rows <- seq_len(nrow(legend)) + start
  openxlsx::addStyle(wb, "Legend", wrap, rows = body_rows, cols = 1:4, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Legend", openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "top"),
                     rows = body_rows, cols = 1)
  openxlsx::setColWidths(wb, "Legend", cols = 1:4, widths = c(20, 13, 26, 110))

  tmp <- paste0(path, ".tmp.xlsx")
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Cannot write ", path, " (open in Excel?) - workbook left at ", tmp)
  invisible(table(factor(df$tier, levels = BOT_TIERS$tier)))
}
