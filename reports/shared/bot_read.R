# reports/shared/bot_read.R — BOT_daily's per-name read, callable from any tool.
#
# One row of bot_daily_<date>_<hhmm>.xlsx for one name and direction, as specified in
# docs/BOT_TOOLS_DESIGN.md section 3. BOT_daily loops it over the universe;
# /analyze calls it for the name it reports on, so both tools read a name
# through the same function.
#
# The caller sources indicators.R, weekly.R, zones.R and gates.R first.
# Names carry a bot_/BOT_/.br_ prefix because /analyze sources many shared
# files into one environment and already defines its own .pct at top level.

.bot_read_deps <- c("calc_ind", "get_last", "to_weekly", "level_read",
                    "zone_ref", "eval_gates", "gate_inputs", "cluster_states",
                    "confluence_state")

BOT_EM_DAYS    <- 10
# Fetch 5 years so the weekly resample has enough bars: calc_ind() needs 130
# rows, and 2 years of daily gives only ~104 weeks. Zones stay on the last 2
# years, which is the window the design document's trial was run on.
BOT_FETCH_YEARS <- 5
BOT_ZONE_YEARS  <- 2

# Dividend-adjusted OHLC. Levels are compared with today's price, and after an
# ex-date the price has dropped by the amount paid, so a level from before it
# is only comparable once adjusted. TDG's special dividend (ex 2025-09-02,
# factor 0.9357) left every earlier pivot 6.9% too high against the chart.
# Adjusting also removes the ex-date gap from ATR and from the overnight-gap
# measure. The last bar is unaffected (factor 1), so `px` is the traded price.
bot_fetch_daily <- function(sym, adjusted = TRUE, ib_name = NULL, market = NULL) {
  d <- tryCatch(getSymIntervalDate(sym, Sys.Date() - round(BOT_FETCH_YEARS * 365), Sys.Date()),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) < 130) return(NULL)
  d <- d[is.finite(d$Close), , drop = FALSE]
  d$bar_source <- "yahoo"
  if (!is.null(ib_name)) d <- bot_fill_from_ibkr(d, ib_name, market = market)
  if (adjusted && "Adjusted" %in% names(d)) {
    f <- d$Adjusted / d$Close
    f[!is.finite(f) | f <= 0] <- 1
    for (k in c("Open", "High", "Low", "Close")) d[[k]] <- d[[k]] * f
  }
  calc_ind(d)
}

.br_n <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) NA_real_ else x
.br_pct <- function(num, den) if (is.finite(num) && is.finite(den) && den != 0) num / den * 100 else NA_real_

# Tickers columns written by BOT_monthly (touch_coefs), named by the row field.
TOUCH_COLS <- c(touch_up75 = "ATR_TouchCoefUp75", touch_up90 = "ATR_TouchCoefUp90",
                touch_dn75 = "ATR_TouchCoefDn75", touch_dn90 = "ATR_TouchCoefDn90")

#' Universe rows for explicitly named symbols, with their Tickers attributes.
#'
#' A named symbol used to get NA for atr_band and gap_tercile and its own name
#' as the Yahoo symbol, even when Tickers knows both (TODO 93.5). Names absent
#' from Tickers keep that fallback, so a raw Yahoo symbol still works.
#'
#' @param names character vector of Tickers.Name values (or Yahoo symbols)
#' @return data.frame(name, yahoo, atr_band, gap_tercile, bench, bench_peers)
bot_read_ticker_rows <- function(names) {
  out <- data.frame(name = names, yahoo = names, atr_band = NA_character_,
                    gap_tercile = NA_character_, coef_hi = NA_real_, coef_lo = NA_real_,
                    touch_up75 = NA_real_, touch_up90 = NA_real_,
                    touch_dn75 = NA_real_, touch_dn90 = NA_real_,
                    bench = NA_character_, bench_peers = NA_character_,
                    stringsAsFactors = FALSE)
  tk <- tryCatch({
    conn <- Tdata::safe_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    have <- names(DBI::dbGetQuery(conn, "SELECT * FROM Tickers LIMIT 1"))
    DBI::dbGetQuery(conn, sprintf(
      "SELECT Name, YahooName, ATR_Band, GapShare_Tercile, ATR_MoveCoefHi, %s AS ATR_MoveCoefLo,
              %s AS BOT_Bench, %s
         FROM Tickers WHERE Name IN (%s)",
      if ("ATR_MoveCoefLo" %in% have) "ATR_MoveCoefLo" else "NULL",
      if ("BOT_Bench" %in% have) "BOT_Bench" else "NULL",
      paste(sprintf("%s AS %s", ifelse(TOUCH_COLS %in% have, TOUCH_COLS, "NULL"), TOUCH_COLS),
            collapse = ", "),
      paste(rep("?", length(names)), collapse = ",")), params = as.list(names))
  }, error = function(e) NULL)
  if (is.null(tk) || !nrow(tk)) return(out)
  i <- match(out$name, tk$Name); ok <- !is.na(i)
  yh <- tk$YahooName[i[ok]]
  out$yahoo[ok] <- ifelse(!is.na(yh) & nzchar(yh), yh, out$name[ok])
  out$atr_band[ok] <- tk$ATR_Band[i[ok]]
  out$gap_tercile[ok] <- tk$GapShare_Tercile[i[ok]]
  out$coef_hi[ok] <- suppressWarnings(as.numeric(tk$ATR_MoveCoefHi[i[ok]]))
  out$coef_lo[ok] <- suppressWarnings(as.numeric(tk$ATR_MoveCoefLo[i[ok]]))
  out$bench[ok] <- as.character(tk$BOT_Bench[i[ok]])
  for (k in seq_along(TOUCH_COLS))
    out[[names(TOUCH_COLS)[k]]][ok] <- suppressWarnings(as.numeric(tk[[TOUCH_COLS[k]]][i[ok]]))
  .bot_group_bench(out)
}

#' S3 benchmark from the name's correlation group (ScannerUniverse.Cluster /
#' ClusterETF), replacing Tickers.BOT_Bench where a group exists.
#'
#' Over the 10-02 universe the group fitted a name's daily returns better than
#' BOT_Bench: median correlation 0.70 for the anchor and 0.72 for the peer
#' basket, against 0.63 for BOT_Bench (60 sessions; 0.69 / 0.70 / 0.62 over
#' 250). Only 79 of 258 names had BOT_Bench equal to their anchor, and 61
#' grouped names had no BOT_Bench at all, so S3 abstained for them.
#'
#' - Grouped names: the median 20-session return of the other members
#'   (bot_row_bench_ret20), whatever the anchor. The name is left out, or rs20
#'   would partly measure it against itself. An anchor ETF outside the group
#'   is not used: it is the nearest universe ETF, a proxy that can serve two
#'   groups (ITA for defence primes and commercial aerospace) or a different
#'   industry (ITB for machinery). The macro report's sector map reads the
#'   same baskets (macro_context/intermarket.R::group_map()).
#' - Ungrouped names, names outside ScannerUniverse, and the only name of a
#'   one-name group: Tickers.BOT_Bench.
#'
#' `bench` becomes "peers:<group>" and `bench_peers` the other members' Yahoo
#' symbols, comma-separated; `group` is the name's correlation group (NA when
#' ungrouped).
.bot_group_bench <- function(out) {
  out$group <- NA_character_
  sc <- .bot_groups_table()
  if (is.null(sc)) return(out)
  for (j in seq_len(nrow(out))) {
    k <- match(out$name[j], sc$Symbol)
    if (is.na(k)) next
    grp <- sc$Cluster[k]
    out$group[j] <- grp
    peers <- sc$yh[sc$Cluster == grp & sc$Symbol != out$name[j]]
    if (!length(peers)) next   # a one-name group keeps BOT_Bench
    out$bench[j] <- paste0("peers:", grp)
    out$bench_peers[j] <- paste(peers, collapse = ",")
  }
  out
}

#' Grouped scanner names with their Yahoo symbols, or NULL.
.bot_groups_table <- function() {
  sc <- tryCatch({
    conn <- Tdata::safe_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    DBI::dbGetQuery(conn,
      "SELECT s.Symbol, s.Cluster, t.YahooName
         FROM ScannerUniverse s
         LEFT JOIN Tickers t ON t.Name = s.Symbol
        WHERE s.IsActive = 1 AND s.Cluster IS NOT NULL AND s.Cluster <> '' AND s.Cluster <> 'Ungrouped'")
  }, error = function(e) NULL)
  if (is.null(sc) || !nrow(sc)) return(NULL)
  sc$yh <- ifelse(!is.na(sc$YahooName) & nzchar(sc$YahooName), sc$YahooName, sc$Symbol)
  sc
}

#' Rotation rank of every correlation group: the group's 20-session return
#' minus SPY's, ranked across all groups (1 = strongest).
#'
#' The group's return follows the S3 rule: the median of the members' (each
#' within +/-50%), whatever the anchor.
#' Measured 2026-10-05 (NewTrading/Strategies/Breakouts/group_rotation_test.py,
#' 319 names, 55 groups, every 5th session over 5 years): names in a top-3
#' group moved +0.30 ATR more over the next 10 sessions than names outside
#' the top 6 (t 2.3 on non-overlapping windows; +0.36, t 2.7, for names above
#' a rising EMA50), while the hit rate of +1.5 ATR before -1.5 ATR rose only
#' 3-5 points (t 1.0-2.0). Reported, not gated. That run used the outside
#' anchor ETF where one existed; rerun the same day with member medians for
#' every group, top 3 vs rest read +0.32 ATR (t 3.4; anchor rule +0.28, t 3.1
#' on that run's 245 dates).
#'
#' @return data.frame(group, grp_rs, grp_rank, n_groups), or NULL
bot_group_rotation <- function() {
  sc <- .bot_groups_table()
  if (is.null(sc)) return(NULL)
  spy <- bot_bench_ret20("SPY")
  if (!is.finite(spy)) return(NULL)
  grp <- unique(sc$Cluster)
  ret <- vapply(grp, function(g) {
    m <- sc[sc$Cluster == g, , drop = FALSE]
    v <- vapply(m$yh, bot_bench_ret20, numeric(1))
    v <- v[is.finite(v) & abs(v) <= BOT_PEER_RET20_MAX]
    if (length(v)) stats::median(v) else NA_real_
  }, numeric(1))
  rs <- ret - spy
  ok <- is.finite(rs)
  data.frame(group = grp, grp_rs = round(rs, 2),
             grp_rank = ifelse(ok, rank(-replace(rs, !ok, -Inf), ties.method = "min"), NA_integer_),
             n_groups = sum(ok), stringsAsFactors = FALSE)
}

.bot_bench_cache <- new.env(parent = emptyenv())

#' 20-session return of a benchmark, in percent, for S3 (rs20).
#'
#' The benchmark is a Yahoo symbol (SMH, ^SSMI, EXV1.DE…), fetched as such.
#' Dividend-adjusted like the names it is compared with. Cached per session:
#' a peer basket fetches each member once however many names it serves.
#'
#' @param bench Yahoo symbol, or NA
#' @return numeric, or NA_real_
bot_bench_ret20 <- function(bench) {
  if (is.null(bench) || length(bench) != 1 || is.na(bench) || !nzchar(bench)) return(NA_real_)
  if (!is.null(.bot_bench_cache[[bench]])) return(.bot_bench_cache[[bench]])
  bd <- tryCatch(getSymIntervalDate(bench, Sys.Date() - 120, Sys.Date(), sym_yahoo = bench),
                 error = function(e) NULL)
  cl <- if (is.null(bd) || !nrow(bd)) numeric(0)
        else if ("Adjusted" %in% names(bd)) bd$Adjusted else bd$Close
  cl <- cl[is.finite(cl)]
  v <- if (length(cl) <= 21) NA_real_ else (cl[length(cl)] / cl[length(cl) - 20] - 1) * 100
  .bot_bench_cache[[bench]] <- v
  v
}

#' Benchmark 20-session return for one row of bot_read_ticker_rows(): the
#' median of the peers' returns when the benchmark is a basket, else the
#' benchmark symbol's return.
#'
#' A peer moving more than BOT_PEER_RET20_MAX percent in 20 sessions is left
#' out: on 2026-10-02 CTVA read -86.5% on Yahoo (an unadjusted corporate
#' action) and, averaged with one other peer, put MOS at rs20 +43.8. The
#' median keeps one odd peer from setting the benchmark in larger groups.
BOT_PEER_RET20_MAX <- 50
bot_row_bench_ret20 <- function(row) {
  peers <- if ("bench_peers" %in% names(row)) row$bench_peers[1] else NA_character_
  if (is.na(peers) || !nzchar(peers)) return(bot_bench_ret20(row$bench[1]))
  v <- vapply(strsplit(peers, ",", fixed = TRUE)[[1]], bot_bench_ret20, numeric(1))
  v <- v[is.finite(v) & abs(v) <= BOT_PEER_RET20_MAX]
  if (length(v)) stats::median(v) else NA_real_
}


#' Append completed sessions Yahoo has not delivered, from IBKR daily bars.
#'
#' Yahoo returns an empty row for the last session of European listings for
#' most of the next day (UBSG, TTE, BNP, SIE, NOVN on 2026-09-29: the 09-28 row
#' all NA), so their read lags a session. IBKR has the bar. Only sessions after
#' the last Yahoo bar and before today are appended — today's bar is partial.
#' The IBKR close is the last trade, not the official closing auction (within
#' ~0.5% on those names), and IBKR volume misses part of the auction and, for
#' US names, off-exchange volume (ratio ~0.56 on AAPL); volume is therefore
#' rescaled by the median IBKR/Yahoo ratio over the overlapping days, so the
#' volume gates read the added bar on Yahoo's scale. Adjusted = Close on the
#' new rows: the adjustment factor of the latest bars is 1.
#'
#' @param d data.frame from getSymIntervalDate(), NA-price rows removed
#' @param ib_name Tickers.Name, the symbol tdata_py resolves the contract from
#' @return d, with any appended rows marked bar_source = "ibkr"
bot_fill_from_ibkr <- function(d, ib_name, market = NULL) {
  last <- as.Date(max(d$date))
  if (bot_bar_lag(last, market = market) == 0) return(d)
  py <- tryCatch(Tdata::tdata_py, error = function(e) NULL)
  if (is.null(py)) return(d)
  b <- tryCatch(py$get_historical_bars(ib_name, duration = "15 D", bar_size = "1 day"),
                error = function(e) NULL)
  if (is.null(b)) return(d)
  b <- as.data.frame(b)
  if (!nrow(b) || !all(c("datetime", "open", "high", "low", "close", "volume") %in% names(b))) return(d)
  b$date <- as.Date(b$datetime)
  ov <- merge(d[, c("date", "Volume")], b[, c("date", "volume")], by = "date")
  ratio <- stats::median(ov$volume / ov$Volume, na.rm = TRUE)
  if (!is.finite(ratio) || ratio <= 0) ratio <- 1
  new <- b[b$date > last & b$date < Sys.Date() & is.finite(b$close), , drop = FALSE]
  if (!nrow(new)) return(d)
  add <- d[rep(nrow(d), nrow(new)), , drop = FALSE]
  add$date <- if (inherits(d$date, "Date")) new$date else as.character(new$date)
  add$Open <- new$open; add$High <- new$high; add$Low <- new$low; add$Close <- new$close
  if ("Adjusted" %in% names(add)) add$Adjusted <- new$close
  add$Volume <- round(new$volume / ratio)
  add$bar_source <- "ibkr"
  out <- rbind(d, add)
  out[order(as.Date(out$date)), , drop = FALSE]
}

#' Weekdays missing between the last daily bar and today.
#'
#' 0 when the bar is today's (partial, intraday) or the previous weekday's.
#' Yahoo can return an empty row for the last session, which the fetch drops
#' silently (EL, 2026-09-22), so the read would be priced a session late with
#' nothing but the `date` column to show it. Exchange holidays count as
#' missing weekdays; a lag of 1 on the day after a holiday is expected.
#'
#' With `market` given and shared/market_calendar.R sourced, the count uses
#' that listing's business days, so an exchange holiday is not a missing
#' session; otherwise it counts weekdays.
#'
#' @param bar_date last bar date (Date or ISO string)
#' @param today reference date
#' @param market MARKETS key of the listing (market_calendar.R), or NULL
#' @return integer
bot_bar_lag <- function(bar_date, today = Sys.Date(), market = NULL) {
  bar_date <- as.Date(bar_date)
  if (is.na(bar_date) || bar_date >= today - 1) return(0L)
  if (!is.null(market) && exists("market_days_between", mode = "function"))
    return(market_days_between(market, bar_date, today))
  days <- seq(bar_date + 1, today - 1, by = "day")
  sum(!format(days, "%u") %in% c("6", "7"))
}

.bot_listing <- function(yahoo) if (exists("stock_market_of", mode = "function")) stock_market_of(yahoo) else NULL

bot_read_row <- function(row, direction, bench_ret20, ibkr_fill = FALSE, rotation = NULL) {
  missing <- .bot_read_deps[!vapply(.bot_read_deps, exists, logical(1), mode = "function")]
  if (length(missing))
    stop("bot_read_row() needs these sourced by the caller: ", paste(missing, collapse = ", "))
  listing <- .bot_listing(row$yahoo)
  d <- bot_fetch_daily(row$yahoo, ib_name = if (isTRUE(ibkr_fill)) row$name else NULL, market = listing)
  if (is.null(d)) return(NULL)
  last <- get_last(list(x = d), "x")
  if (is.null(last) || !nrow(last)) return(NULL)

  px  <- as.numeric(tail(d$Close, 1))
  atr <- as.numeric(tail(d$atr14, 1))
  if (!is.finite(px) || !is.finite(atr) || atr <= 0) return(NULL)

  # Expected move: the denominator for every "% of em10" - the 90th-percentile
  # 10-session move, the size of a winning BOT trade (spec §3.7).
  # em10 = today's ATR% x sqrt(10) x the name's 10-session move coefficient
  # (10th / 90th percentile of its standardised signed moves over 8 years).
  # The coefficient is a ratio, so it moves little in a month and dividend
  # adjustment leaves it unchanged: BOT_monthly stores it in Tickers and this
  # reads it, instead of re-fetching 8 years per name every day (~25% of the
  # run). Named symbols without a stored coefficient fall back to the live
  # computation.
  atr_pct_now <- atr / px * 100
  k_em  <- atr_pct_now * sqrt(BOT_EM_DAYS)
  c_hi  <- .br_n(suppressWarnings(as.numeric(row$coef_hi)))
  c_lo  <- .br_n(suppressWarnings(as.numeric(row$coef_lo)))
  need  <- if (identical(direction, "short")) c_lo else c_hi
  if (is.finite(need)) {
    em_hi <- if (is.finite(c_hi)) round(c_hi * k_em, 2) else NA_real_
    em_lo <- if (is.finite(c_lo)) round(c_lo * k_em, 2) else NA_real_
  } else {
    em <- tryCatch(atr_expected_move(row$yahoo, BOT_EM_DAYS, conf = 0.80, spot = px),
                   error = function(e) NULL)
    em_lo <- .br_n(em$move_lower_pct); em_hi <- .br_n(em$move_upper_pct)
  }
  # Every level field is on the trade's axis (level_read(): `res` is the target
  # side, `sup` the stop side), so a short measures against the DOWN move.
  # move_lower_pct is signed negative.
  long <- !identical(direction, "short")
  sgn  <- if (long) 1 else -1
  em_t   <- if (long) em_hi else abs(em_lo)
  em_abs <- if (is.finite(em_t)) px * em_t / 100 else NA_real_

  # Zones read the last BOT_ZONE_YEARS of the series; the rest of the fetch exists
  # for indicator warm-up and for the weekly resample.
  zd <- d[as.Date(d$date) >= Sys.Date() - round(BOT_ZONE_YEARS * 365), , drop = FALSE]
  lr <- level_read(if (nrow(zd) >= 130) zd else d, atr, em_abs, direction = direction)

  # Weekly half of the daily-vs-weekly read, resampled from the full fetch.
  wk  <- to_weekly(d)
  wkc <- if (!is.null(wk) && nrow(wk) >= 130) calc_ind(wk) else NULL
  wlast <- if (!is.null(wkc)) get_last(list(x = wkc), "x") else NULL
  w_ema50 <- if (!is.null(wlast) && nrow(wlast)) .br_n(wlast$ma50) else NA_real_

  rs20 <- if (is.finite(.br_n(last$ret20)) && is.finite(bench_ret20))
            .br_n(last$ret20) - bench_ret20 else NA_real_
  gi <- gate_inputs(last, rs20)
  gd <- eval_gates(gi, px, direction)
  gw <- if (!is.null(wlast) && nrow(wlast)) eval_gates(gate_inputs(wlast), px, direction) else NULL
  cs <- cluster_states(gd)
  wi <- if (!is.null(wlast) && nrow(wlast)) gate_inputs(wlast) else NULL

  res <- lr$res; sup <- lr$sup; fb <- lr$fib
  res_ref <- zone_ref(res, if (long) "res" else "sup")
  sup_ref <- zone_ref(sup, if (long) "sup" else "res")

  # Entry factors F1/F2, ported from bot_scan_universe.py::score_one() when that
  # scanner was retired. Validated on 93 realised trades (bot_book_design
  # 20260827.md 9b): ATR in its own top quartile carried 20.7R of 42.9R, and a
  # prior move of 2-3 ATR returned 0.76R per trade. Reported, never gated.
  atr_hist <- utils::tail(d$atr14 / d$Close * 100, 504)
  atr_hist <- atr_hist[is.finite(atr_hist)]
  atr_pctile <- if (length(atr_hist) > 250) mean(atr_hist < atr_pct_now) * 100 else NA_real_
  # Signed, in ATR units: (Close_t - Close_t-20) / atr14. Not ret20% / atr%,
  # which carries a spurious C_t/C_t-20 factor and inflates large up-moves.
  prior20_atr <- if (nrow(d) > 20 && is.finite(atr) && atr > 0)
                   (px - d$Close[nrow(d) - 20]) / atr else NA_real_
  f1 <- is.finite(atr_pctile) && atr_pctile >= 75
  f2 <- is.finite(prior20_atr) && abs(prior20_atr) >= 2

  # Overnight gap risk, measured against THIS trade's stop rather than against
  # the name's peers. Tickers.GapShare is the overnight SHARE of variance and
  # #82 A.3 established it as a stable name attribute, but across these 169
  # rows it does not predict whether a gap clears the stop: Spearman 0.073,
  # inside one standard error of zero. What does is the stop distance itself
  # (Spearman -0.889), which the zone engine sets per session. So the quantity
  # is trade-specific and belongs here, not in monthly membership. gap_share
  # still tracks gap SIZE as designed (Spearman 0.477 against gap_p95_atr).
  gdat <- utils::tail(d, 505)   # NOT `gd` - that is the gate vector from eval_gates()
  r_on <- log(gdat$Open[-1] / gdat$Close[-nrow(gdat)])
  r_on <- r_on[is.finite(r_on)]
  gap_p95_pct <- if (length(r_on) >= 200)
                   unname(stats::quantile(abs(r_on), 0.95)) * 100 else NA_real_
  stop_dist <- sgn * (px - .br_n(lr$stop_px))
  gap_vs_stop <- if (is.finite(gap_p95_pct) && is.finite(stop_dist) && stop_dist > 0)
                   gap_p95_pct / 100 * px / stop_dist else NA_real_

  # Spot standing inside a zone is reported, not vetoed (TODO 94). Over 284
  # names, a long entered inside any zone reached +1.5 ATR before -1.5 ATR in
  # 53.3% of cases against 53.6% outside (difference -0.003, SE 0.009), while
  # the veto removed 46% of candidate entries; and price reacted at the
  # nearest zone no more than at a placebo level. The only entry veto left is
  # the per-trade gap risk below.
  zone_state <- if (isTRUE(lr$in_res_zone) && isTRUE(lr$in_sup_zone)) "in_both"
    else if (isTRUE(lr$in_res_zone)) "in_resistance"
    else if (isTRUE(lr$in_sup_zone)) "in_support" else ""
  veto <- character(0)
  # >= 1 means a 95th-percentile overnight move covers the whole stop, so the
  # stop is not a stop: price gaps through it instead of trading through it.
  if (isTRUE(gap_vs_stop >= 1)) veto <- c(veto, "gap_through_stop")
  # Price on the wrong side of both the daily and the weekly EMA50 is not a BOT
  # setup in this direction: a long there is buying a downtrend (TSN and EXC,
  # 2026-09-28: gapped down, at the 52-week low, short candidates rather than
  # longs). Needs both, so a pullback below the daily EMA50 inside a weekly
  # uptrend stays a candidate. No veto when either average is unavailable.
  e_d <- .br_n(gi$ema50)
  if (is.finite(e_d) && is.finite(w_ema50) &&
      sgn * (px - e_d) < 0 && sgn * (px - w_ema50) < 0) veto <- c(veto, "against_trend")

  # Winning interval: the sessions after which a good (p75) / winning (p90)
  # trade TOUCHES the target, (d / C)^2 with d the distance to it in ATR and C
  # the name's touch coefficient (spec 3.7). A vehicle that expires before
  # sess_target_p90 cannot win; one that expires before sess_target_p75 needs
  # a fast winner. Coefficients come from Tickers (BOT_monthly); a name without
  # them is measured on the bars already fetched.
  tc75 <- .br_n(suppressWarnings(as.numeric(if (long) row$touch_up75 else row$touch_dn75)))
  tc90 <- .br_n(suppressWarnings(as.numeric(if (long) row$touch_up90 else row$touch_dn90)))
  if (!is.finite(tc90) && exists("touch_coefs", mode = "function")) {
    tcl <- touch_coefs(d, n = BOT_EM_DAYS)
    tc75 <- if (long) tcl$up75 else tcl$dn75; tc90 <- if (long) tcl$up90 else tcl$dn90
  }
  d_tgt <- sgn * (.br_n(lr$target) - px) / atr
  sess_at <- function(cq) if (is.finite(d_tgt) && d_tgt > 0 && is.finite(cq) && cq > 0)
                           round((d_tgt / cq)^2, 1) else NA_real_

  list(
    date = as.character(as.Date(tail(d$date, 1))),
    bar_lag = bot_bar_lag(tail(d$date, 1), market = listing),
    bar_source = if ("bar_source" %in% names(d)) tail(d$bar_source, 1) else "yahoo",
    name = row$name, yahoo = row$yahoo, direction = direction,
    px = round(px, 4), atr = round(atr, 4), atr_pct = round(atr_pct_now, 3),
    atr_pctile = round(atr_pctile, 1), prior20_atr = round(prior20_atr, 2),
    entry_factors = as.integer(f1) + as.integer(f2),
    zz_th = round(lr$zz_th * 100, 3), n_pivots = lr$n_pivots,
    rng_pct_20 = round(.br_n(gi$rng_pct_20), 2), rng_dyn = round(.br_n(lr$rng_dyn), 2),
    zone_window_sessions = lr$zone_window_sessions,

    res_zone_lo = if (!is.null(res)) round(res$lo, 4) else NA_real_,
    res_zone_hi = if (!is.null(res)) round(res$hi, 4) else NA_real_,
    res_touches = if (!is.null(res)) res$touches else NA_integer_,
    res_first   = if (!is.null(res)) res$first else NA_character_,
    res_last    = if (!is.null(res)) res$last  else NA_character_,
    # Distances are to the edge in play (zone_ref): the near edge when the zone
    # lies ahead, the far edge when spot stands inside it. Both stay >= 0 and
    # both answer the same question — how far to the level `target` sits on.
    res_dist_pct    = if (!is.null(res)) round(.br_pct(sgn * (res_ref - px), px), 3) else NA_real_,
    res_dist_atr    = if (!is.null(res)) round(sgn * (res_ref - px) / atr, 3) else NA_real_,
    res_pct_of_em10 = if (!is.null(res)) round(.br_pct(sgn * (res_ref - px), em_abs), 1) else NA_real_,
    res2_zone_lo = if (!is.null(lr$res2)) round(lr$res2$lo, 4) else NA_real_,
    res2_zone_hi = if (!is.null(lr$res2)) round(lr$res2$hi, 4) else NA_real_,
    res2_touches = if (!is.null(lr$res2)) lr$res2$touches else NA_integer_,
    n_res_to_ext = lr$n_res_to_ext,

    sup_zone_lo = if (!is.null(sup)) round(sup$lo, 4) else NA_real_,
    sup_zone_hi = if (!is.null(sup)) round(sup$hi, 4) else NA_real_,
    sup_touches = if (!is.null(sup)) sup$touches else NA_integer_,
    sup_first   = if (!is.null(sup)) sup$first else NA_character_,
    sup_last    = if (!is.null(sup)) sup$last  else NA_character_,
    sup_dist_pct    = if (!is.null(sup)) round(.br_pct(sgn * (px - sup_ref), px), 3) else NA_real_,
    sup_dist_atr    = if (!is.null(sup)) round(sgn * (px - sup_ref) / atr, 3) else NA_real_,
    sup_pct_of_em10 = if (!is.null(sup)) round(.br_pct(sgn * (px - sup_ref), em_abs), 1) else NA_real_,

    leg_low         = if (!is.null(fb)) round(fb$leg_low, 4) else NA_real_,
    leg_anchor_date = if (!is.null(fb)) fb$anchor_date else NA_character_,
    leg_high        = if (!is.null(fb)) round(fb$leg_high, 4) else NA_real_,
    fib_ret_382 = if (!is.null(fb)) round(unname(fb$ret[1]), 4) else NA_real_,
    fib_ret_500 = if (!is.null(fb)) round(unname(fb$ret[2]), 4) else NA_real_,
    fib_ret_618 = if (!is.null(fb)) round(unname(fb$ret[3]), 4) else NA_real_,
    fib_ext_1272 = if (!is.null(fb)) round(unname(fb$ext[1]), 4) else NA_real_,
    fib_ext_1618 = if (!is.null(fb)) round(unname(fb$ext[2]), 4) else NA_real_,

    target = round(.br_n(lr$target), 4), target_source = lr$target_source,
    target_agree = if (is.na(lr$fib_confirms_res)) NA_integer_
                   else as.integer(isTRUE(lr$fib_confirms_res)),
    stop = round(.br_n(lr$stop_px), 4), stop_source = lr$stop_source,
    # What asym rests on. Zones are validated by REPETITION (>= 2 attempts to
    # cross); Fibonacci levels and a flat 3x ATR stop are validated by nothing,
    # so a high asym built from both is a ratio of two unobserved levels.
    level_basis = {
      tz <- lr$target_source %in% c("zone", "in_zone", "flip_zone")
      sz <- lr$stop_source %in% c("zone_stop", "in_zone_stop", "flip_zone_stop")
      if (tz && sz) "zone" else if (!tz && !sz) "geometric" else "mixed"
    },
    stop_agree = if (is.na(lr$fib_confirms_sup)) NA_integer_
                 else as.integer(isTRUE(lr$fib_confirms_sup)),
    asym = round(.br_n(lr$asym), 3), asym_fib = round(.br_n(lr$asym_fib), 3),
    sess_target_p90 = sess_at(tc90), sess_target_p75 = sess_at(tc75),

    em10_lo = em_lo, em10_hi = em_hi,

    ema50 = round(.br_n(gi$ema50), 4),
    ema50_disp_pct = round(.br_pct(px - .br_n(gi$ema50), .br_n(gi$ema50)), 3),
    ema50_slope = round(.br_n(gi$ema50_slope), 3),
    w_ema50 = round(w_ema50, 4),
    w_ema50_disp_pct = round(.br_pct(px - w_ema50, w_ema50), 3),

    d_squeeze = round(.br_n(gi$d_squeeze), 4),
    w_squeeze = if (!is.null(wi)) round(.br_n(wi$d_squeeze), 4) else NA_real_,
    d_vol_decline = round(.br_n(gi$d_vol_decline), 4),
    w_vol_decline = if (!is.null(wi)) round(.br_n(wi$d_vol_decline), 4) else NA_real_,
    d_vol_surge = round(.br_n(gi$d_vol_surge), 4),
    w_vol_surge = if (!is.null(wi)) round(.br_n(wi$d_vol_surge), 4) else NA_real_,

    obv_slope = .br_n(gi$obv_slope),
    obv_slope_days = round(.br_n(gi$obv_slope_days), 3),
    rsi14 = round(.br_n(gi$rsi14), 2), rsi_slope = round(.br_n(gi$rsi_slope), 2),
    updn_ratio = round(.br_n(gi$updn_ratio), 3), ret20 = round(.br_n(gi$ret20), 3),
    rs20 = round(.br_n(gi$rs20), 3),
    rs_bench = if (!is.null(row$bench) && !is.na(row$bench[1])) as.character(row$bench[1]) else NA_character_,
    group = if (!is.null(row$group) && !is.na(row$group[1])) as.character(row$group[1]) else NA_character_,
    grp_rs = if (!is.null(rotation) && !is.null(row$group)) rotation$grp_rs[match(row$group[1], rotation$group)] else NA_real_,
    grp_rank = if (!is.null(rotation) && !is.null(row$group)) rotation$grp_rank[match(row$group[1], rotation$group)] else NA_real_,
    adx10 = round(.br_n(gi$adx10), 2),

    trend_state = cs$trend_state,
    # Weekly trend cluster (TODO 72a): shown beside the daily one so a daily
    # setup against the weekly trend reads at a glance; not a veto, not scored.
    w_trend_state = if (!is.null(gw)) cluster_states(gw)$trend_state else "n/a",
    compression_state = cs$compression_state,
    supply_state = cs$supply_state, rs_state = cs$rs_state,
    confluence = confluence_state(gd, gw),

    atr_band = row$atr_band, gap_tercile = row$gap_tercile,

    # Entry veto, assembled above: a stop a single overnight move can clear is
    # not a stop. The row is kept either way so the level read stays visible.
    tradable = as.integer(length(veto) == 0),
    veto_reason = paste(veto, collapse = "+"),
    zone_state = zone_state,
    gap_p95_pct = round(gap_p95_pct, 2),
    gap_vs_stop = round(gap_vs_stop, 2),

    note = if (identical(lr$stop_source, "atr_stop"))
             sprintf("nearest %s %.1f ATR away - ATR stop used",
                     if (long) "support" else "resistance",
                     if (is.null(sup)) NA_real_
                     else if (long) (px - sup$hi) / atr else (sup$lo - px) / atr) else "")
}

# Column order and tiers are the spec's, kept here so a schema change is one edit.
BOT_READ_COLS <- c("date","bar_lag","bar_source","name","yahoo","direction","tradable","veto_reason","zone_state",
  "px","atr","atr_pct","atr_pctile","prior20_atr","entry_factors",
  "zz_th","n_pivots",
  "rng_pct_20","rng_dyn","zone_window_sessions",
  "res_zone_lo","res_zone_hi","res_touches","res_first","res_last",
  "res_dist_pct","res_dist_atr","res_pct_of_em10",
  "res2_zone_lo","res2_zone_hi","res2_touches","n_res_to_ext",
  "sup_zone_lo","sup_zone_hi","sup_touches","sup_first","sup_last",
  "sup_dist_pct","sup_dist_atr","sup_pct_of_em10",
  "leg_low","leg_anchor_date","leg_high",
  "fib_ret_382","fib_ret_500","fib_ret_618","fib_ext_1272","fib_ext_1618",
  "target","target_source","target_agree","stop","stop_source","stop_agree",
  "gap_p95_pct","gap_vs_stop",
  "level_basis","asym","asym_fib","sess_target_p90","sess_target_p75","em10_lo","em10_hi",
  "ema50","ema50_disp_pct","ema50_slope","w_ema50","w_ema50_disp_pct",
  "d_squeeze","w_squeeze","d_vol_decline","w_vol_decline","d_vol_surge","w_vol_surge",
  "obv_slope","obv_slope_days","rsi14","rsi_slope","updn_ratio","ret20","rs20","rs_bench","group","grp_rs","grp_rank","adx10",
  "trend_state","w_trend_state","compression_state","supply_state","rs_state","confluence",
  "atr_band","gap_tercile","note")
# Default output: the few columns read every day. BOT_daily is a daily sheet,
# so it stays short; --detail emits every field.
BOT_READ_DEFAULT <- c("name","direction","px","atr","tradable","asym",
  "target","target_source","stop","sess_target_p90","sess_target_p75","trend_state","zone_state")
BOT_READ_DETAIL_ONLY <- c("yahoo","atr_pct","zz_th","n_pivots","rng_pct_20","rng_dyn",
  "res_first","res_dist_pct","sup_first","sup_dist_pct",
  "fib_ret_382","fib_ret_500","em10_lo","ema50","ema50_slope","w_ema50",
  "d_squeeze","w_squeeze","d_vol_decline","w_vol_decline","d_vol_surge","w_vol_surge",
  "obv_slope","obv_slope_days","rsi14","rsi_slope","updn_ratio","ret20","rs20","rs_bench","group","grp_rs","grp_rank","adx10")
