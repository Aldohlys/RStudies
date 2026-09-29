# reports/analyze/valuation.R — the valuation block of /analyze (TODO 92).
#
# Context for position trades and single-name coverage; not a BOT input and
# never scored. Reads PE_History (scripts/collect_pe.py). Each basis is used
# where it is comparable:
#   own history          reported EPS (the company's reported, often adjusted,
#                        figure, stepping on the report date) — the only basis
#                        with history
#   vs sector / index    Yahoo trailing P/E — the basis Yahoo gives for ETFs
#   adjustment gap       reported vs Yahoo (GAAP) EPS: the share of earnings
#                        that rests on the company's adjustments
# Percentiles need VAL_MIN_DAYS observations; below that the report says how
# much history exists.

VAL_MIN_DAYS <- 60L

.val_pctile <- function(x, now) {
  x <- x[is.finite(x)]
  if (length(x) < VAL_MIN_DAYS || !is.finite(now)) return(NA_real_)
  round(mean(x <= now) * 100)
}

.val_series <- function(conn, sym, basis) {
  DBI::dbGetQuery(conn, "SELECT date, pe_ttm, pe_fwd, eps_ttm FROM PE_History
                         WHERE sym = ? AND basis = ? ORDER BY date",
                  params = list(sym, basis))
}

# PE_History keys ETFs by Tickers.Name when they are in Tickers, else by symbol.
.val_key <- function(conn, sym) {
  if (is.null(sym) || is.na(sym) || !nzchar(sym)) return(NA_character_)
  n <- DBI::dbGetQuery(conn, "SELECT Name FROM Tickers WHERE Name = ? OR YahooName = ? LIMIT 1",
                       params = list(sym, sym))
  if (nrow(n)) n$Name[1] else sym
}

run_valuation <- function(ticker) {
  conn <- tryCatch(Tdata::safe_db_connect(), error = function(e) NULL)
  if (is.null(conn)) return(list(status = "FETCH FAILED", reason = "database not reachable"))
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  have <- DBI::dbListTables(conn)
  if (!"PE_History" %in% have)
    return(list(status = "NO DATA", reason = "PE_History table missing (run scripts/collect_pe.py)"))

  tk <- DBI::dbGetQuery(conn, "SELECT Type, BOT_Bench FROM Tickers WHERE Name = ?", params = list(ticker))
  bench <- if (nrow(tk) && "BOT_Bench" %in% names(tk)) tk$BOT_Bench[1] else NA_character_

  rep <- .val_series(conn, ticker, "reported_eps")
  yah <- .val_series(conn, ticker, "yahoo_info")
  if (!nrow(rep) && !nrow(yah))
    return(list(status = "NO DATA", reason = sprintf("no P/E stored for %s", ticker)))

  own <- NULL
  if (nrow(rep)) {
    d <- as.Date(rep$date); last <- nrow(rep)
    now <- rep$pe_ttm[last]
    in_y <- function(y) rep$pe_ttm[d >= max(d) - round(y * 365.25)]
    own <- list(pe = now, eps = rep$eps_ttm[last], date = rep$date[last],
                p5 = .val_pctile(in_y(5), now), p10 = .val_pctile(in_y(10), now),
                loss_share_5y = round(mean(!is.finite(in_y(5))) * 100),
                from = rep$date[1])
  }
  y_now <- if (nrow(yah)) yah[nrow(yah), ] else NULL

  ref <- function(sym) {
    key <- .val_key(conn, sym)
    if (is.na(key)) return(NULL)
    s <- .val_series(conn, key, "yahoo_info")
    if (!nrow(s)) return(list(sym = sym, pe = NA_real_, days = 0L))
    list(sym = sym, pe = s$pe_ttm[nrow(s)], days = nrow(s), from = s$date[1], series = s)
  }
  b <- ref(bench); spy <- ref("SPY")

  rel <- function(r) {
    if (is.null(r) || is.null(y_now) || !is.finite(y_now$pe_ttm) || !is.finite(r$pe) || r$pe <= 0)
      return(list(ratio = NA_real_, pctile = NA_real_))
    ratio <- y_now$pe_ttm / r$pe
    m <- merge(yah[, c("date", "pe_ttm")], r$series[, c("date", "pe_ttm")], by = "date")
    hist <- m$pe_ttm.x / m$pe_ttm.y
    list(ratio = ratio, pctile = .val_pctile(hist, ratio))
  }

  gap <- if (!is.null(own) && !is.null(y_now) && is.finite(own$eps) && is.finite(y_now$eps_ttm) &&
             y_now$eps_ttm > 0) round((own$eps / y_now$eps_ttm - 1) * 100) else NA_real_

  list(status = "LIVE", ticker = ticker, is_equity = nrow(rep) > 0,
       own = own, yahoo = y_now, bench = b, spy = spy,
       rel_bench = rel(b), rel_spy = rel(spy), gap_pct = gap,
       yahoo_days = nrow(yah), yahoo_from = if (nrow(yah)) yah$date[1] else NA_character_)
}

.render_valuation <- function(v) {
  head <- '<h2>Valuation &mdash; context for position trades; not a BOT input</h2>'
  if (is.null(v)) return("")
  if (!identical(v$status, "LIVE"))
    return(paste0(head, sprintf('<p class="sub">%s: %s</p>', v$status, v$reason %||% "")))
  f1 <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) "n/a" else sprintf("%.1f", x)
  f2 <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) "n/a" else sprintf("%.2f", x)
  fp <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) sprintf("n/a (&lt; %d days)", VAL_MIN_DAYS) else sprintf("%.0f", x)
  hist_note <- function(days, from) if (!is.finite(days) || days < 1) "no snapshot yet"
                                    else sprintf("Yahoo snapshots since %s (%d day%s)", from, days, if (days > 1) "s" else "")
  rows <- character(0)
  o <- v$own
  if (!is.null(o))
    rows <- c(rows, sprintf(
      '<tr><td>Own history (reported EPS)</td><td class="value">P/E %s</td><td>5y pctile %s &middot; 10y pctile %s</td><td class="note">daily since %s%s</td></tr>',
      f1(o$pe), fp(o$p5), fp(o$p10), o$from,
      if (isTRUE(o$loss_share_5y > 0)) sprintf("; EPS &le; 0 on %d%% of the last 5y (P/E blank)", o$loss_share_5y) else ""))
  y <- v$yahoo
  bpe <- if (!is.null(v$bench)) sprintf("%s %s", v$bench$sym, f1(v$bench$pe)) else "no benchmark"
  rows <- c(rows, sprintf(
    '<tr><td>Vs sector / index (Yahoo)</td><td class="value">%s %s</td><td>%s &middot; SPY %s</td><td class="note">%s</td></tr>',
    v$ticker, f1(if (!is.null(y)) y$pe_ttm else NA), bpe, f1(v$spy$pe),
    hist_note(v$yahoo_days, v$yahoo_from)))
  rows <- c(rows, sprintf(
    '<tr><td>Relative P/E (Yahoo)</td><td class="value">&divide;%s %s &middot; &divide;SPY %s</td><td>pctile %s &middot; %s</td><td class="note">percentile of the ratio over the common Yahoo history</td></tr>',
    if (!is.null(v$bench)) v$bench$sym else "bench", f2(v$rel_bench$ratio), f2(v$rel_spy$ratio),
    fp(v$rel_bench$pctile), fp(v$rel_spy$pctile)))
  rows <- c(rows, sprintf(
    '<tr><td>Forward P/E (Yahoo)</td><td class="value">%s</td><td></td><td class="note">%s</td></tr>',
    f1(if (!is.null(y)) y$pe_fwd else NA), if (isTRUE(v$is_equity)) "" else "Yahoo gives no forward P/E for ETFs"))
  if (isTRUE(v$is_equity))
    rows <- c(rows, sprintf(
      '<tr><td>Adjustment gap</td><td class="value">%s</td><td></td><td class="note">reported EPS vs Yahoo (GAAP) EPS, trailing 12 months: the share of earnings resting on the company&rsquo;s adjustments</td></tr>',
      if (is.finite(v$gap_pct)) sprintf("reported %+d%% vs GAAP", as.integer(v$gap_pct)) else "n/a"))
  note <- paste0('<p class="sub">Reported EPS = the EPS the company reports on results day (often its adjusted figure), ',
                 'summed over 4 quarters and stepping on the report date. Yahoo = Yahoo&rsquo;s trailing P/E (GAAP); ',
                 'for ETFs Yahoo does not state how loss-making holdings are treated. No cheap / expensive reading.</p>')
  paste0(head, '<table><tr><th>Basis</th><th>Value</th><th>Rank</th><th>Note</th></tr>',
         paste(rows, collapse = ""), '</table>', note)
}
