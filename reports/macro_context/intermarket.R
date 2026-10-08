# intermarket.R — intermarket panels: per-instrument trend metrics, ratios, relative rotation,
# BOT sector verdicts per correlation group. Own fetch (about 15 months of history, needed for the 200-day average and z-scores),
# cached daily in DB table "intermarket_cache".

INTERMARKET_CACHE <- "intermarket_cache"

fetch_intermarket_data <- function() {
  today <- as.character(Sys.Date())
  raw <- cache_read(INTERMARKET_CACHE, today)
  syms <- report_symbols()
  if (!is.null(raw)) {
    raw$cache_date <- NULL
    raw$date <- as.Date(raw$date)
    # Symbols added to the config since today's cache was written: fetch and append them
    add <- setdiff(syms, unique(raw$ticker))
    if (length(add)) {
      extra <- tryCatch(Tdata::getYahooData(add, from_date = Sys.Date() - HISTORY_DAYS, to_date = Sys.Date()),
                        error = function(e) NULL)
      if (!is.null(extra) && nrow(extra) > 0) {
        cache_append(INTERMARKET_CACHE, extra, today)
        raw <- rbind(raw, extra[, names(raw)])
      }
    }
    return(refetch_empty_closes(raw, today))
  }
  message("Fetching ", length(syms), " intermarket symbols...")
  raw <- tryCatch(Tdata::getYahooData(syms, from_date = Sys.Date() - HISTORY_DAYS, to_date = Sys.Date()),
                  error = function(e) { message("ERROR: ", e$message); NULL })
  if (!is.null(raw) && nrow(raw) > 0) cache_write(INTERMARKET_CACHE, raw, today)
  if (is.null(raw)) raw else refetch_empty_closes(raw, today)
}

#' Symbols whose latest weekday bar or bars have no close: data.frame(ticker, missing, last_close)
#' Yahoo can return a session's bar before its close is filled in (08 Oct 2026, 09:00:
#' 35 European and Asian symbols had an empty 07 Oct close). get_close() drops empty
#' rows, so without this check the report shows the session before as if it were the last.
empty_last_closes <- function(raw) {
  r <- raw[raw$date < Sys.Date() & bday(raw$date), c("ticker", "date", "Close")]
  out <- lapply(split(r, r$ticker), function(d) {
    d <- d[order(d$date), ]
    ok <- which(!is.na(d$Close))
    lv <- if (length(ok)) d$date[max(ok)] else as.Date(NA)
    miss <- d$date[is.na(d$Close) & (is.na(lv) | d$date > lv)]
    if (!length(miss)) return(NULL)
    data.frame(ticker = d$ticker[1], missing = max(miss), last_close = lv, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) data.frame(ticker = character(), missing = as.Date(character()), last_close = as.Date(character())) else out
}

#' Re-fetch symbols with an empty last close once; replace their recent rows in the data
#' and in today's cache when Yahoo now has the close. What is still empty stays in
#' attr(raw, "stale") so the panels and the scenario text can say so.
refetch_empty_closes <- function(raw, today) {
  st <- empty_last_closes(raw)
  if (nrow(st)) {
    from <- min(c(st$last_close, st$missing), na.rm = TRUE) - 7
    message("Empty last close for ", nrow(st), " symbols (", paste(head(st$ticker, 8), collapse = ", "),
            if (nrow(st) > 8) ", ..." else "", "): re-fetching from ", from)
    fr <- tryCatch(Tdata::getYahooData(st$ticker, from_date = from, to_date = Sys.Date()), error = function(e) NULL)
    if (!is.null(fr) && nrow(fr) > 0) {
      fr <- fr[!is.na(fr$Close), names(raw)]
      fixed <- intersect(st$ticker, unique(fr$ticker[fr$date >= min(st$missing)]))
      if (length(fixed)) {
        fr <- fr[fr$ticker %in% fixed, ]
        keep <- !(raw$ticker %in% fixed & raw$date >= min(fr$date))
        raw <- rbind(raw[keep, ], fr)
        cache_replace_recent(INTERMARKET_CACHE, fr, today)
      }
    }
    st <- empty_last_closes(raw)
    if (nrow(st)) message("Still no close after re-fetch: ", paste(st$ticker, collapse = ", "))
  }
  attr(raw, "stale") <- st
  raw
}

#' Everything the report needs from the intermarket side
run_intermarket <- function(breadth) {
  raw <- fetch_intermarket_data()
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  stale <- attr(raw, "stale")
  raw <- roll_adjust(raw)
  missing <- setdiff(report_symbols(), unique(raw$ticker[!is.na(raw$Close)]))
  if (length(missing)) message("Intermarket: no data for ", paste(missing, collapse = ", "))
  sections <- mark_stale(analyze_sections(raw), stale)
  sectors <- analyze_sectors(raw)
  bh <- load_breadth_history(raw, if (is.null(breadth)) NA_real_ else breadth$pct)
  z1 <- asset_moves(raw, 21, bh); z3 <- asset_moves(raw, 63, bh); st <- asset_states(raw, bh)
  matches <- add_persistence(match_archetypes(z1, z3, st), scenario_history(raw, bh = bh))
  carry <- tryCatch(carry_alert(raw), error = function(e) { message("Carry alert failed: ", conditionMessage(e)); NULL })
  movie <- build_movie(sections, breadth, z1, matches)
  list(sections = sections, sectors = sectors, z1 = z1, z3 = z3, st = st, matches = matches, movie = movie, carry = carry,
       stale = stale)
}

#' Continuous futures without roll gaps. Each day's return comes from the front
#' contract of that day when Yahoo still lists it, else from the =F series, except on
#' a roll day, where the =F return mixes two contracts: there the nearest listed
#' contract stands in (expired contracts are not served). Levels are rebuilt backwards
#' from the last close of the listed front, so "Last" is the real front price.
roll_adjust <- function(raw) {
  for (key in intersect(names(ROLL_ADJUSTED), unique(raw$ticker))) {
    root <- ROLL_ADJUSTED[[key]]
    i <- which(raw$ticker == key & !is.na(raw$Close))
    i <- i[order(raw$date[i])]
    if (length(i) < 2) next
    dt <- raw$date[i]; px <- raw$Close[i]
    cur <- fut_front_ym(root, max(dt))
    f <- raw[raw$ticker == fut_contract(root, cur)$sym & !is.na(raw$Close), c("date", "Close")]
    fpx <- setNames(f$Close, as.character(f$date))
    fr <- vapply(dt, function(d) fut_front_ym(root, d), 0)
    r <- numeric(length(dt))
    for (k in 2:length(dt)) {
      a <- fpx[as.character(dt[k - 1])]; b <- fpx[as.character(dt[k])]
      has_f <- !is.na(a) && !is.na(b)
      r[k] <- if (fr[k] == cur && has_f) log(b / a) else
        if (fr[k] == fr[k - 1]) log(px[k] / px[k - 1]) else
        if (has_f) log(b / a) else 0
    }
    last <- fpx[as.character(dt[length(dt)])]
    if (is.na(last)) last <- px[length(px)]
    adj <- unname(last) / exp(rev(cumsum(rev(c(r[-1], 0)))))
    raw$Close[i] <- adj
    if ("Adjusted" %in% names(raw)) raw$Adjusted[i] <- adj
  }
  raw
}

#' Flag panel rows whose last bar had no close (see empty_last_closes)
mark_stale <- function(sections, stale) {
  if (is.null(stale) || !nrow(stale)) return(sections)
  lapply(sections, function(sec) {
    sec$instruments <- lapply(sec$instruments, function(m) {
      j <- match(m$sym, stale$ticker)
      if (!is.na(j)) m$stale_missing <- stale$missing[j]
      m
    })
    sec
  })
}

ema <- function(x, n) as.numeric(stats::filter(x * (2 / (n + 1)), 1 - 2 / (n + 1),
                                               method = "recursive", init = x[1] ))
sma <- function(x, n) as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))

#' Close series for one symbol, or "A/B" ratio of two symbols on common dates
get_close <- function(raw, key) {
  one <- function(sym) {
    col <- if (sym %in% ADJUSTED_SYMBOLS && "Adjusted" %in% names(raw)) "Adjusted" else "Close"
    d <- raw[raw$ticker == sym & !is.na(raw[[col]]), c("date", col)]
    names(d)[2] <- "Close"
    d[order(d$date), ]
  }
  if (grepl("/", key, fixed = TRUE)) {
    p <- strsplit(key, "/", fixed = TRUE)[[1]]
    is_fx <- endsWith(p, "=X")
    if (xor(is_fx[1], is_fx[2])) {
      # Yahoo FX bars sit on a different calendar (Sunday rows, no Friday rows while the
      # UK is on summer time), so an inner join drops about one day in five and stretches
      # "1M" to six weeks. Take the latest FX value on or before each date of the other leg.
      a <- one(p[!is_fx]); b <- one(p[is_fx])
      i <- findInterval(as.numeric(a$date), as.numeric(b$date))
      a <- a[i > 0, ]; fxv <- b$Close[i[i > 0]]
      if (nrow(a) == 0) return(NULL)
      r <- if (is_fx[2]) a$Close / fxv else fxv / a$Close
      return(data.frame(date = a$date, Close = r))
    }
    m <- merge(one(p[1]), one(p[2]), by = "date")
    if (nrow(m) == 0) return(NULL)
    return(data.frame(date = m$date, Close = m$Close.x / m$Close.y))
  }
  d <- one(key)
  if (nrow(d) == 0) NULL else d
}

#' Trend state from EMA20/EMA50: UP, DOWN or MIXED
#' UP = close above EMA50 and EMA20 above EMA50 (mirror for DOWN)
trend_state <- function(close, e20, e50) {
  if (any(is.na(c(close, e20, e50)))) return(NA_character_)
  if (close > e50 && e20 > e50) return("UP")
  if (close < e50 && e20 < e50) return("DOWN")
  "MIXED"
}

#' Metrics for one series. kind = "yield" reports changes in basis points,
#' kind = "usd" in dollars (price differences such as a futures calendar spread).
series_metrics <- function(d, kind = "price") {
  if (is.null(d) || nrow(d) < 60) return(NULL)
  x <- d$Close; n <- length(x)
  e20 <- ema(x, 20); e50 <- ema(x, 50); s200 <- sma(x, 200)
  chg <- function(k) {
    if (n <= k) return(NA_real_)
    if (kind == "yield") 100 * (x[n] - x[n - k]) else if (kind == "usd") x[n] - x[n - k] else 100 * (x[n] / x[n - k] - 1)
  }
  last252 <- tail(x, 252)
  rng <- max(last252) - min(last252)
  list(
    last = x[n], date = d$date[n], date_prev = if (n > 1) d$date[n - 1] else NA,
    c1d = chg(1), c1w = chg(5), c1m = chg(21), c3m = chg(63),
    ema20 = e20[n], ema50 = e50[n], sma200 = s200[n],
    above200 = if (is.na(s200[n])) NA else x[n] > s200[n],
    ema50_slope = if (n > 10) 100 * (e50[n] / e50[n - 10] - 1) else NA_real_,
    pos52 = if (rng > 0) 100 * (x[n] - min(last252)) / rng else NA_real_,
    trend = trend_state(x[n], e20[n], e50[n]),
    spark = tail(data.frame(date = d$date, close = x, ema50 = e50), 252)
  )
}

#' Relative rotation of a close series vs the benchmark's (simplified RRG)
#' rs_ratio = 100 * (series/bench) / its 50-day average; rs_mom = 10-day change of rs_ratio
rotation <- function(d, bench_d) {
  if (is.null(d) || is.null(bench_d)) return(NULL)
  m <- merge(d, bench_d, by = "date")
  if (nrow(m) < 70) return(NULL)
  r <- m$Close.x / m$Close.y
  ratio <- 100 * r / sma(r, 50)
  n <- length(ratio)
  rs <- ratio[n]; mom <- ratio[n] - ratio[n - 10]
  quad <- if (rs >= 100 && mom >= 0) "Leading" else if (rs >= 100) "Weakening" else
    if (mom >= 0) "Improving" else "Lagging"
  list(rs_ratio = rs, rs_mom = mom, quadrant = quad,
       rs_3m = 100 * (r[n] / r[max(1, n - 63)] - 1))
}

#' Driver alignment: sum of sign * trend direction (+1 UP, -1 DOWN, 0 MIXED), scaled to [-1, 1]
driver_score <- function(raw, drivers) {
  if (length(drivers) == 0) return(list(score = NA_real_, detail = ""))
  parts <- mapply(function(key, sgn) {
    m <- series_metrics(get_close(raw, key))
    dir <- if (is.null(m) || is.na(m$trend)) 0 else switch(m$trend, UP = 1, DOWN = -1, 0)
    list(v = sgn * dir, txt = sprintf("%s %s%s", key, if (is.null(m)) "n/a" else m$trend,
                                       if (sgn < 0) " (inverse)" else ""))
  }, names(drivers), drivers, SIMPLIFY = FALSE)
  list(score = mean(vapply(parts, `[[`, 0, "v")),
       detail = paste(vapply(parts, `[[`, "", "txt"), collapse = "; "))
}

#' BOT verdict for a sector group
#' LONG  = trend UP, rotation Leading/Improving, drivers not against (score >= 0)
#' SHORT = trend DOWN, rotation Lagging/Weakening, drivers not against (score <= 0)
#' AVOID = trend MIXED, or drivers clearly against the trend (|score| >= 0.5, opposite sign)
#' WATCH = everything else (trend and rotation disagree)
bot_verdict <- function(trend, quadrant, score) {
  if (is.na(trend) || trend == "MIXED") return("AVOID")
  s <- if (is.na(score)) 0 else score
  if (trend == "UP") {
    if (s <= -0.5) return("AVOID")
    if (quadrant %in% c("Leading", "Improving")) return("LONG")
  } else {
    if (s >= 0.5) return("AVOID")
    if (quadrant %in% c("Lagging", "Weakening")) return("SHORT")
  }
  "WATCH"
}

analyze_sections <- function(raw) {
  lapply(SECTIONS, function(sec) {
    inst <- lapply(sec$instruments, function(i) {
      m <- series_metrics(get_close(raw, i[1]), i[3])
      if (is.null(m)) return(NULL)
      c(list(sym = i[1], label = i[2], kind = i[3]), m)
    })
    rat <- lapply(sec$ratios, function(r) {
      m <- series_metrics(get_close(raw, paste0(r[1], "/", r[2])))
      if (is.null(m)) return(NULL)
      c(list(sym = paste0(r[1], "/", r[2]), label = r[3], meaning = r[4], kind = "price"), m)
    })
    spr <- lapply(sec$spreads %||% list(), function(r) {
      a <- get_close(raw, r[1]); b <- get_close(raw, r[2])
      if (is.null(a) || is.null(b)) return(NULL)
      m0 <- merge(a, b, by = "date")
      kind <- if (length(r) >= 5) r[5] else "yield"   # spreads are yields unless the config says otherwise
      m <- series_metrics(data.frame(date = m0$date, Close = m0$Close.x - m0$Close.y), kind)
      if (is.null(m)) return(NULL)
      c(list(sym = paste0(r[1], "-", r[2]), label = r[3], meaning = r[4], kind = kind), m)
    })
    list(id = sec$id, title = sec$title,
         instruments = Filter(Negate(is.null), inst),
         ratios = Filter(Negate(is.null), c(rat, spr)))
  })
}

.group_map_cache <- new.env(parent = emptyenv())

#' BOT sector map rows: the correlation groups (ScannerUniverse.Cluster) that hold at
#' least one Tickers.BOT_Eligible name, so the map and BOT's S3 benchmark
#' (shared/bot_read.R) read the same groups. Each group is read as the equal-weight
#' index of all its members, never through its anchor ETF: an anchor outside the
#' group is the nearest universe ETF, which can serve two groups (ITA) or another
#' industry (ITB for machinery). S3 uses the same members (peer median).
#' Cached per session.
#'
#' @return list(groups = list of list(group, members, n_bot, tags),
#'   ungrouped = data.frame(name, bench) of BOT names in no group)
group_map <- function() {
  if (!is.null(.group_map_cache$gm)) return(.group_map_cache$gm)
  conn <- Tdata::safe_db_connect()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  sc <- DBI::dbGetQuery(conn,
    "SELECT s.Symbol, s.Cluster, s.ClusterETF, t.YahooName, t.BOT_Eligible, t.BOT_Bench,
            a.YahooName AS AnchorYahoo
       FROM ScannerUniverse s
       LEFT JOIN Tickers t ON t.Name = s.Symbol
       LEFT JOIN Tickers a ON a.Name = s.ClusterETF
      WHERE s.IsActive = 1 AND s.Role = 'scanner'
        AND s.Cluster IS NOT NULL AND s.Cluster <> '' AND s.Cluster <> 'Ungrouped'")
  bot <- DBI::dbGetQuery(conn, "SELECT Name, BOT_Bench FROM Tickers WHERE BOT_Eligible = 1")
  sc$yh <- ifelse(!is.na(sc$YahooName) & nzchar(sc$YahooName), sc$YahooName, sc$Symbol)
  sc$anchor_yh <- ifelse(!is.na(sc$AnchorYahoo) & nzchar(sc$AnchorYahoo), sc$AnchorYahoo, sc$ClusterETF)
  sc$bot <- sc$BOT_Eligible %in% 1

  grp <- sort(unique(sc$Cluster[sc$bot]))
  no_drv <- setdiff(grp, names(GROUP_DRIVERS))
  if (length(no_drv)) stop("GROUP_DRIVERS (intermarket_config.R) has no entry for group(s): ",
                           paste(no_drv, collapse = "; "))
  groups <- lapply(grp, function(g) {
    m <- sc[sc$Cluster == g, , drop = FALSE]
    a <- m$ClusterETF[1]
    # Tags let a scenario's ETF (archetypes.R) find this group: the anchor, and the
    # benchmark most members carried before the groups existed (Tickers.BOT_Bench).
    bb <- m$BOT_Bench[!is.na(m$BOT_Bench) & nzchar(m$BOT_Bench)]
    tags <- unique(c(if (!is.na(a)) c(a, m$anchor_yh[1]), if (length(bb)) names(which.max(table(bb)))))
    list(group = g, members = m$yh, n_bot = sum(m$bot), tags = tags)
  })
  ug <- bot[!bot$Name %in% sc$Symbol, , drop = FALSE]
  .group_map_cache$gm <- list(groups = groups,
                              ungrouped = data.frame(name = ug$Name, bench = ug$BOT_Bench, stringsAsFactors = FALSE))
  .group_map_cache$gm
}

# A member's daily return beyond this is a bad print (unit change, split), not a move
EW_MAX_DAILY_RET <- 0.5

#' Equal-weight index of several symbols (dividend-adjusted): the mean daily return of
#' the members quoted that day, compounded from 100. A day counts only when at least
#' half the members have a return, so a holiday on one exchange does not set the move.
ew_close <- function(raw, syms) {
  col <- if ("Adjusted" %in% names(raw)) "Adjusted" else "Close"
  r <- do.call(rbind, lapply(syms, function(s) {
    d <- raw[raw$ticker == s & is.finite(raw[[col]]) & raw[[col]] > 0, c("date", col)]
    if (nrow(d) < 2) return(NULL)
    d <- d[order(d$date), ]
    data.frame(date = d$date[-1], ret = diff(d[[col]]) / head(d[[col]], -1))
  }))
  if (is.null(r)) return(NULL)
  r <- r[abs(r$ret) <= EW_MAX_DAILY_RET, ]
  n <- table(r$date)
  mu <- tapply(r$ret, r$date, mean)
  keep <- n[names(mu)] >= max(1, ceiling(length(syms) / 2))
  mu <- mu[keep]
  if (length(mu) < 2) return(NULL)
  data.frame(date = as.Date(names(mu)), Close = 100 * cumprod(1 + as.numeric(mu)))
}

analyze_sectors <- function(raw) {
  gm <- group_map()
  bench_d <- get_close(raw, BENCHMARK)
  rows <- lapply(gm$groups, function(g) {
    d <- ew_close(raw, g$members)
    m <- series_metrics(d)
    rr <- rotation(d, bench_d)
    if (is.null(m) || is.null(rr)) return(NULL)
    ds <- driver_score(raw, GROUP_DRIVERS[[g$group]])
    quoted <- sum(g$members %in% raw$ticker[!is.na(raw$Close)])
    data.frame(
      group = g$group, bench = "EW", trend = m$trend,
      c1m = m$c1m, c3m = m$c3m, pos52 = m$pos52, above200 = m$above200,
      rs_ratio = rr$rs_ratio, rs_mom = rr$rs_mom, quadrant = rr$quadrant, rs_3m = rr$rs_3m,
      driver_score = ds$score, drivers = ds$detail,
      verdict = bot_verdict(m$trend, rr$quadrant, ds$score),
      n_members = length(g$members), n_quoted = quoted, n_bot = g$n_bot,
      members = paste(g$members, collapse = " "), tags = paste(g$tags, collapse = " "),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  ord <- c(LONG = 1, SHORT = 2, WATCH = 3, AVOID = 4)
  out <- out[order(ord[out$verdict], -out$rs_ratio), ]
  attr(out, "ungrouped") <- gm$ungrouped
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' All symbols the report needs
report_symbols <- function() {
  split_keys <- function(k) if (is.null(k)) character(0) else unlist(strsplit(k, "/", fixed = TRUE))
  s <- unlist(lapply(SECTIONS, function(sec) c(
    vapply(sec$instruments, `[`, "", 1),
    unlist(lapply(sec$ratios, `[`, 1:2)),
    unlist(lapply(sec$spreads %||% list(), `[`, 1:2)))))
  gm <- group_map()
  g <- c(unlist(lapply(gm$groups, `[[`, "members")),
         unlist(lapply(GROUP_DRIVERS, function(d) split_keys(names(d)))))
  f <- unlist(lapply(FP_ASSETS[names(FP_ASSETS) != "ABS_BREADTH"],   # breadth comes from the daily runs, not Yahoo
                     function(a) unlist(strsplit(a[[1]], "/|-(?=\\^)", perl = TRUE))))
  fronts <- vapply(ROLL_ADJUSTED, function(root) fut_contract(root, fut_front_ym(root, Sys.Date() - 1))$sym, "")
  unique(stats::na.omit(c(BENCHMARK, s, g, f, fronts)))
}
