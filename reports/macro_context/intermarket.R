# intermarket.R — intermarket panels: per-instrument trend metrics, ratios, relative rotation,
# BOT sector verdicts. Own fetch (about 15 months of history, needed for the 200-day average and z-scores),
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
    return(raw)
  }
  message("Fetching ", length(syms), " intermarket symbols...")
  raw <- tryCatch(Tdata::getYahooData(syms, from_date = Sys.Date() - HISTORY_DAYS, to_date = Sys.Date()),
                  error = function(e) { message("ERROR: ", e$message); NULL })
  if (!is.null(raw) && nrow(raw) > 0) cache_write(INTERMARKET_CACHE, raw, today)
  raw
}

#' Everything the report needs from the intermarket side
run_intermarket <- function(breadth) {
  raw <- fetch_intermarket_data()
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  missing <- setdiff(report_symbols(), unique(raw$ticker[!is.na(raw$Close)]))
  if (length(missing)) message("Intermarket: no data for ", paste(missing, collapse = ", "))
  sections <- analyze_sections(raw)
  sectors <- analyze_sectors(raw)
  z1 <- asset_moves(raw, 21); z3 <- asset_moves(raw, 63)
  matches <- add_persistence(match_archetypes(z1, z3), scenario_history(raw))
  movie <- build_movie(sections, breadth, z1, matches)
  list(sections = sections, sectors = sectors, z1 = z1, z3 = z3, matches = matches, movie = movie)
}

ema <- function(x, n) as.numeric(stats::filter(x * (2 / (n + 1)), 1 - 2 / (n + 1),
                                               method = "recursive", init = x[1] ))
sma <- function(x, n) as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))

#' Close series for one symbol, or "A/B" ratio of two symbols on common dates
get_close <- function(raw, key) {
  one <- function(sym) {
    d <- raw[raw$ticker == sym & !is.na(raw$Close), c("date", "Close")]
    d[order(d$date), ]
  }
  if (grepl("/", key, fixed = TRUE)) {
    p <- strsplit(key, "/", fixed = TRUE)[[1]]
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

#' Metrics for one series. kind = "yield" reports changes in basis points.
series_metrics <- function(d, kind = "price") {
  if (is.null(d) || nrow(d) < 60) return(NULL)
  x <- d$Close; n <- length(x)
  e20 <- ema(x, 20); e50 <- ema(x, 50); s200 <- sma(x, 200)
  chg <- function(k) {
    if (n <= k) return(NA_real_)
    if (kind == "yield") 100 * (x[n] - x[n - k]) else 100 * (x[n] / x[n - k] - 1)
  }
  last252 <- tail(x, 252)
  rng <- max(last252) - min(last252)
  list(
    last = x[n], date = d$date[n],
    c1w = chg(5), c1m = chg(21), c3m = chg(63),
    ema20 = e20[n], ema50 = e50[n], sma200 = s200[n],
    above200 = if (is.na(s200[n])) NA else x[n] > s200[n],
    ema50_slope = if (n > 10) 100 * (e50[n] / e50[n - 10] - 1) else NA_real_,
    pos52 = if (rng > 0) 100 * (x[n] - min(last252)) / rng else NA_real_,
    trend = trend_state(x[n], e20[n], e50[n]),
    spark = tail(data.frame(date = d$date, close = x, ema50 = e50), 252)
  )
}

#' Relative rotation vs benchmark (simplified RRG)
#' rs_ratio = 100 * (ETF/bench) / its 50-day average; rs_mom = 10-day change of rs_ratio
rotation <- function(raw, sym, bench = BENCHMARK) {
  d <- get_close(raw, paste0(sym, "/", bench))
  if (is.null(d) || nrow(d) < 70) return(NULL)
  r <- d$Close
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
      m <- series_metrics(data.frame(date = m0$date, Close = m0$Close.x - m0$Close.y), "yield")
      if (is.null(m)) return(NULL)
      c(list(sym = paste0(r[1], "-", r[2]), label = r[3], meaning = r[4], kind = "yield"), m)
    })
    list(id = sec$id, title = sec$title,
         instruments = Filter(Negate(is.null), inst),
         ratios = Filter(Negate(is.null), c(rat, spr)))
  })
}

analyze_sectors <- function(raw) {
  rows <- lapply(SECTOR_GROUPS, function(g) {
    m <- series_metrics(get_close(raw, g$bench))
    rr <- rotation(raw, g$bench)
    if (is.null(m) || is.null(rr)) return(NULL)
    ds <- driver_score(raw, g$drivers)
    data.frame(
      group = g$group, bench = g$bench, trend = m$trend,
      c1m = m$c1m, c3m = m$c3m, pos52 = m$pos52, above200 = m$above200,
      rs_ratio = rr$rs_ratio, rs_mom = rr$rs_mom, quadrant = rr$quadrant, rs_3m = rr$rs_3m,
      driver_score = ds$score, drivers = ds$detail,
      verdict = bot_verdict(m$trend, rr$quadrant, ds$score),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  ord <- c(LONG = 1, SHORT = 2, WATCH = 3, AVOID = 4)
  out[order(ord[out$verdict], -out$rs_ratio), ]
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' All symbols the report needs
report_symbols <- function() {
  split_keys <- function(k) if (is.null(k)) character(0) else unlist(strsplit(k, "/", fixed = TRUE))
  s <- unlist(lapply(SECTIONS, function(sec) c(
    vapply(sec$instruments, `[`, "", 1),
    unlist(lapply(sec$ratios, `[`, 1:2)),
    unlist(lapply(sec$spreads %||% list(), `[`, 1:2)))))
  g <- unlist(lapply(SECTOR_GROUPS, function(x) c(x$bench, split_keys(names(x$drivers)))))
  f <- unlist(lapply(FP_ASSETS, function(a) unlist(strsplit(a[[1]], "/|-(?=\\^)", perl = TRUE))))
  unique(c(BENCHMARK, s, g, f))
}
