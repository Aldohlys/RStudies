# intermarket_render.R — HTML fragments for the macro_context template:
# {{MOVIE}} global movie + scenario match, {{PANELS}} asset-class tables, {{SECTOR_MAP}} BOT sector map

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

fmt_num <- function(x, kind = "price") {
  if (is.na(x)) return("&ndash;")
  if (kind == "yield") return(sprintf("%.2f%%", x))
  if (abs(x) >= 10000) return(formatC(x, format = "f", digits = 0, big.mark = ","))
  if (abs(x) >= 100) return(formatC(x, format = "f", digits = 1, big.mark = ","))
  if (abs(x) >= 1) return(sprintf("%.2f", x))
  sprintf("%.4f", x)
}

im_chg <- function(x, kind = "price") {
  if (is.na(x)) return("<td class='im-num'>&ndash;</td>")
  cls <- if (x > 0) "im-pos" else if (x < 0) "im-neg" else ""
  txt <- if (kind == "yield") sprintf("%+.0f bp", x) else sprintf("%+.1f%%", x)
  sprintf("<td class='im-num %s'>%s</td>", cls, txt)
}

trend_badge <- function(t) {
  if (is.na(t)) return("<span class='im-badge'>n/a</span>")
  sprintf("<span class='im-badge im-t-%s'>%s</span>", tolower(t), t)
}

pos_bar <- function(p) {
  if (is.na(p)) return("&ndash;")
  sprintf("<div class='im-bar' title='%.0f%% of 52-week range'><i style='left:%.0f%%'></i></div>", p, p)
}

sparkline <- function(sp, w = 130, h = 30) {
  if (is.null(sp) || nrow(sp) < 2) return("")
  rng <- range(c(sp$close, sp$ema50), na.rm = TRUE)
  if (diff(rng) == 0) return("")
  xs <- seq(0, w, length.out = nrow(sp))
  ys <- function(v) h - 2 - (v - rng[1]) / diff(rng) * (h - 4)
  pts <- function(v) paste(sprintf("%.1f,%.1f", xs, ys(v))[!is.na(v)], collapse = " ")
  sprintf(paste0("<svg class='im-spark' viewBox='0 0 %d %d' width='%d' height='%d' aria-hidden='true'>",
                 "<polyline class='im-sp-ema' points='%s'/><polyline class='im-sp-px' points='%s'/></svg>"),
          w, h, w, h, pts(sp$ema50), pts(sp$close))
}

row_html <- function(m, meaning = NULL) {
  label <- if (is.null(meaning)) esc(m$label) else
    sprintf("%s<div class='im-sub'>%s</div>", esc(m$label), esc(meaning))
  a200 <- if (is.na(m$above200)) "&ndash;" else if (m$above200) "above" else "<span class='im-neg'>below</span>"
  paste0("<tr><td>", label, "</td>",
         "<td class='im-num'>", fmt_num(m$last, m$kind), "</td>",
         im_chg(m$c1w, m$kind), im_chg(m$c1m, m$kind), im_chg(m$c3m, m$kind),
         "<td>", a200, "</td><td>", pos_bar(m$pos52), "</td>",
         "<td>", trend_badge(m$trend), "</td><td>", sparkline(m$spark), "</td></tr>")
}

table_head <- "<tr class='im-head'><td>Instrument</td><td>Last</td><td>1W</td><td>1M</td><td>3M</td><td>200-day</td><td>52-week range</td><td>Trend</td><td>1 year (EMA50 dashed)</td></tr>"

panel_html <- function(sec) {
  inst <- paste(vapply(sec$instruments, row_html, ""), collapse = "\n")
  rat <- if (length(sec$ratios)) paste0(
    "<div class='im-h3'>Relationships</div><div class='im-tw'><table>", sub("Instrument", "Ratio / spread", table_head),
    paste(vapply(sec$ratios, function(r) row_html(r, r$meaning), ""), collapse = "\n"), "</table></div>") else ""
  sprintf("<div class='im-panel' id='im-%s'><div class='im-h2'>%s</div><div class='im-tw'><table>%s%s</table></div>%s</div>",
          sec$id, esc(sec$title), table_head, inst, rat)
}

panels_html <- function(sections) paste(vapply(sections, panel_html, ""), collapse = "\n")

MOVIE_TITLES <- c(stocks = "Stock markets", fx = "Currencies", metals = "Precious metals", oil = "Oil",
                  commod = "Other commodities", rates = "Rates and credit", world = "World markets")

#' Bench ETF -> today's verdict text, to check scenario implications against the sector map
verdict_tag <- function(etf, sectors) {
  v <- sectors$verdict[sectors$bench == etf]
  if (length(v) == 0) return(sprintf("%s", etf))
  sprintf("%s <span class='im-badge im-v-%s'>%s</span>", etf, tolower(v[1]), v[1])
}

#' 60-day score line with the "in place" threshold dashed
score_spark <- function(h, w = 160, h_px = 34) {
  if (is.null(h) || length(h) < 2) return("")
  lo <- -1; hi <- 1
  xs <- seq(0, w, length.out = length(h))
  y <- function(v) h_px - 2 - (v - lo) / (hi - lo) * (h_px - 4)
  sprintf(paste0("<svg class='im-spark' viewBox='0 0 %d %d' width='%d' height='%d' aria-label='60-day match score'>",
                 "<line class='im-sp-ema' x1='0' x2='%d' y1='%.1f' y2='%.1f'/><polyline class='im-sp-px' points='%s'/></svg>"),
          w, h_px, w, h_px, w, y(SCEN_ACTIVE), y(SCEN_ACTIVE), paste(sprintf("%.1f,%.1f", xs, y(h)), collapse = " "))
}

age_badge <- function(st) sprintf("<span class='im-badge im-age-%s'>%s</span>", tolower(st), st)

scenario_card <- function(m, sectors, rank) {
  a <- m$a
  impl <- function(etfs) if (length(etfs)) paste(vapply(etfs, verdict_tag, "", sectors = sectors), collapse = " ") else "none"
  sprintf(paste0(
    "<div class='im-card%s'><div class='im-card-h'><span class='im-card-name'>%s</span>",
    "<span class='im-score'>%+.0f%% <span class='im-sub'>1M</span> &middot; %+.0f%% <span class='im-sub'>3M</span></span></div>",
    "<p class='im-age'>%s %s <span class='im-sub'>last 60 trading days, dashed = in place (%.0f%%)</span><br>%s</p>",
    "<p>%s</p><p><b>Past episodes.</b> %s</p><p><b>What came next.</b> %s</p>",
    "<p><b>Signs to watch.</b> %s</p><p><b>Not this scenario if.</b> %s</p>",
    "<p><b>BOT.</b> %s<br>Long side: %s<br>Short side: %s</p></div>"),
    if (rank == 1) " im-top" else "", esc(a$name), 100 * m$score, 100 * m$score3m,
    age_badge(m$age$status), score_spark(m$hist), 100 * SCEN_ACTIVE, m$age$text,
    a$movie, a$analogs, a$after, a$tells, a$invalid, a$bot$note, impl(a$bot$long), impl(a$bot$short))
}

movie_html <- function(movie, matches, sectors) {
  paras <- paste(vapply(names(MOVIE_TITLES), function(k)
    sprintf("<p><b>%s.</b> %s</p>", MOVIE_TITLES[[k]], movie[[k]]), ""), collapse = "\n")
  all_scores <- paste(vapply(matches, function(m) sprintf(
    "<tr><td>%s</td><td class='im-num'>%+.0f%%</td><td class='im-num'>%+.0f%%</td><td>%s</td><td class='im-num'>%s</td><td class='im-num im-sub'>%s</td><td>%s</td></tr>",
    esc(m$name), 100 * m$score, 100 * m$score3m, age_badge(m$age$status),
    if (m$age$run > 0) m$age$run else "&ndash;",
    paste(sprintf("%+.0f", 100 * m$prev5), collapse = " "), score_spark(m$hist, 120, 26)), ""), collapse = "")
  paste0(
    "<div class='im-movie'>", paras,
    "<p class='im-fit'><b>How it fits together.</b> ", movie$fit, "</p></div>",
    "<div class='im-cards'>", scenario_card(matches[[1]], sectors, 1), scenario_card(matches[[2]], sectors, 2), "</div>",
    "<details class='im-details'><summary>All scenarios: match score</summary><table>",
    "<tr class='im-head'><td>Scenario</td><td>1 month</td><td>3 months</td><td>Age</td><td>Days in place</td><td>Previous 5 reports (oldest first)</td><td>Last 60 days</td></tr>", all_scores, "</table>",
    "<div class='im-legend'>Match = weighted agreement between today's moves and the scenario's typical moves, ",
    "-100% (opposite) to +100% (identical). Each asset's move is measured as a z-score: the 1-month (or 3-month) change ",
    "divided by its usual volatility over that horizon, so moves in different assets are comparable. ",
    "A 1-month score above the 3-month score means the scenario is emerging; below it, the scenario is fading. ",
    "Age (scores recomputed from price history for each of the last 60 trading days; in place = score &ge; 40%): ",
    "NEW = in place today but in at most one of the previous five reports, be cautious; BUILDING = in place under 15 days and strengthening; ",
    "WAVERING = under 15 days and weakening; ESTABLISHED = 15-39 days; MATURE = 40 days or more, may be near its end; ",
    "FADED = in place in one of the previous five reports but not today.</div></details>")
}

sectors_html <- function(sx) {
  rows <- apply(sx, 1, function(r) {
    num <- function(v, f) { v <- as.numeric(v); if (is.na(v)) "&ndash;" else sprintf(f, v) }
    paste0("<tr><td><span class='im-badge im-v-", tolower(r[["verdict"]]), "'>", r[["verdict"]], "</span></td>",
           "<td>", esc(r[["group"]]), "</td><td>", r[["bench"]], "</td>",
           "<td>", trend_badge(r[["trend"]]), "</td><td>", r[["quadrant"]], "</td>",
           "<td class='im-num'>", num(r[["rs_ratio"]], "%.1f"), "</td>",
           "<td class='im-num'>", num(r[["rs_mom"]], "%+.1f"), "</td>",
           im_chg(as.numeric(r[["rs_3m"]])), im_chg(as.numeric(r[["c1m"]])),
           "<td class='im-num'>", num(r[["driver_score"]], "%+.2f"), "</td>",
           "<td class='im-sub'>", esc(r[["drivers"]]), "</td></tr>")
  })
  paste0("<div class='im-tw'><table><tr class='im-head'><td>Verdict</td><td>Group</td><td>ETF</td><td>Trend</td><td>Rotation</td>",
         "<td>RS ratio</td><td>RS mom</td><td>RS 3M</td><td>1M</td><td>Drivers</td><td>Driver detail</td></tr>",
         paste(rows, collapse = "\n"), "</table></div>",
         "<div class='im-legend'>Trend: UP = close above EMA50 and EMA20 above EMA50; DOWN = mirror; MIXED = neither. ",
         "RS ratio = 100 &times; (ETF / S&amp;P 500) divided by its 50-day average. RS mom = 10-day change of RS ratio. ",
         "Rotation: Leading = RS ratio &ge; 100 and RS mom &ge; 0; Weakening = &ge; 100, mom &lt; 0; Improving = &lt; 100, mom &ge; 0; Lagging = &lt; 100, mom &lt; 0. ",
         "RS 3M = 63-day change of ETF / S&amp;P 500. Drivers = mean of (sensitivity sign &times; driver trend, UP +1 / DOWN &minus;1 / MIXED 0), &minus;1 to +1. ",
         "Verdict: LONG = trend UP, rotation Leading or Improving, drivers &ge; 0. SHORT = trend DOWN, rotation Lagging or Weakening, drivers &le; 0. ",
         "AVOID = trend MIXED, or drivers &le; &minus;0.5 against an UP trend (&ge; +0.5 against a DOWN trend). WATCH = all other cases.</div>")
}
