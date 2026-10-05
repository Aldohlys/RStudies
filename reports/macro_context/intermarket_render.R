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
         im_chg(m$c1d, m$kind), im_chg(m$c1w, m$kind), im_chg(m$c1m, m$kind), im_chg(m$c3m, m$kind),
         "<td>", a200, "</td><td>", pos_bar(m$pos52), "</td>",
         "<td>", trend_badge(m$trend), "</td><td>", sparkline(m$spark), "</td></tr>")
}

table_head <- "<tr class='im-head'><td>Instrument</td><td>Last</td><td>1D</td><td>1W</td><td>1M</td><td>3M</td><td>200-day</td><td>52-week range</td><td>Trend</td><td>1 year (EMA50 dashed)</td></tr>"

panel_html <- function(sec) {
  inst <- paste(vapply(sec$instruments, row_html, ""), collapse = "\n")
  rat <- if (length(sec$ratios)) paste0(
    "<div class='im-h3'>Relationships</div><div class='im-tw'><table>", sub("Instrument", "Ratio / spread", table_head),
    paste(vapply(sec$ratios, function(r) row_html(r, r$meaning), ""), collapse = "\n"), "</table></div>") else ""
  sprintf("<details class='im-panel' id='im-%s'><summary class='im-h2'>%s</summary><div class='im-tw'><table>%s%s</table></div>%s</details>",
          sec$id, esc(sec$title), table_head, inst, rat)
}

panels_html <- function(sections) paste(vapply(sections, panel_html, ""), collapse = "\n")

# Index groups for the last-session strip at the top of section 00
DAILY_STRIP <- list(
  "US" = c("^GSPC", "^NDX", "RSP", "IWM"),
  "Europe" = c("^STOXX50E", "^GDAXI", "^SSMI", "^FCHI", "^IBEX", "FTSEMIB.MI"),
  "Asia" = c("^N225", "^KS11", "^HSCE", "000001.SS"),
  "Latin America" = c("^BVSP", "^MXX"),
  "EM (USD)" = c("EEM")
)

short_date <- function(d) {
  lt <- as.POSIXlt(as.Date(d))
  sprintf("%s %d %s", c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[lt$wday + 1], lt$mday, month.abb[lt$mon + 1])
}

#' One line per region: each index's change from its previous close to its last bar.
#' Markets close on different days, so every figure carries the date of its last bar.
daily_strip_html <- function(sections) {
  all_m <- unlist(lapply(sections, `[[`, "instruments"), recursive = FALSE)
  by_sym <- setNames(all_m, vapply(all_m, `[[`, "", "sym"))
  rows <- vapply(names(DAILY_STRIP), function(region) {
    ms <- by_sym[intersect(DAILY_STRIP[[region]], names(by_sym))]
    if (!length(ms)) return("")
    items <- vapply(ms, function(m) {
      cls <- if (is.na(m$c1d)) "" else if (m$c1d > 0) "im-pos" else if (m$c1d < 0) "im-neg" else ""
      txt <- if (is.na(m$c1d)) "&ndash;" else sprintf("%+.2f%%", m$c1d)
      sprintf("<span class='im-day'>%s <b class='%s'>%s</b> <span class='im-sub'>%s</span></span>",
              esc(m$label), cls, txt, short_date(m$date))
    }, "")
    sprintf("<div class='im-day-row'><span class='im-day-reg'>%s</span>%s</div>", region, paste(items, collapse = ""))
  }, "")
  paste0("<div class='im-daily'><div class='im-h3'>Last session &mdash; change from previous close, local currency</div>",
         paste(rows, collapse = ""),
         "<div class='im-legend'>Date = day of the last bar. A bar dated on the report day can be intraday ",
         "if that market was still open when the report ran.</div></div>")
}

MOVIE_TITLES <- c(stocks = "Stock markets", fx = "Currencies", metals = "Precious metals", oil = "Oil",
                  commod = "Other commodities", rates = "Rates and credit", world = "World markets")

#' Scenario ETF -> verdicts of the map groups tagged with it (anchor, or the benchmark
#' most members carried before the groups existed), one badge per group, name on hover
verdict_tag <- function(etf, sectors) {
  hit <- vapply(strsplit(sectors$tags, " ", fixed = TRUE), function(t) etf %in% t, logical(1))
  if (!any(hit)) return(etf)
  paste0(etf, " ", paste(sprintf("<span class='im-badge im-v-%s' title='%s'>%s</span>",
                                 tolower(sectors$verdict[hit]), esc(sectors$group[hit]), sectors$verdict[hit]),
                         collapse = " "))
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

#' Conditional chains (archetypes with a `chain`), shown whatever the scenario's rank
chains_html <- function(z) {
  if (is.null(z)) return("")
  rows <- lapply(Filter(function(a) !is.null(a$chain), ARCHETYPES), function(a) {
    cs <- chain_status(a$chain, z)
    links <- paste(sprintf("<span class='%s'>%s</span>", ifelse(cs$on, "im-chain-on", "im-sub"), cs$steps),
                   collapse = " &rarr; ")
    sprintf("<p><b>%s</b> <span class='im-sub'>(%s)</span>. %s<br>%s<br><b>%s</b></p>",
            esc(a$chain$name), esc(a$name), a$chain$text, links, cs$status)
  })
  if (!length(rows)) return("")
  paste0("<div class='im-chains'><p class='im-sub'>Chains to watch: a link counts as moving when its 1-month z-score is +",
         CHAIN_ON, " or more.</p>", paste(rows, collapse = ""), "</div>")
}

movie_html <- function(movie, matches, sectors, z = NULL) {
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
    "<div class='im-cards'>", scenario_card(matches[[1]], sectors, 1), scenario_card(matches[[2]], sectors, 2), "</div>", chains_html(z),
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
           "<td>", esc(r[["group"]]), "<div class='im-sub'>", esc(r[["members"]]), "</div></td>",
           "<td class='im-num'>", trimws(r[["n_bot"]]), " / ", trimws(r[["n_members"]]),
           if (isTRUE(as.integer(r[["n_quoted"]]) < as.integer(r[["n_members"]])))
             sprintf(" <span class='im-sub'>(%s quoted)</span>", trimws(r[["n_quoted"]])) else "", "</td>",
           "<td>", trend_badge(r[["trend"]]), "</td><td>", r[["quadrant"]], "</td>",
           "<td class='im-num'>", num(r[["rs_ratio"]], "%.1f"), "</td>",
           "<td class='im-num'>", num(r[["rs_mom"]], "%+.1f"), "</td>",
           im_chg(as.numeric(r[["rs_3m"]])), im_chg(as.numeric(r[["c1m"]])),
           "<td class='im-num'>", num(r[["driver_score"]], "%+.2f"), "</td>",
           "<td class='im-sub'>", esc(r[["drivers"]]), "</td></tr>")
  })
  ug <- attr(sx, "ungrouped")
  ug_html <- if (is.null(ug) || !nrow(ug)) "" else paste0(
    "<p class='im-sub'><b>BOT names in no correlation group</b> (no map row; parent sectors are in the US sectors panel): ",
    esc(paste(paste0(ug$name, ifelse(is.na(ug$bench) | !nzchar(ug$bench), "", paste0(" (", ug$bench, ")"))),
              collapse = ", ")), "</p>")
  paste0("<div class='im-tw'><table><tr class='im-head'><td>Verdict</td><td>Group</td><td>BOT / members</td><td>Trend</td><td>Rotation</td>",
         "<td>RS ratio</td><td>RS mom</td><td>RS 3M</td><td>1M</td><td>Drivers</td><td>Driver detail</td></tr>",
         paste(rows, collapse = "\n"), "</table></div>", ug_html,
         "<div class='im-legend'>Rows = correlation groups (ScannerUniverse, cluster review) holding at least one BOT-eligible name. ",
         "Each group is read as the equal-weight index of all its members (mean daily dividend-adjusted return, compounded; ",
         "a day counts when at least half the members trade), never through an anchor ETF; BOT's S3 compares a name with the same members. ",
         "BOT / members = BOT-eligible names / all names in the group. ",
         "Trend: UP = close above EMA50 and EMA20 above EMA50; DOWN = mirror; MIXED = neither. ",
         "RS ratio = 100 &times; (group index / S&amp;P 500) divided by its 50-day average. RS mom = 10-day change of RS ratio. ",
         "Rotation: Leading = RS ratio &ge; 100 and RS mom &ge; 0; Weakening = &ge; 100, mom &lt; 0; Improving = &lt; 100, mom &ge; 0; Lagging = &lt; 100, mom &lt; 0. ",
         "RS 3M = 63-day change of group index / S&amp;P 500. Drivers = mean of (sensitivity sign &times; driver trend, UP +1 / DOWN &minus;1 / MIXED 0), &minus;1 to +1. ",
         "Verdict: LONG = trend UP, rotation Leading or Improving, drivers &ge; 0. SHORT = trend DOWN, rotation Lagging or Weakening, drivers &le; 0. ",
         "AVOID = trend MIXED, or drivers &le; &minus;0.5 against an UP trend (&ge; +0.5 against a DOWN trend). WATCH = all other cases.</div>")
}
