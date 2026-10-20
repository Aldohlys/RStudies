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
  if (kind == "usd") return(sprintf("%s&#36;%.2f", if (x < 0) "&minus;" else "", abs(x)))
  if (abs(x) >= 10000) return(formatC(x, format = "f", digits = 0, big.mark = ","))
  if (abs(x) >= 100) return(formatC(x, format = "f", digits = 1, big.mark = ","))
  if (abs(x) >= 1) return(sprintf("%.2f", x))
  sprintf("%.4f", x)
}

im_chg <- function(x, kind = "price") {
  if (is.na(x)) return("<td class='im-num'>&ndash;</td>")
  cls <- if (x > 0) "im-pos" else if (x < 0) "im-neg" else ""
  txt <- if (kind == "yield") sprintf("%+.0f bp", x) else
    if (kind == "usd") sprintf("%s&#36;%.2f", if (x < 0) "&minus;" else "+", abs(x)) else sprintf("%+.1f%%", x)
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
  if (!is.null(m$stale_missing))
    label <- sprintf("%s<div class='im-sub im-neg'>No close from Yahoo for %s: figures stop at %s</div>",
                     label, short_date(m$stale_missing), short_date(m$date))
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
#' Warning under the strip: symbols whose last bar had no close (empty_last_closes)
stale_html <- function(sections, stale) {
  if (is.null(stale) || !nrow(stale)) return("")
  all_m <- unlist(lapply(sections, `[[`, "instruments"), recursive = FALSE)
  lab <- setNames(vapply(all_m, `[[`, "", "label"), vapply(all_m, `[[`, "", "sym"))
  shown <- intersect(stale$ticker, names(lab))
  fp <- names(Filter(function(a) any(unlist(strsplit(a[[1]], "/|-(?=\\^)", perl = TRUE)) %in% stale$ticker),
                     FP_ASSETS[names(FP_ASSETS) != "ABS_BREADTH"]))
  n_other <- length(setdiff(stale$ticker, shown))
  days <- paste(unique(vapply(sort(unique(stale$missing)), short_date, "")), collapse = ", ")
  paste0("<div class='stale-banner'>NO CLOSE FROM YAHOO FOR ", days, " &mdash; ", nrow(stale), " symbols",
         "<span class='stale-sub'>Re-fetched once, still empty; these use their previous close. ",
         if (length(shown)) paste0("Panels: ", esc(paste(lab[shown], collapse = ", ")), ". ") else "",
         if (length(fp)) paste0("Scenario inputs one session old: ", esc(paste(vapply(fp, fp_label, ""), collapse = ", ")), ". ") else "",
         if (n_other) sprintf("Also %d other symbols (sector-map members or ratio legs).", n_other) else "",
         "</span></div>")
}

daily_strip_html <- function(sections) {
  all_m <- unlist(lapply(sections, `[[`, "instruments"), recursive = FALSE)
  by_sym <- setNames(all_m, vapply(all_m, `[[`, "", "sym"))
  shown <- by_sym[intersect(unlist(DAILY_STRIP), names(by_sym))]
  dts <- vapply(shown, function(m) as.character(m$date), "")
  common <- names(sort(table(dts), decreasing = TRUE))[1]
  rows <- vapply(names(DAILY_STRIP), function(region) {
    ms <- by_sym[intersect(DAILY_STRIP[[region]], names(by_sym))]
    if (!length(ms)) return("")
    items <- vapply(ms, function(m) {
      cls <- if (is.na(m$c1d)) "" else if (m$c1d > 0) "im-pos" else if (m$c1d < 0) "im-neg" else ""
      txt <- if (is.na(m$c1d)) "&ndash;" else sprintf("%+.2f%%", m$c1d)
      if (!is.null(m$stale_missing)) cls <- paste(cls, "im-stale")
      own <- if (as.character(m$date) != common) sprintf(" <span class='im-sub'>(%s)</span>", short_date(m$date)) else ""
      sprintf("<span class='im-day'>%s <b class='%s'>%s</b>%s</span>", esc(m$label), cls, txt, own)
    }, "")
    sprintf("<div class='im-day-row'><span class='im-day-reg'>%s</span>%s</div>", region, paste(items, collapse = ""))
  }, "")
  paste0("<div class='im-daily'><div class='im-h3'>Last session (", short_date(common), "), change from previous close, local currency</div>",
         paste(rows, collapse = ""), "</div>")
}

MOVIE_TITLES <- c(stocks = "Stock markets", fx = "Currencies", metals = "Precious metals", oil = "Oil",
                  commod = "Other commodities", rates = "Rates and credit", world = "World markets")

#' Sector-map groups behind a list of scenario ETFs: each group once, whichever ETF tags it (tags = the group's
#' anchor and the benchmark most members carried before the groups existed). Returns verdict -> group names.
scenario_groups <- function(etfs, sectors) {
  if (!length(etfs) || is.null(sectors) || !nrow(sectors)) return(list())
  tags <- strsplit(sectors$tags, " ", fixed = TRUE)
  hit <- which(vapply(tags, function(tg) any(etfs %in% tg), logical(1)))
  split(sub("^[^-]+ - ", "", sectors$group[hit]), sectors$verdict[hit])
}

#' Scenario x sector map table: the scenario's expected direction against today's section-10 verdict.
#' Rows only when they hold a group; WATCH / AVOID = price has not confirmed either way.
bot_table_html <- function(a, sectors) {
  up <- scenario_groups(a$bot$long, sectors); dn <- scenario_groups(a$bot$short, sectors)
  g <- function(x, v) if (is.null(x[[v]])) character(0) else x[[v]]
  ln <- function(v, grp) if (length(grp)) sprintf("<span class='im-badge im-v-%s'>%s</span> %s", tolower(v), v, esc(paste(grp, collapse = ", "))) else ""
  rows <- list(
    list("Scenario LONG, Sector map LONG", esc(paste(g(up, "LONG"), collapse = ", ")), "bt-agree"),
    list("Scenario SHORT, Sector map SHORT", esc(paste(g(dn, "SHORT"), collapse = ", ")), "bt-agree"),
    list("Scenario LONG, Sector map SHORT", esc(paste(g(up, "SHORT"), collapse = ", ")), "bt-conflict"),
    list("Scenario SHORT, Sector map LONG", esc(paste(g(dn, "LONG"), collapse = ", ")), "bt-conflict"),
    list("Sector map WATCH / AVOID", paste(Filter(nzchar, c(ln("WATCH", c(g(up, "WATCH"), g(dn, "WATCH"))),
                                                          ln("AVOID", c(g(up, "AVOID"), g(dn, "AVOID"))))), collapse = "<br>"), "bt-plain"))
  rows <- Filter(function(r) nzchar(r[[2]]), rows)
  if (!length(rows)) return("")
  leg_note("00 &mdash; scenario vs sector map table", paste0(
    "Scenario LONG / SHORT = groups the scenario expects to rise / fall: the sector-map groups tagged with the ETFs listed for the scenario in archetypes.R (bot$long, bot$short). ",
    "Sector map = today's section-10 verdict, one per group. Rows with no group are not shown. ",
    "Sector map WATCH / AVOID = neither agrees nor conflicts: price has not confirmed the scenario's direction ",
    "(WATCH = trend and rotation disagree; AVOID = trend mixed, or drivers against the trend)."))
  paste0("<table class='bt'>", paste(vapply(rows, function(r) sprintf("<tr><th>%s</th><td class='%s'>%s</td></tr>", r[[1]], r[[3]], r[[2]]), ""), collapse = ""),
         "</table>")
}

#' 60-day lines: state solid, move dotted, the state "in place" threshold dashed
score_spark <- function(h, w = 160, h_px = 34, hm = NULL) {
  if (is.null(h) || length(h) < 2) return("")
  lo <- -1; hi <- 1
  xs <- seq(0, w, length.out = length(h))
  y <- function(v) h_px - 2 - (pmax(lo, pmin(hi, v)) - lo) / (hi - lo) * (h_px - 4)
  pts <- function(v) { ok <- !is.na(v); paste(sprintf("%.1f,%.1f", xs[ok], y(v[ok])), collapse = " ") }
  mv <- if (is.null(hm) || length(hm) != length(h)) "" else sprintf("<polyline class='im-sp-mv' points='%s'/>", pts(hm))
  sprintf(paste0("<svg class='im-spark' viewBox='0 0 %d %d' width='%d' height='%d' aria-label='60-day state and move scores'>",
                 "<line class='im-sp-ema' x1='0' x2='%d' y1='%.1f' y2='%.1f'/>%s<polyline class='im-sp-px' points='%s'/></svg>"),
          w, h_px, w, h_px, w, y(STATE_ACTIVE), y(STATE_ACTIVE), mv, pts(h))
}

score_pair <- function(m) sprintf(
  "state %s &middot; move %+.0f%% <span class='im-sub'>1M</span> / %+.0f%% <span class='im-sub'>3M</span>",
  if (is.na(m$state)) "n/a" else sprintf("%+.0f%%", 100 * m$state), 100 * m$score, 100 * m$score3m)

age_badge <- function(st) sprintf("<span class='im-badge im-age-%s'>%s</span>", tolower(st), st)

#' What moved a scenario's state between two sessions: per fingerprint asset, its position in its one-year range
#' (signed as in FP_ASSETS) then and now, and its contribution change = weight x (state now - state then) / sum |weight|.
#' The contributions add up to the state score, so the column sums to the state change.
state_drivers_html <- function(m, st, st5, d5) {
  if (is.null(st) || is.null(st5)) return("")
  fp <- m$a$fp
  k <- intersect(names(fp), names(st)[!is.na(st) & !is.na(st5[names(st)])])
  if (!length(k)) return("")
  den <- sum(abs(fp[k]))
  ch <- fp[k] * (st[k] - st5[k]) / den
  o <- order(-abs(ch))
  rows <- vapply(o, function(i) {
    a <- k[i]
    sprintf("<tr><td>%s</td><td class='im-num'>%+g</td><td class='im-num'>%+.0f</td><td class='im-num'>%+.0f</td><td class='im-num %s'>%+.1f</td></tr>",
            esc(fp_label(a)), fp[[a]], 100 * st5[[a]], 100 * st[[a]], if (ch[i] > 0) "im-pos" else if (ch[i] < 0) "im-neg" else "", 100 * ch[i])
  }, "")
  s0 <- sum(fp[k] * st5[k]) / den; s1 <- sum(fp[k] * st[k]) / den
  leg_note("Detail 00b &mdash; what moved the state", paste0(
    "Per fingerprint asset: weight in the scenario, position in its one-year range (&minus;100 bottom, +100 top, signed so that + means toward the scenario's expected direction) ",
    "5 sessions ago and now, and the change in its contribution to the state score = weight &times; (position now &minus; position then) / &Sigma; |weight|, in points. ",
    "The column adds up to the state change. Sorted by size of the change."))
  sprintf(paste0("<details class='im-details' open><summary>What moved the state since %s: %+.0f%% &rarr; %+.0f%%</summary>",
                 "<table class='im-drv'><tr class='im-head'><td>Asset</td><td>Weight</td><td>Then</td><td>Now</td><td>Change (pts)</td></tr>%s</table></details>"),
          short_date(d5), 100 * s0, 100 * s1, paste(rows, collapse = ""))
}

#' The scenario on top (highest state, in place) 5 sessions ago when it is no longer one of the two cards:
#' its "what moved the state" table, so a change of top scenario is explained from both sides
lost_top_html <- function(matches, st, st5, d5) {
  n <- length(matches[[1]]$hist)
  if (n < 6) return("")
  s5 <- vapply(matches, function(m) if (length(m$hist) == n) m$hist[n - 5] else NA_real_, 0)
  if (all(is.na(s5)) || max(s5, na.rm = TRUE) < STATE_ACTIVE) return("")
  j <- which.max(s5)
  if (j <= 2) return("")
  m <- matches[[j]]
  sprintf("<div class='im-card'><div class='im-card-h'><span class='im-card-name'>Lost the top place: %s</span><span class='im-score'>%s</span></div><p class='im-age'>%s Top scenario on %s (state %+.0f%%).</p>%s</div>",
          esc(m$a$name), score_pair(m), age_badge(m$age$status), short_date(d5), 100 * s5[j], state_drivers_html(m, st, st5, d5))
}

#' Scenario card. compact = Main tab (status, confirms / ends if, BOT table); full = Detail (all texts)
scenario_card <- function(m, sectors, rank, compact = FALSE, drivers = "") {
  a <- m$a
  leg_note("00 &mdash; scenario cards", sprintf(
    "Line next to the status: last 60 trading days; solid = state, dotted = move, dashed = in place (state %.0f%%).", 100 * STATE_ACTIVE))
  head <- sprintf(paste0("<div class='im-card%s'><div class='im-card-h'><span class='im-card-name'>%s</span>",
                         "<span class='im-score'>%s</span></div><p class='im-age'>%s %s<br>%s</p>"),
                  if (rank == 1) " im-top" else "", esc(a$name), score_pair(m),
                  age_badge(m$age$status), score_spark(m$hist, hm = m$hist_move), m$age$text)
  bot <- sprintf("<div class='bot-box'><p class='im-compact'><b>BOT, what followed historically.</b> %s</p>%s</div>", a$bot$note, bot_table_html(a, sectors))
  if (compact) return(paste0(head,
    sprintf("<p class='im-compact'><b>Confirms:</b> %s</p><p class='im-compact'><b>Ends if:</b> %s</p>", a$tells, a$invalid),
    bot, "</div>"))
  paste0(head,
         sprintf("<p>%s</p><p><b>Past episodes.</b> %s</p><p><b>What came next.</b> %s</p><p><b>Signs to watch.</b> %s</p><p><b>Not this scenario if.</b> %s</p>",
                 a$movie, a$analogs, a$after, a$tells, a$invalid),
         drivers, bot, "</div>")
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

#' Yen carry-unwind alert (3-session window), shown whatever the scenario ranking
carry_html <- function(carry) {
  if (is.null(carry)) return("")
  ca <- CARRY_ALERT; t <- carry$today
  f <- function(v) if (is.na(v)) "n/a" else sprintf("%+.1f", v)
  cls <- switch(t$status, FIRING = "im-age-mature", WATCH = "im-age-new", "im-age-wavering")
  recent <- if (!is.null(carry$last_fired) && t$status != "FIRING")
    sprintf(" Last fired %s, within the last %d sessions.", format(carry$last_fired, "%d %b"), CARRY_LOOKBACK) else ""
  sprintf(paste0(
    "<div class='im-chains'><p><b>%s</b> <span class='im-badge %s'>%s</span> <span class='im-sub'>%d-session window, %s</span><br>",
    "Yen z %s (fires at +%g, watch at +%g); confirmations at z &le; &minus;%g: AUD/JPY %s, Nikkei %s, Bitcoin %s; VIX at or above VIX3M: %s. ",
    "%d of 4 confirmations.%s</p>",
    "<p class='im-sub'>FIRING = yen &ge; +%g and at least 2 confirmations; WATCH = yen &ge; +%g and at least 1. %s %s</p></div>"),
    esc(ca$name), cls, t$status, ca$window, format(t$date, "%d %b"),
    f(t$yen), ca$yen_fire, ca$yen_watch, ca$confirm_z, f(t$audjpy), f(t$nikkei), f(t$btc), if (t$vix_inverted) "yes" else "no",
    t$n_conf, recent, ca$yen_fire, ca$yen_watch,
    if (t$status == "QUIET") "" else ca$movie, if (t$status == "QUIET") "" else paste("BOT, what followed historically:", ca$bot))
}

#' Section 00 on the Main tab: top scenario (compact), what the month says, last-session strip
movie_main_html <- function(movie, matches, sectors, sections) {
  txt <- paste(unlist(movie), collapse = " ")
  reads <- regmatches(txt, gregexpr("<span class='im-read'>[^<]*</span>", txt))[[1]]
  reads <- trimws(gsub("<[^>]+>", "", reads))
  paste0(
    scenario_card(matches[[1]], sectors, 1, compact = TRUE),
    if (length(reads)) paste0("<div class='im-reads'><b>What the month says:</b><ul>", paste(sprintf("<li>%s</li>", reads), collapse = ""), "</ul></div>") else "",
    daily_strip_html(sections),
    "<div class='proto-link'>Full movie, scenario description and history, other candidates, scenario table, chains, yen carry: ",
    "<a href='#detail' onclick=\"showTab('detail');return false;\">Detail tab, 00b</a></div>")
}

#' Section 00b on the Detail tab: the full movie, both cards, ranked and full scenario tables, chains, yen carry
movie_detail_html <- function(movie, matches, sectors, z = NULL, carry = NULL, st = NULL, st5 = NULL, d5 = NULL) {
  paras <- paste(vapply(names(MOVIE_TITLES), function(k)
    sprintf("<p><b>%s.</b> %s</p>", MOVIE_TITLES[[k]], movie[[k]]), ""), collapse = "\n")
  all_scores <- paste(vapply(matches, function(m) sprintf(
    "<tr><td>%s</td><td class='im-num'>%s</td><td class='im-num'>%+.0f%%</td><td class='im-num'>%+.0f%%</td><td>%s</td><td class='im-num'>%s</td><td class='im-num im-sub'>%s</td><td>%s</td></tr>",
    esc(m$name), if (is.na(m$state)) "n/a" else sprintf("%+.0f%%", 100 * m$state), 100 * m$score, 100 * m$score3m,
    age_badge(m$age$status), if (m$age$run > 0) m$age$run else "&ndash;",
    paste(sprintf("%+.0f", 100 * m$prev5), collapse = " "), score_spark(m$hist, 120, 26, m$hist_move)), ""), collapse = "")
  leg_note("Detail 00b &mdash; ranked scenario table", sprintf(paste0(
    "Numbers match the map. State = where each asset stands in its one-year range against the scenario's fingerprint; ",
    "in place from %+.0f%% for %d sessions in a row. Move 1M = how far the last month's moves point toward it; strong from %+.0f%%. Tails on the map: the last 5 sessions."),
    100 * STATE_ACTIVE, MIN_IN_PLACE, 100 * SCEN_ACTIVE))
  leg_note("Detail 00b &mdash; all scenarios: state and move scores", paste0(
    "Both scores are weighted agreements with the scenario's fingerprint, &minus;100% (opposite) to +100% (identical). ",
    sprintf("State = where each asset stands in its range of the last %d sessions (bottom &minus;1, top +1): it decides whether the scenario is in place (state &ge; %.0f%%, %d sessions in a row). ",
            STATE_WINDOW, 100 * STATE_ACTIVE, MIN_IN_PLACE),
    "Move = each asset's change over the last 21 sessions (Move 1M) or 63 sessions (Move 3M), divided by its usual volatility over that horizon: it gives the direction. ",
    sprintf("Status is recomputed for each of the last %d trading days (glossary L1, M1). ", HIST_DAYS),
    "Ranked by state. Thresholds calibrated on dated 2004-2026 episodes (M1)."))
  paste0(
    "<div class='im-movie'>", paras,
    "<p class='im-fit'><b>How it fits together.</b> ", movie$fit, "</p></div>",
    "<div class='im-cards'>", scenario_card(matches[[1]], sectors, 1, drivers = state_drivers_html(matches[[1]], st, st5, d5)),
    scenario_card(matches[[2]], sectors, 2, drivers = state_drivers_html(matches[[2]], st, st5, d5)), "</div>",
    lost_top_html(matches, st, st5, d5),
    scenario_list_html(matches),
    chains_html(z), carry_html(carry),
    "<details class='im-details' open><summary>All scenarios: state and move scores</summary><table>",
    "<tr class='im-head'><td>Scenario</td><td>State</td><td>Move 1M</td><td>Move 3M</td><td>Status</td><td>Days in place</td><td>State, previous 5 reports (oldest first)</td><td>Last 60 days</td></tr>", all_scores, "</table></details>")
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
         leg_note("10 &mdash; BOT sector map", paste0("Rows = correlation groups (ScannerUniverse, cluster review) holding at least one BOT-eligible name. ",
         "Each group is read as the equal-weight index of all its members (mean daily dividend-adjusted return, compounded; ",
         "a day counts when at least half the members trade), never through an anchor ETF; BOT's S3 compares a name with the same members. ",
         "BOT / members = BOT-eligible names / all names in the group. ",
         "Trend: UP = close above EMA50 and EMA20 above EMA50; DOWN = mirror; MIXED = neither. ",
         "RS ratio = 100 &times; (group index / S&amp;P 500) divided by its 50-day average. RS mom = 10-day change of RS ratio. ",
         "Rotation: Leading = RS ratio &ge; 100 and RS mom &ge; 0; Weakening = &ge; 100, mom &lt; 0; Improving = &lt; 100, mom &ge; 0; Lagging = &lt; 100, mom &lt; 0. ",
         "RS 3M = 63-day change of group index / S&amp;P 500. Drivers = mean of (sensitivity sign &times; driver trend, UP +1 / DOWN &minus;1 / MIXED 0), &minus;1 to +1. ",
         "Verdict: LONG = trend UP, rotation Leading or Improving, drivers &ge; 0. SHORT = trend DOWN, rotation Lagging or Weakening, drivers &le; 0. ",
         "AVOID = trend MIXED, or drivers &le; &minus;0.5 against an UP trend (&ge; +0.5 against a DOWN trend). WATCH = all other cases.")))
}

#' Year-to-date band per asset: grey bar = lowest to highest return since 31 Dec, dot = today
ytd_band_html <- function(y) {
  if (is.null(y) || !nrow(y)) return("")
  lo <- min(0, y$low); hi <- max(0, y$high); pad <- (hi - lo) * 0.03; lo <- lo - pad; hi <- hi + pad
  x <- function(v) 100 * (v - lo) / (hi - lo)
  rows <- vapply(seq_len(nrow(y)), function(i) {
    r <- y[i, ]
    sprintf(paste0("<tr><td class='ytd-name'>%s</td><td class='ytd-cell'><div class='ytd-track'>",
                   "<div class='ytd-zero' style='left:%.1f%%'></div>",
                   "<div class='ytd-range' style='left:%.1f%%;width:%.1f%%'></div>",
                   "<div class='ytd-dot %s' style='left:%.1f%%'></div></div></td>",
                   "<td class='ytd-num %s'>%+.1f%%</td><td class='ytd-num ytd-dim'>%+.1f / %+.1f</td></tr>"),
            esc(r$name), x(0), x(r$low), max(0.4, x(r$high) - x(r$low)),
            if (r$cur >= 0) "ytd-up" else "ytd-dn", x(r$cur),
            if (r$cur >= 0) "ytd-up" else "ytd-dn", r$cur, r$low, r$high)
  }, "")
  paste0("<div class='im-h3'>Year to date &mdash; range since 31 Dec and where each asset stands now</div>",
         "<table class='ytd-table'><tr class='im-head'><td></td><td></td><td>YTD</td><td>Low / high</td></tr>",
         paste(rows, collapse = ""), "</table>",
         leg_note("09 &mdash; year to date band", paste0("Grey bar = lowest to highest return reached since the last close of last year; dot = today; ",
         "vertical line = 0. Price returns in each asset's own currency (EEM, LQD, HYG in USD); gold and Brent on the front-month future (not roll-adjusted); bond ETFs fall when yields rise. ",
         "A dot near the left end of its bar = the asset gave back the year's gains.")))
}
