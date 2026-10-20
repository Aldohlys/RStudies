# summary_render.R — the one-screen summary at the top of the report ({{SUMMARY}}) and the
# cross-asset heatmap at the top of section 09 ({{HEATMAP}}).
#
# Summary: scenario and alert line, 8 key tiles, the scenario map (state vs move, 5-session
# tails, 60-day timeline of the scenario in place), what changed since the previous report
# (DB tables written by main.R), and the BOT sector verdicts.
# Heatmap: every panel instrument's 1D / 1W / 1M / 3M move divided by its usual volatility.
# Nothing here computes a signal: everything comes from run_intermarket() and regime_risk.R.

# ── Small helpers ───────────────────────────────────────────────────────────

#' Scenario name without its subtitle: "Stagflation / oil supply shock" -> "Stagflation"
short_name <- function(x) {
  x <- sub(" \\(.*$", "", x)
  p <- strsplit(x, " / ", fixed = TRUE)[[1]]
  if (length(p) > 1 && nchar(p[1]) > 3) p[1] else x
}

#' Instruments and ratios of every panel, by symbol ("HYG/IEF" for a ratio)
panel_by_sym <- function(sections) {
  all_m <- unlist(lapply(sections, function(s) c(s$instruments, s$ratios)), recursive = FALSE)
  all_m <- all_m[!duplicated(vapply(all_m, `[[`, "", "sym"))]
  setNames(all_m, vapply(all_m, `[[`, "", "sym"))
}

#' Plain sparkline of a numeric vector (no EMA)
mini_spark <- function(v, w = 110, h = 26) {
  v <- v[!is.na(v)]
  if (length(v) < 2 || diff(range(v)) == 0) return("")
  xs <- seq(0, w, length.out = length(v))
  ys <- h - 2 - (v - min(v)) / diff(range(v)) * (h - 4)
  sprintf("<svg class='ck-spark' viewBox='0 0 %d %d' width='%d' height='%d' aria-hidden='true'><polyline points='%s'/></svg>",
          w, h, w, h, paste(sprintf("%.1f,%.1f", xs, ys), collapse = " "))
}

#' Volatility-adjusted move of a panel row over n sessions: change / (stdev of daily
#' changes over 120 sessions x sqrt(n)), from the row's one-year series. Same definition
#' as move_z() (archetypes.R); yields and spreads in points, prices in log returns.
heat_z <- function(m, n) {
  v <- m$spark$close
  v <- v[!is.na(v)]
  k <- length(v)
  if (k < max(n + 1, 60)) return(NA_real_)
  lvl <- m$kind %in% c("yield", "usd")
  if (!lvl && any(v <= 0)) return(NA_real_)
  dx <- if (lvl) diff(v) else diff(log(v))
  sdv <- stats::sd(tail(dx, 120))
  if (is.na(sdv) || sdv == 0) return(NA_real_)
  mv <- if (lvl) v[k] - v[k - n] else log(v[k] / v[k - n])
  mv / (sdv * sqrt(n))
}

#' Heatmap class from a z-score: hz-p1..p3 up, hz-n1..n3 down, "" below 0.5
heat_class <- function(z) {
  if (is.na(z) || abs(z) < 0.5) return("")
  lvl <- if (abs(z) >= 2) 3 else if (abs(z) >= 1) 2 else 1
  sprintf("hz-%s%d", if (z > 0) "p" else "n", lvl)
}

chg_txt <- function(x, kind = "price") {
  if (is.na(x)) return("&ndash;")
  if (kind == "yield") return(sprintf("%+.0f bp", x))
  if (kind == "usd") return(sprintf("%s&#36;%.2f", if (x < 0) "&minus;" else "+", abs(x)))
  sprintf("%+.1f%%", x)
}

# ── Key tiles ───────────────────────────────────────────────────────────────
# sym = panel row giving the level and the sparkline; fp = FP_ASSETS key giving the
# 1-month z-score; tone = +1 when a rise supports risk and liquidity (principle 3),
# -1 when it drains it, 0 when it depends on the scenario.
SUMMARY_TILES <- list(
  list(label = "VIX",             sym = "^VIX",     fp = "VIX",     tone = -1),
  list(label = "MOVE",            sym = "^MOVE",    fp = "MOVE",    tone = -1),
  list(label = "US 10-year",      sym = "^TNX",     fp = "US10Y",   tone = -1),
  list(label = "Dollar (DXY)",    sym = "DX-Y.NYB", fp = "USD",     tone = -1),
  list(label = "Credit HYG/IEF",  sym = "HYG/IEF",  fp = "CREDIT",  tone = 1),
  list(label = "WTI crude",       sym = "CL=F",     fp = "OIL",     tone = 0),
  list(label = "Gold",            sym = "GC=F",     fp = "GOLD",    tone = 0),
  list(label = "Breadth",         sym = "S5FI",     fp = "ABS_BREADTH", tone = 1)
)
TILE_SPARK_DAYS <- 63

tile_class <- function(z, tone) {
  if (is.na(z) || abs(z) < 1) return("ck-flat")
  if (tone == 0) return("ck-hot")
  if (tone * z > 0) "ck-good" else "ck-bad"
}

tile_html <- function(t, by_sym, z1, breadth = NULL, bh = NULL, vix = NULL) {
  z <- if (t$fp %in% names(z1)) unname(z1[[t$fp]]) else NA_real_
  if (t$sym == "S5FI") {
    lvl <- if (is.null(breadth)) NA_real_ else breadth$pct
    hist <- if (is.null(bh) || !nrow(bh)) numeric(0) else tail(bh$pct, TILE_SPARK_DAYS)
    # One reading per day the report ran, so a month back is found by date, not by row count
    prev <- if (is.null(bh) || !nrow(bh)) NA_real_ else {
      j <- which(bh$date <= max(bh$date) - 30)
      if (length(j)) bh$pct[max(j)] else NA_real_
    }
    value <- if (is.na(lvl)) "&ndash;" else sprintf("%.0f%%", lvl)
    sub <- paste(if (is.na(prev) || is.na(lvl)) "1M &ndash;" else sprintf("1M %+.0f pts", lvl - prev), "&middot; stocks &gt; MA50")
    zt <- if (is.na(z)) "" else sprintf("z %+.1f", z)   # (level - 50) / 15, as in the scenario match
    spark <- mini_spark(hist)
  } else {
    m <- by_sym[[t$sym]]
    if (is.null(m)) return(sprintf("<div class='ck-tile ck-flat'><div class='ck-t-label'>%s</div><div class='ck-t-val'>&ndash;</div></div>", esc(t$label)))
    value <- fmt_num(m$last, m$kind)
    sub <- paste("1M", chg_txt(m$c1m, m$kind))
    zt <- if (is.na(z)) "" else sprintf("z %+.1f", z)
    spark <- mini_spark(tail(m$spark$close, TILE_SPARK_DAYS))
    if (t$sym == "^VIX" && !is.null(vix) && !is.null(vix$ratio) && !is.na(vix$ratio))
      sub <- sprintf("%s &middot; VIX/VIX3M %.2f%s", sub, vix$ratio, if (vix$ratio >= 1) " <b>inverted</b>" else "")
    if (!is.null(m$stale_missing)) sub <- paste(sub, "<span class='im-neg'>(no last close)</span>")
  }
  tip <- sprintf("1-month move z-score %s. Colour: blue = the move supports risk and liquidity, red = it drains them, black = large move whose reading depends on the scenario (|z| &ge; 1).",
                 if (is.na(z)) "n/a" else sprintf("%+.2f", z))
  sprintf(paste0("<div class='ck-tile %s' title='%s'><div class='ck-t-label'>%s</div>",
                 "<div class='ck-t-val'>%s<span class='ck-t-z'>%s</span></div><div class='ck-t-sub'>%s</div>%s</div>"),
          tile_class(z, t$tone), tip, esc(t$label), value, zt, sub, spark)
}

# ── Scenario map: state (x) vs 1-month move (y) ─────────────────────────────

STATUS_ORDER <- c("BUILDING", "ESTABLISHED", "MATURE", "FADING", "PENDING", "EMERGING", "FADED", "INACTIVE")

scenario_map_svg <- function(matches) {
  W <- 460; H <- 330; L <- 44; R <- 22; T <- 12; B <- 38
  px <- function(x) L + (pmax(-1, pmin(1, x)) + 1) / 2 * (W - L - R)
  py <- function(y) T + (1 - (pmax(-1, pmin(1, y)) + 1) / 2) * (H - T - B)
  xa <- STATE_ACTIVE; ya <- SCEN_ACTIVE
  rect <- function(x0, x1, y0, y1, cls) sprintf("<rect class='%s' x='%.1f' y='%.1f' width='%.1f' height='%.1f'/>",
                                                cls, px(x0), py(y1), px(x1) - px(x0), py(y0) - py(y1))
  lab <- function(x, y, txt, anchor = "start") sprintf("<text class='ck-m-reg' x='%.1f' y='%.1f' text-anchor='%s'>%s</text>", px(x), py(y), anchor, txt)
  regions <- paste0(
    rect(xa, 1, ya, 1, "ck-r-building"), rect(xa, 1, 0, ya, "ck-r-established"),
    rect(xa, 1, -1, 0, "ck-r-fading"), rect(-1, xa, ya, 1, "ck-r-emerging"),
    lab(0.98, 0.93, "BUILDING", "end"), lab(0.98, 0.04, "ESTABLISHED / MATURE", "end"),
    lab(0.98, -0.93, "FADING", "end"), lab(-0.97, 0.93, "EMERGING"), lab(-0.97, -0.93, "NOT IN PLACE"))
  ticks <- c(-1, -0.5, 0, 0.5, 1)
  axes <- paste0(
    paste(sprintf("<line class='ck-m-grid' x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f'/>", px(ticks), px(ticks), py(-1), py(1)), collapse = ""),
    paste(sprintf("<line class='ck-m-grid' x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f'/>", px(-1), px(1), py(ticks), py(ticks)), collapse = ""),
    sprintf("<line class='ck-m-thr' x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f'/>", px(xa), px(xa), py(-1), py(1)),
    sprintf("<line class='ck-m-thr' x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f'/>", px(-1), px(1), py(ya), py(ya)),
    sprintf("<line class='ck-m-zero' x1='%.1f' x2='%.1f' y1='%.1f' y2='%.1f'/>", px(-1), px(1), py(0), py(0)),
    paste(sprintf("<text class='ck-m-tick' x='%.1f' y='%.1f' text-anchor='middle'>%+.0f%%</text>", px(ticks), H - B + 14, 100 * ticks), collapse = ""),
    paste(sprintf("<text class='ck-m-tick' x='%.1f' y='%.1f' text-anchor='end'>%+.0f%%</text>", L - 5, py(ticks) + 4, 100 * ticks), collapse = ""),
    sprintf("<text class='ck-m-ax' x='%.1f' y='%d' text-anchor='middle'>State: where markets stand vs the fingerprint &rarr;</text>", (L + W - R) / 2, H - 4),
    sprintf("<text class='ck-m-ax' transform='translate(11 %.1f) rotate(-90)' text-anchor='middle'>Move 1M &rarr;</text>", (T + H - B) / 2))
  # Draw lower-ranked scenarios first so the leaders sit on top
  pts <- vapply(rev(seq_along(matches)), function(i) {
    m <- matches[[i]]
    if (is.na(m$state) || is.na(m$score)) return("")
    n <- length(m$hist)
    tail_svg <- ""
    if (n >= 2 && length(m$hist_move) == n) {
      k <- max(1, n - 5):n
      ok <- !is.na(m$hist[k]) & !is.na(m$hist_move[k])
      if (sum(ok) >= 2) {
        tx <- px(m$hist[k][ok]); ty <- py(m$hist_move[k][ok])
        # Hollow dot on the oldest point: the trail runs from it to the marker (today)
        tail_svg <- sprintf("<polyline class='ck-m-tail' points='%s'/><circle class='ck-m-start' cx='%.1f' cy='%.1f' r='2.5'/>",
          paste(sprintf("%.1f,%.1f", tx, ty), collapse = " "), tx[1], ty[1])
      }
    }
    st <- tolower(m$age$status)
    sprintf(paste0("<g class='ck-m-pt ck-s-%s'>%s<circle cx='%.1f' cy='%.1f' r='9'/>",
                   "<text x='%.1f' y='%.1f' text-anchor='middle'>%d</text>",
                   "<title>%d. %s: %s, state %+.0f%%, move %+.0f%%</title></g>"),
            st, tail_svg, px(m$state), py(m$score), px(m$state), py(m$score) + 3.5, i,
            i, esc(m$name), m$age$status, 100 * m$state, 100 * m$score)
  }, "")
  sprintf("<svg class='ck-map' viewBox='0 0 %d %d' role='img' aria-label='Scenario map: state against one-month move'>%s%s%s</svg>",
          W, H, regions, axes, paste(pts, collapse = ""))
}

#' Ranked scenario list next to the map (numbers match the map markers)
scenario_list_html <- function(matches) {
  rows <- vapply(seq_along(matches), function(i) {
    m <- matches[[i]]
    sprintf("<tr%s><td class='ck-rank ck-s-%s'><span>%d</span></td><td title='%s'>%s</td><td>%s</td><td class='im-num'>%s</td><td class='im-num'>%+.0f%%</td><td class='im-num im-sub'>%s</td></tr>",
            if (m$age$status == "INACTIVE") " class='ck-dim'" else "", tolower(m$age$status), i, esc(m$name),
            esc(short_name(m$name)), age_badge(m$age$status),
            if (is.na(m$state)) "n/a" else sprintf("%+.0f%%", 100 * m$state), 100 * m$score,
            if (m$age$run > 0) m$age$run else "&ndash;")
  }, "")
  paste0("<table class='ck-list'><tr class='im-head'><td></td><td>Scenario</td><td>Status</td><td>State</td><td>Move 1M</td><td>Days</td></tr>",
         paste(rows, collapse = ""), "</table>")
}

# 13 hues, one per archetype (order of ARCHETYPES), for the timeline band
TIMELINE_COLORS <- c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1",
                     "#ff9da7", "#9c755f", "#86bcb6", "#d37295", "#8cd17d", "#a0cbe8")

#' 60-day band: each session coloured by the top scenario in place, under the 3-session rule of band_runs()
#' (regime_risk.R); grey cells = no scenario; runs listed newest first, gaps not listed
scenario_timeline_html <- function(matches, dates) {
  if (!length(matches) || is.null(dates)) return("")
  n <- length(dates)
  runs <- band_runs(matches, n)
  ids <- vapply(matches, `[[`, "", "id")
  arch_ids <- vapply(ARCHETYPES, `[[`, "", "id")
  col_of <- function(id) TIMELINE_COLORS[(match(id, arch_ids) - 1) %% length(TIMELINE_COLORS) + 1]
  name_of <- function(id) short_name(matches[[match(id, ids)]]$name)
  lab <- rep(NA_character_, n); pend <- rep(FALSE, n)
  for (k in seq_len(nrow(runs))) { lab[runs$a[k]:runs$b[k]] <- runs$id[k]; pend[runs$a[k]:runs$b[k]] <- runs$pending[k] }
  W <- 460; H <- 16; w <- W / n
  cells <- paste(vapply(seq_len(n), function(i) sprintf(
    "<rect x='%.2f' y='0' width='%.2f' height='%d' fill='%s'%s><title>%s: %s%s</title></rect>",
    (i - 1) * w, w + 0.3, H, if (is.na(lab[i])) "var(--border)" else col_of(lab[i]), if (pend[i]) " fill-opacity='0.45'" else "",
    short_date(dates[i]), if (is.na(lab[i])) "no scenario" else esc(name_of(lab[i])), if (pend[i]) " (pending)" else ""), ""), collapse = "")
  items <- vapply(rev(seq_len(nrow(runs))), function(k) {
    a <- runs$a[k]; b <- runs$b[k]; p <- runs$pending[k]
    sprintf("<span class='ck-run'><i style='background:%s%s'></i>%s&ndash;%s %s%s</span>",
            col_of(runs$id[k]), if (p) ";opacity:.45" else "", format(as.Date(dates[a]), "%d %b"),
            if (b == n) "today" else format(as.Date(dates[b]), "%d %b"), esc(name_of(runs$id[k])),
            if (p) sprintf(" (pending, %d of %d sessions)", b - a + 1, MIN_IN_PLACE) else "")
  }, "")
  sprintf(paste0("<div class='ck-h'>Scenario in place, last %d sessions</div>",
                 "<svg class='ck-band' viewBox='0 0 %d %d' preserveAspectRatio='none' role='img' aria-label='Scenario in place each day'>%s</svg>",
                 "<div class='ck-runs'>%s</div>"), n, W, H, cells, paste(items, collapse = ""))
}

# ── Changed since the previous report ───────────────────────────────────────

#' Previous report's rows (the last cache_date before today) from the tables main.R writes
load_prev_report <- function(today = Sys.Date()) {
  tryCatch({
    conn <- Tdata::safe_db_connect(); on.exit(DBI::dbDisconnect(conn), add = TRUE)
    d <- DBI::dbGetQuery(conn, "SELECT MAX(cache_date) AS d FROM macro_context_results WHERE cache_date < ?",
                         params = list(as.character(today)))$d
    if (!length(d) || is.na(d)) return(NULL)
    q <- function(tbl) tryCatch(DBI::dbGetQuery(conn, sprintf("SELECT * FROM %s WHERE cache_date = ?", tbl), params = list(d)),
                                error = function(e) NULL)
    list(date = as.Date(d), results = q("macro_context_results"),
         scenarios = q("macro_intermarket_scenarios"), sectors = q("macro_intermarket_sectors"))
  }, error = function(e) NULL)
}

BREADTH_ZONES <- c(20, 35, 50, 75)

#' List of change lines (HTML) between the previous report and today
report_changes <- function(prev, rg, im, vix = NULL, breadth = NULL) {
  out <- character(0)
  arrow <- function(a, b) sprintf("%s &rarr; <b>%s</b>", esc(a), esc(b))
  r <- prev$results
  if (!is.null(r) && nrow(r)) {
    r <- r[1, ]
    if (!is.null(rg) && !is.na(r$bias) && r$bias != rg$regime)   # column bias holds the Regime since 2026-10-20
      out <- c(out, paste("Regime:", arrow(r$bias, rg$regime)))
    if (!is.null(rg$risk) && "risk" %in% names(r) && !is.na(r$risk) && r$risk != rg$risk)
      out <- c(out, paste("Risk:", arrow(r$risk, rg$risk)))
    if (!is.null(vix) && !is.null(vix$vix_zone) && !is.na(r$vix_zone) && !is.na(vix$vix_zone) && r$vix_zone != vix$vix_zone)
      out <- c(out, sprintf("VIX zone: %s (VIX %.1f)", arrow(r$vix_zone, vix$vix_zone), vix$vix))
    if (!is.null(breadth) && !is.na(r$s5fi) && !is.na(breadth$pct) &&
        findInterval(r$s5fi, BREADTH_ZONES) != findInterval(breadth$pct, BREADTH_ZONES))
      out <- c(out, sprintf("Breadth crossed a zone line (20/35/50/75%%): %.0f%% &rarr; <b>%.0f%%</b>", r$s5fi, breadth$pct))
  }
  sc <- prev$scenarios
  if (!is.null(im) && !is.null(sc) && nrow(sc)) {
    key <- if ("state_score" %in% names(sc) && any(!is.na(sc$state_score))) sc$state_score else sc$score_1m
    prev_top <- sc$scenario[which.max(key)]
    names_by_id <- setNames(vapply(im$matches, function(m) short_name(m$name), ""), vapply(im$matches, `[[`, "", "id"))
    nm <- function(id) if (id %in% names(names_by_id)) names_by_id[[id]] else id
    if (length(prev_top) && prev_top != im$matches[[1]]$id)
      out <- c(out, paste("Top scenario:", arrow(nm(prev_top), nm(im$matches[[1]]$id))))
    for (m in im$matches) {
      j <- match(m$id, sc$scenario)
      if (!is.na(j) && !is.na(sc$age[j]) && sc$age[j] != m$age$status)
        out <- c(out, sprintf("%s: %s", esc(short_name(m$name)), arrow(sc$age[j], m$age$status)))
    }
  }
  sx <- prev$sectors
  if (!is.null(im) && !is.null(sx) && nrow(sx) && !is.null(im$sectors)) {
    j <- match(im$sectors$group, sx$group)
    flip <- which(!is.na(j) & sx$verdict[j] != im$sectors$verdict)
    for (i in flip) out <- c(out, sprintf("Sector %s: %s", esc(im$sectors$group[i]), arrow(sx$verdict[j[i]], im$sectors$verdict[i])))
    new <- setdiff(im$sectors$group, sx$group)
    if (length(new)) out <- c(out, paste("New sector-map groups:", esc(paste(new, collapse = ", "))))
  }
  out
}

changes_html <- function(prev, rg, im, vix = NULL, breadth = NULL) {
  if (is.null(prev)) return("<div class='ck-h'>Changed since the previous report</div><p class='im-sub'>No earlier report in the database.</p>")
  ch <- report_changes(prev, rg, im, vix, breadth)
  body <- if (length(ch)) paste0("<ul class='ck-changes'>", paste(sprintf("<li>%s</li>", ch), collapse = ""), "</ul>")
          else "<p class='im-sub'>No change in regime, risk, VIX zone, breadth zone, scenario status or sector verdicts.</p>"
  sprintf("<div class='ck-h'>Changed since %s</div>%s", short_date(prev$date), body)
}


# ── Head line: top scenario and alerts ──────────────────────────────────────

summary_head_html <- function(im) {
  m <- im$matches[[1]]
  top <- sprintf("<span class='ck-k'>Top scenario</span> <b>%s</b> %s <span class='im-sub'>%s</span>",
                 esc(short_name(m$name)), age_badge(m$age$status), score_pair(m))
  carry <- if (is.null(im$carry)) "" else {
    s <- im$carry$today$status
    cls <- switch(s, FIRING = "ck-l-red", WATCH = "ck-l-amber", "ck-l-off")
    sprintf("<span class='ck-light %s'></span><span class='ck-k'>Yen carry</span> %s", cls, s)
  }
  ch <- Filter(function(a) !is.null(a$chain), ARCHETYPES)
  chains <- if (!length(ch) || is.null(im$z1)) "" else {
    on <- vapply(ch, function(a) { cs <- chain_status(a$chain, im$z1); if (all(cs$on)) 2L else if (cs$on[1]) 1L else 0L }, 0L)
    cls <- if (any(on == 2)) "ck-l-red" else if (any(on == 1)) "ck-l-amber" else "ck-l-off"
    sprintf("<span class='ck-light %s'></span><span class='ck-k'>Chains</span> %d firing, %d triggered of %d",
            cls, sum(on == 2), sum(on == 1), length(ch))
  }
  sprintf("<div class='ck-head'><div>%s</div><div class='ck-alerts'>%s%s</div></div>", top, carry, chains)
}

# ── Assembly ────────────────────────────────────────────────────────────────

summary_html <- function(im, rg, rk, breadth = NULL, vix = NULL, prev = NULL) {
  if (is.null(im)) return("")
  by_sym <- panel_by_sym(im$sections)
  tiles <- paste(vapply(SUMMARY_TILES, tile_html, "", by_sym = by_sym, z1 = im$z1,
                        breadth = breadth, bh = im$bh, vix = vix), collapse = "")
  rg$risk <- rk$level
  paste0(
    "<section class='section ck' aria-label='Summary'><div class='stitle'>Summary &mdash; regime, key markets, what changed</div><div class='ck-body'>",
    summary_head_html(im),
    "<div class='ck-tiles'>", tiles, "</div>",
    "<div class='ck-three'><div>", scenario_map_svg(im$matches), scenario_timeline_html(im$matches, im$hist_dates),
    "<div class='proto-link'>Map numbers: ranked table in <a href='#detail' onclick=\"showTab('detail');return false;\">Detail, 00b</a>.</div></div>",
    "<div>", framework_table_html(rg, rk), changes_html(prev, rg, im, vix, breadth), "</div></div>",
    "</div></section>")
}

# ── Cross-asset heatmap (section 09) ────────────────────────────────────────

HEAT_COLS <- c("1D" = 1, "1W" = 5, "1M" = 21, "3M" = 63)
HEAT_CHG  <- c("1D" = "c1d", "1W" = "c1w", "1M" = "c1m", "3M" = "c3m")

heat_row <- function(m, meaning = NULL) {
  cells <- vapply(names(HEAT_COLS), function(h) {
    z <- heat_z(m, HEAT_COLS[[h]])
    sprintf("<td class='hz %s' title='%s %s: %s, %s usual volatility'>%s</td>", heat_class(z), esc(m$label), h,
            chg_txt(m[[HEAT_CHG[[h]]]], m$kind), if (is.na(z)) "n/a" else sprintf("%+.1f&times;", z),
            chg_txt(m[[HEAT_CHG[[h]]]], m$kind))
  }, "")
  arrow <- switch(if (is.na(m$trend)) "NA" else m$trend, UP = "<span class='im-pos'>&#9650;</span>",
                  DOWN = "<span class='im-neg'>&#9660;</span>", MIXED = "<span class='im-sub'>&#9670;</span>", "&ndash;")
  label <- esc(m$label)
  if (!is.null(m$stale_missing)) label <- paste0("<span class='im-stale'>", label, "</span>")
  sprintf("<tr><td class='hz-l'%s>%s</td><td class='im-num hz-last'>%s</td>%s<td>%s</td><td class='hz-t' title='Trend %s'>%s</td></tr>",
          if (is.null(meaning)) "" else sprintf(" title='%s'", esc(meaning)), label, fmt_num(m$last, m$kind),
          paste(cells, collapse = ""), pos_bar(m$pos52), if (is.na(m$trend)) "n/a" else m$trend, arrow)
}

heat_block <- function(title, rows) {
  if (!length(rows)) return("")
  sprintf(paste0("<div class='hz-block'><div class='im-h3'>%s</div><table class='hz-table'>",
                 "<tr class='im-head'><td></td><td>Last</td><td>1D</td><td>1W</td><td>1M</td><td>3M</td><td>52w</td><td></td></tr>%s</table></div>"),
          esc(title), paste(rows, collapse = ""))
}

heatmap_html <- function(sections) {
  if (is.null(sections) || !length(sections)) return("")
  inst <- paste(vapply(sections, function(s) heat_block(s$title, vapply(s$instruments, heat_row, "")), ""), collapse = "")
  rat <- paste(vapply(sections, function(s) heat_block(s$title,
    vapply(s$ratios, function(r) heat_row(r, r$meaning), "")), ""), collapse = "")
  paste0(
    "<div class='im-h3'>Cross-asset heatmap &mdash; which markets move together</div>",
    "<div class='hz-grid'>", inst, "</div>",
    "<details class='im-details'><summary>Relationships heatmap: ratios and spreads (hover a name for its meaning)</summary><div class='hz-grid'>", rat, "</div></details>",
    leg_note("09 &mdash; cross-asset heatmap", paste0("Cell text = the change (bp for yields and spreads). Colour = that change divided by the instrument's usual volatility ",
    "over the same horizon (standard deviation of daily changes over 120 sessions &times; &radic;days): blue up, red down; ",
    "light 0.5&ndash;1&times;, medium 1&ndash;2&times;, strong 2&times; or more; no colour below 0.5&times;. ",
    "Up is up in price: a rising VIX, yield or USD/JPY is blue. 52w = position in the 52-week range. ",
    "Arrow = trend (&#9650; UP, &#9660; DOWN, &#9670; MIXED). Full tables with sparklines: the panels below.")))
}
