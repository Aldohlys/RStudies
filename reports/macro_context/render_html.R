# render_html.R — Build HTML sections and render template for macro context report

source(file.path(SCRIPT_DIR, "..", "shared", "html_helpers.R"))

# ── Build all sections ──────────────────────────────────────────────────────
# Sections 05 (mismatches), 06 (synthesis) and 07 (regime model) were dropped on 2026-10-20
build_sections <- function(vix, rates, breadth, spy, commodities, events, scenario_scores = NULL) {
  zc <- zone_class

  # Section 1: VIX Complex
  sec1 <- paste0(
    html_row_tip("VIX spot", vix$vix, vix$vix_zone,
      paste0(vix$vix_label, " | 1d:", fmt_chg(vix$vix, vix$vix_prev)),
      "Source: Yahoo ^VIX | Spot value | BLUE <20, BLACK 20-25, RED 25-30, DARKRED >30"),
    if (!is.na(vix$vvix)) html_row_tip("VVIX", vix$vvix, vix$vvix_zone, vix$vvix_label,
      "Source: Yahoo ^VVIX | Vol-of-vol | BLUE <80, BLACK 80-100, RED 100-120, DARKRED >120") else "",
    html_sub_tip(paste0("Term structure: ", vix$ts_r), "", vix$ts_zone, vix$ts_n,
      "VIX9D vs VIX vs VIX3M | Contango = normal, Backwardation = fear"),
    if (!is.na(vix$ratio)) html_sub_tip("VIX/VIX3M", sprintf("%.3f", vix$ratio), vix$r_zone, vix$r_note,
      "VIX / VIX3M ratio | BLUE <0.90 contango, BLACK 0.90-1.00, RED >=1.00 backwardation | 10y median 0.88, p95 1.02") else "",
    if (!is.na(vix$vx_spread)) html_sub_tip("VX1-VX2", sprintf("%+.2f", vix$vx_spread), vix$vx_zone,
      paste0(vix$vx_label, " | 1d:", fmt_chg(vix$vx_spread, vix$vx_prev), " | ", vix$vx_detail),
      "Source: CBOE daily settlement, front minus second monthly VIX future | BLUE < -0.5 contango, BLACK -0.5 to 0, RED 0 to +0.5, DARKRED > +0.5 backwardation")
    else html_sub_tip("VX1-VX2", "NO DATA", "ORANGE", "CBOE settlement unavailable",
      "Source: CBOE daily settlement, front minus second monthly VIX future"),
    if (!is.na(vix$vix_20d)) html_sub_tip("VIX 20d ago", sprintf("%.2f", vix$vix_20d),
      ifelse(vix$vix > vix$vix_20d, "RED", "GREEN"), vix$vix_tend,
      "Source: Yahoo ^VIX | 20-day lookback for trend | Rising = bearish") else ""
  )

  # Section 2: Rates
  sec2 <- paste0(
    html_row_tip("10Y Yield", rates$y10, rates$y10_zone,
      paste0(rates$y10_label, " | 1d:", fmt_chg(rates$y10, rates$y10_prev)),
      "Source: Yahoo ^TNX | Spot yield | BLUE <4.5%, BLACK 4.5-5%, RED >5%"),
    if (!is.na(rates$y30)) html_row_tip("30Y Yield", rates$y30, rates$y30_zone,
      paste0("1d:", fmt_chg(rates$y30, rates$y30_prev)),
      "Source: Yahoo ^TYX | 30-year yield | Same thresholds as 10Y") else "",
    if (!is.na(rates$sp)) html_sub_tip("3M/10Y spread", sprintf("%+.2f%%", rates$sp), rates$sp_zone, paste0(rates$sp_label, " | ", rates$sp_shape),
      "10Y (^TNX) minus 13-week bill (^IRX) | Level: negative = inversion | Colour from the 20d move when the spread moved >= 5bp: RED bear steepening (long end selling off), BLUE bull steepening, BLACK flattening") else "",
    if (!is.na(rates$tlt_pct)) html_sub_tip("TLT 20d", sprintf("%+.1f%%", rates$tlt_pct), rates$tlt_zone, rates$tlt_n,
      "Source: Yahoo TLT | 20-day return | BLUE >+1%, RED <-1%") else "",
    if (!isTRUE(rates$fred_ok) && !is.na(rates$tips_pct)) html_sub_tip("TIP 20d", sprintf("%+.1f%%", rates$tips_pct), rates$tips_zone, rates$tips_n,
      "Source: Yahoo TIP | 20-day return | Real yield proxy (shown only when FRED is unavailable) | BLUE >+0.5%, RED <-0.5%") else "",
    rates_fred_rows(rates)
  )

  # Section 3: Breadth
  S5FI_VALUE <- breadth$pct
  s5fi_zone  <- zone(S5FI_VALUE, c(20,35,50,75), c("DARKRED","RED","ORANGE","GREEN","ORANGE"))
  s5fi_label <- zone(S5FI_VALUE, c(20,35,50,75),
    c("Capitulation (<20)","Oversold (20-35) — short positions mature, reversal watch",
      "Bearish/transition (35-50)","Bull market (50-75)","Overbought (>75)"))

  sec3 <- paste0(
    html_row_tip(sprintf("MA50 Breadth (%d/%d stocks)", breadth$n_above, breadth$n_valid),
             S5FI_VALUE, s5fi_zone,
             paste0(s5fi_label, sprintf(" | %.1fs", breadth$elapsed)),
             "Computed: % S&P500 stocks > MA50 | Wikipedia constituents + Yahoo data | BLUE >50%, RED <35%, DARKRED <20%"),
    if (!is.na(spy$spy_c)) html_sub_tip(
      sprintf("SPY $%.2f", spy$spy_c),
      sprintf("MA20:%.0f MA50:%.0f", spy$spy_m20, spy$spy_m50),
      spy$spy_z, spy$spy_n,
      "Source: Yahoo SPY | Position vs MA20/MA50 | BLUE = above both, RED = below both") else ""
  )

  # Section 4: Dollar + Commodities
  comm_tips <- c(
    "DX-Y.NYB" = "Source: Yahoo DX-Y.NYB | US Dollar Index 20d return | BLUE <+1%, RED >3%",
    "USO"      = "Source: Yahoo USO | Oil ETF | MA20 position + 20d return",
    "GLD"      = "Source: Yahoo GLD | Gold ETF | MA20 position + 20d return",
    "XLE"      = "Source: Yahoo XLE | Energy sector ETF | MA20 position + 20d return"
  )
  sec4 <- paste0(lapply(commodities$comm_rows, function(r)
    if (is.na(r$cur)) html_sub_tip(r$tk, "NO DATA", "ORANGE", "", comm_tips[r$tk])
    else html_row_tip(r$tk, r$cur, r$z, r$note, comm_tips[r$tk])))

  # Section 7: Events
  ev_row_css <- c(CRITICAL = "zone-darkred", HIGH = "zone-red", MODERATE = "zone-orange")
  ev_badge_css <- c(CRITICAL = "ev-critical", HIGH = "ev-high", MODERATE = "ev-moderate")
  sec7 <- paste0(lapply(events, function(ev) sprintf(
    '<tr class="event-row %s">
      <td class="ev-date">%s</td>
      <td class="ev-event">%s</td>
      <td><span class="ev-badge %s">%s</span></td>
      <td class="ev-action">%s</td></tr>',
    ev_row_css[ev$impact], ev$date, ev$event, ev_badge_css[ev$impact], ev$impact, ev$action)))

  # Positioning (COT age) from the regime model's attributes: still used by the stale-data banner
  positioning <- if (!is.null(scenario_scores)) attr(scenario_scores, "positioning") else NULL

  # Stress banner: simple VIX > 25 warning (backtested — regime signals don't predict BOT outcomes,
  # but VIX > 25 + credit stress marks the rare liquidity events worth avoiding)
  stress_banner <- ""
  if (!is.na(vix$vix) && vix$vix >= 25) {
    stress_banner <- sprintf(
      '<div class="stress-banner">MARKET STRESS &mdash; VIX at %.1f<span class="stress-sub">Backtest shows BOT entries during liquidity stress carry elevated risk. Consider waiting for VIX to settle below 25.</span></div>',
      vix$vix)
  }

  # Data-hygiene banner: COT positioning file has missed at least one weekly
  # release, so crowding_score and the 'net' direction labels are on old data.
  # Not market stress - deliberately a separate, calmer banner.
  data_banner <- ""
  if (!is.null(positioning) && isTRUE(positioning$cot_stale)) {
    n <- positioning$cot_missed_releases
    data_banner <- sprintf(
      '<div class="stale-banner">STALE POSITIONING DATA &mdash; COT is as of %s, %d days old (%d weekly release%s missed)<span class="stale-sub">Crowding score %.2f and the net-direction labels below are computed from that file. Refresh reports/macro_context/positioning.R before trusting the positioning read.</span></div>',
      positioning$cot_as_of, round(positioning$cot_age_days), n,
      ifelse(n == 1, "", "s"), positioning$crowding_score)
  }

  list(sec1 = sec1, sec2 = sec2, sec3 = sec3,
       sec4 = paste0(sec4, collapse = ""),
       sec7 = paste0(sec7, collapse = ""),
       stress_banner = stress_banner,
       data_banner = data_banner)
}

# ── Rates rows from FRED: decomposition, 2Y, policy pricing ────────────────
rates_fred_rows <- function(rates) {
  if (!isTRUE(rates$fred_ok)) return(html_sub_tip("FRED rates", "NO DATA", "ORANGE", "FRED unavailable: no decomposition today", ""))
  chg <- function(x) sprintf("20d %s | YTD %s", if (is.na(x$c20)) "n/a" else sprintf("%+.2f", x$c20),
                             if (is.na(x$ytd)) "n/a" else sprintf("%+.2f", x$ytd))
  asof <- function(x) if (!is.na(x$d) && x$d < Sys.Date() - 3) sprintf(" | as of %s", format(x$d, "%d %b")) else ""
  split_txt <- function(s, lbl) if (is.null(s) || any(is.na(s))) "" else
    sprintf("%s: 10Y %+.2f = breakeven %+.2f + term premium %+.2f + real path %+.2f",
            lbl, s[["nominal"]], s[["breakeven"]], s[["term_premium"]], s[["real_path"]])
  r <- rates
  paste0(
    html_sub_tip("10Y real yield", sprintf("%.2f%%", r$real$v), r$real$zone, paste0(chg(r$real), asof(r$real)),
      "Source: FRED DFII10 (10-year TIPS yield) | BLUE <1.5%, BLACK 1.5-2.5%, RED >=2.5% | Rising real yields = headwind for gold, long-duration equities"),
    html_sub_tip("10Y breakeven", sprintf("%.2f%%", r$be$v), r$be$zone, paste0(chg(r$be), asof(r$be)),
      "Source: FRED T10YIE (10Y nominal minus 10Y TIPS) = inflation expected by the bond market | BLACK <1.8% (deflation scare), BLUE 1.8-2.6%, RED >=2.6% (inflation scare)"),
    html_sub_tip("10Y term premium", sprintf("%.2f%%", r$tp$v), r$tp$zone, paste0(chg(r$tp), asof(r$tp)),
      "Source: FRED THREEFYTP10 (Kim-Wright model, about one week lag) = extra yield demanded to hold 10 years instead of rolling bills | BLUE <0.5%, BLACK 0.5-1.0%, RED >=1.0%"),
    if (!is.na(r$driver20)) html_sub_tip("10Y move driver", r$driver20, r$split_zone,
      paste0(split_txt(r$split20, "20d"), " | ", split_txt(r$split_ytd, "YTD"),
             if (!is.na(r$driver_ytd)) paste0(" (YTD driver: ", r$driver_ytd, ")") else "",
             sprintf(" | as of %s", format(r$split_date, "%d %b"))),
      "Splits the 10Y change into breakeven + term premium + real path (real yield minus term premium); the three add up to the 10Y change. Driver = the part contributing most in the direction of the move. RED = rising 10Y led by term premium (bond-rout tell), BLACK = rising otherwise or little change, BLUE = 10Y falling") else "",
    html_sub_tip("2Y yield", sprintf("%.2f%%", r$y2$v), "ORANGE", paste0(chg(r$y2), asof(r$y2)), "Source: FRED DGS2"),
    html_sub_tip("2Y/10Y spread", sprintf("%+.2f%%", r$s210$v), r$s210$zone, paste0(r$s210$label, " | ", r$s210$shape),
      "FRED DGS10 minus DGS2 | Level: negative = inversion | Colour from the 20d move when the spread moved >= 5bp: RED bear steepening, BLUE bull steepening, BLACK flattening"),
    html_sub_tip("Policy priced (2Y - fed funds)", sprintf("%+.2f%%", r$pol$v), r$pol$zone, paste0(r$pol$label, " | ", chg(r$pol)),
      "FRED DGS2 minus DFF (effective fed funds) = rough measure of the policy change priced over about a year | BLUE < -0.25 cuts priced, BLACK on hold, RED > +0.25 hikes priced | YTD change = repricing of the Fed path since January")
  )
}

# ── Render final HTML ───────────────────────────────────────────────────────

#' Header stamp: when the report ran and the last US session its data covers (S&P 500 last bar).
#' A same-day re-run reuses the morning cache, so the run time alone does not say how fresh the data is.
run_stamp <- function(im) {
  now <- as.POSIXlt(Sys.time())
  run <- sprintf("Run %d %s %d %02d:%02d", now$mday, month.abb[now$mon + 1], now$year + 1900, now$hour, now$min)
  spx <- if (is.null(im)) NULL else Filter(function(m) identical(m$sym, "^GSPC"), unlist(lapply(im$sections, `[[`, "instruments"), recursive = FALSE))
  if (!length(spx) || is.null(spx[[1]]$date)) return(run)
  sprintf("%s &middot; data to %s close", run, short_date(spx[[1]]$date))
}
render_macro_html <- function(sections, rg, rk, breadth, out_dir, im = NULL, cot = NULL, vix = NULL, prev = NULL) {
  template_file <- file.path(SCRIPT_DIR, "template.html")
  template <- paste(readLines(template_file, encoding = "UTF-8"), collapse = "
")
  leg_reset()
  # Rendered in page order, so the Legend's table notes (L2) follow the page; the Legend itself last
  main_00 <- if (is.null(im)) "<div class='no-data'>Intermarket data unavailable</div>" else
    paste0(stale_html(im$sections, im$stale), movie_main_html(im$movie, im$matches, im$sectors, im$sections))
  summary <- summary_html(im, rg, rk, breadth, vix, prev)
  sector_map <- if (is.null(im)) "" else sectors_html(im$sectors)
  heatmap <- if (is.null(im)) "" else paste0(ytd_band_html(im$ytd), heatmap_html(im$sections))
  panels <- if (is.null(im)) "" else panels_html(im$sections)
  cot_html <- cot_dashboard_html(cot)
  detail_00 <- if (is.null(im)) "" else movie_detail_html(im$movie, im$matches, im$sectors, im$z1, im$carry, im$st, im$st5, im$st5_date)

  replacements <- list(
    "{{RUN_DATE}}"    = run_stamp(im),
    "{{REGIME_BAR}}"  = regime_bar_html(rg, rk),
    "{{SUMMARY}}"     = summary,
    "{{MAIN_00}}"     = main_00,
    "{{SECTOR_MAP}}"  = sector_map,
    "{{HEATMAP}}"     = heatmap,
    "{{PANELS}}"      = panels,
    "{{COT_DASHBOARD}}" = cot_html,
    "{{DETAIL_00}}"   = detail_00,
    "{{SEC1}}"        = sections$sec1,
    "{{SEC2}}"        = sections$sec2,
    "{{SEC3}}"        = sections$sec3,
    "{{SEC4}}"        = sections$sec4,
    "{{SEC7}}"        = sections$sec7,
    "{{STRESS_BANNER}}" = sections$stress_banner,
    "{{DATA_BANNER}}"   = if (is.null(sections$data_banner)) "" else sections$data_banner
  )
  replacements[["{{LEGEND}}"]] <- legend_html()

  html <- render_template(template, replacements)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- file.path(out_dir, sprintf("macro_context_%s.html", format(Sys.Date(), "%Y%m%d")))
  writeLines(html, out_file)
  message(sprintf("HTML written: %s", out_file))
  out_file
}
