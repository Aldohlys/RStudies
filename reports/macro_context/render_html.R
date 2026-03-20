# render_html.R — Build HTML sections and render template for macro context report

source(file.path(SCRIPT_DIR, "..", "shared", "html_helpers.R"))

# ── HTML row builders ───────────────────────────────────────────────────────
html_row <- function(label, value, z, note) {
  vs <- if (is.na(value)) "n/a" else sprintf("%.2f", value)
  sprintf('<tr class="data-row %s"><td class="label">%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    zone_class(z), label, vs, note)
}

html_sub <- function(label, val_str, z, note) {
  sprintf('<tr class="sub-row %s"><td class="label indent">%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    zone_class(z), label, val_str, note)
}

# ── Build all sections ──────────────────────────────────────────────────────
build_sections <- function(vix, rates, breadth, spy, commodities, mismatches, synthesis, events, scenario_scores = NULL) {
  zc <- zone_class

  # Section 1: VIX Complex
  sec1 <- paste0(
    html_row_tip("VIX spot", vix$vix, vix$vix_zone,
      paste0(vix$vix_label, " | 1d:", fmt_chg(vix$vix, vix$vix_prev)),
      "Source: Yahoo ^VIX | Spot value | GREEN <20, ORANGE 20-25, RED 25-30, DARKRED >30"),
    if (!is.na(vix$vvix)) html_row_tip("VVIX", vix$vvix, vix$vvix_zone, vix$vvix_label,
      "Source: Yahoo ^VVIX | Vol-of-vol | GREEN <80, ORANGE 80-100, RED 100-120, DARKRED >120") else "",
    html_sub_tip(paste0("Term structure: ", vix$ts_r), "", vix$ts_zone, vix$ts_n,
      "VIX9D vs VIX vs VIX3M | Contango = normal, Backwardation = fear"),
    if (!is.na(vix$ratio)) html_sub_tip("VIX/VIX3M", sprintf("%.3f", vix$ratio), vix$r_zone, vix$r_note,
      "VIX / VIX3M ratio | <0.85 = backwardation, >1.10 = healthy contango") else "",
    if (!is.na(vix$vix_20d)) html_sub_tip("VIX 20d ago", sprintf("%.2f", vix$vix_20d),
      ifelse(vix$vix > vix$vix_20d, "RED", "GREEN"), vix$vix_tend,
      "Source: Yahoo ^VIX | 20-day lookback for trend | Rising = bearish") else ""
  )

  # Section 2: Rates
  sec2 <- paste0(
    html_row_tip("10Y Yield", rates$y10, rates$y10_zone,
      paste0(rates$y10_label, " | 1d:", fmt_chg(rates$y10, rates$y10_prev)),
      "Source: Yahoo ^TNX | Spot yield | GREEN <4.5%, ORANGE 4.5-5%, RED >5%"),
    if (!is.na(rates$y30)) html_row_tip("30Y Yield", rates$y30, rates$y30_zone,
      paste0("1d:", fmt_chg(rates$y30, rates$y30_prev)),
      "Source: Yahoo ^TYX | 30-year yield | Same thresholds as 10Y") else "",
    if (!is.na(rates$sp)) html_sub_tip("2Y/10Y spread", sprintf("%+.2f%%", rates$sp), rates$sp_zone, rates$sp_label,
      "10Y (^TNX) minus 2Y (^IRX) | Negative = inversion, >0.25 = normal") else "",
    if (!is.na(rates$tlt_pct)) html_sub_tip("TLT 20d", sprintf("%+.1f%%", rates$tlt_pct), rates$tlt_zone, rates$tlt_n,
      "Source: Yahoo TLT | 20-day return | GREEN >+1%, RED <-1%") else "",
    if (!is.na(rates$tips_pct)) html_sub_tip("TIP 20d", sprintf("%+.1f%%", rates$tips_pct), rates$tips_zone, rates$tips_n,
      "Source: Yahoo TIP | 20-day return | Real yield proxy | GREEN >+0.5%, RED <-0.5%") else ""
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
             "Computed: % S&P500 stocks > MA50 | Wikipedia constituents + Yahoo data | GREEN >50%, RED <35%, DARKRED <20%"),
    if (!is.na(spy$spy_c)) html_sub_tip(
      sprintf("SPY $%.2f", spy$spy_c),
      sprintf("MA20:%.0f MA50:%.0f", spy$spy_m20, spy$spy_m50),
      spy$spy_z, spy$spy_n,
      "Source: Yahoo SPY | Position vs MA20/MA50 | GREEN = above both, RED = below both") else ""
  )

  # Section 4: Dollar + Commodities
  comm_tips <- c(
    "DX-Y.NYB" = "Source: Yahoo DX-Y.NYB | US Dollar Index 20d return | GREEN <+1%, RED >3%",
    "USO"      = "Source: Yahoo USO | Oil ETF | MA20 position + 20d return",
    "GLD"      = "Source: Yahoo GLD | Gold ETF | MA20 position + 20d return",
    "XLE"      = "Source: Yahoo XLE | Energy sector ETF | MA20 position + 20d return"
  )
  sec4 <- paste0(lapply(commodities$comm_rows, function(r)
    if (is.na(r$cur)) html_sub_tip(r$tk, "NO DATA", "ORANGE", "", comm_tips[r$tk])
    else html_row_tip(r$tk, r$cur, r$z, r$note, comm_tips[r$tk])))

  # Section 5: Mismatches
  mm_css <- c("UNUSUAL WEAKNESS" = "zone-darkred", "UNUSUAL STRENGTH" = "zone-orange",
    "CONFIRMED SHORT" = "zone-red", "FRAGILE RALLY" = "zone-orange", "LAGGING vs MACRO" = "zone-green")
  priority_order <- c("UNUSUAL WEAKNESS","UNUSUAL STRENGTH","CONFIRMED SHORT","FRAGILE RALLY","LAGGING vs MACRO")

  sec5 <- if (length(mismatches) == 0) {
    '<tr><td colspan="3" class="no-data">No significant mismatches detected.</td></tr>'
  } else {
    sorted_mm <- mismatches[order(match(sapply(mismatches, function(x) x$type), priority_order))]
    paste0(lapply(sorted_mm, function(m) sprintf(
      '<tr class="mm-header %s">
        <td colspan="3"><span class="mm-type">%s</span>
          <span class="mm-etf">%s \u2014 %s</span>
          <span class="mm-meta">Trend:%s | 20d:%+.1f%% | RS:%+.1f%%</span></td></tr>
      <tr class="mm-detail"><td colspan="3">
          <span class="mm-winds">TW(%d):%s &nbsp;|&nbsp; HW(%d):%s</span><br>
          <span class="mm-note">%s</span><br>
          <span class="mm-signal">&#10230; %s</span></td></tr>',
      mm_css[m$type], m$type, m$sector, m$etf,
      m$trend, m$r20, ifelse(is.na(m$rs), 0, m$rs),
      m$n_tw, ifelse(m$n_tw > 0, paste(m$tw_names, collapse = ", "), "none"),
      m$n_hw, ifelse(m$n_hw > 0, paste(m$hw_names, collapse = ", "), "none"),
      m$note, m$signal)))
  }

  # Section 6: Synthesis
  sec6 <- paste0(lapply(synthesis$synth_notes, function(n)
    sprintf('<li class="%s">%s</li>', zc(n$z), n$t)))

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

  # Section 8: Regime Detection
  sec8 <- ""
  if (!is.null(scenario_scores) && nrow(scenario_scores) > 0) {
    sorted <- scenario_scores[order(-scenario_scores$probability), ]
    signals <- attr(scenario_scores, "signals")
    positioning <- attr(scenario_scores, "positioning")
    sig_tips <- attr(scenario_scores, "signal_tooltips")

    # Regime cards
    regime_cards <- paste0(vapply(seq_len(nrow(sorted)), function(i) {
      s <- sorted[i, ]
      is_dom <- isTRUE(s$is_dominant)
      prob_css <- if (s$probability > 40) "sc-high" else if (s$probability > 30) "sc-mid" else "sc-low"
      delta_str <- if (!is.na(s$delta)) sprintf(" (%+.1f%%)", s$delta) else ""
      dom_mark <- if (is_dom) '<span class="sc-dominant">DOMINANT</span> ' else ""
      catalyst_mark <- if (s$catalyst_boost > 0) sprintf(' <span class="sc-catalyst" title="Event catalyst boost: +%.2f">&#9889;</span>', s$catalyst_boost) else ""
      tooltip <- REGIME_WEIGHTS[[s$regime]]$tooltip
      sprintf(
        '<div class="sc-card %s" title="%s">
          <div class="sc-header">%s<span class="sc-name">%s</span>%s
            <span class="sc-prob %s">%.1f%%%s</span></div>
          <div class="sc-details">Raw: %.3f | %s</div>
        </div>',
        ifelse(is_dom, "sc-active", ""), tooltip, dom_mark, s$regime_label, catalyst_mark,
        prob_css, s$probability, delta_str,
        s$raw_score, s$details
      )
    }, character(1)), collapse = "\n")

    # Signal gauges
    signal_html <- ""
    if (!is.null(signals)) {
      sig_bars <- paste0(vapply(names(signals), function(sn) {
        v <- signals[[sn]]
        if (is.null(v) || is.na(v)) v <- 0
        pct <- round(v * 100)
        tip <- if (!is.null(sig_tips[[sn]])) sig_tips[[sn]] else sn
        bar_css <- if (v > 0.65) "sig-bar-high" else if (v > 0.35) "sig-bar-mid" else "sig-bar-low"
        sprintf('<div class="sig-gauge" title="%s"><span class="sig-label">%s</span><div class="sig-track"><div class="sig-fill %s" style="width:%d%%"></div></div><span class="sig-val">%.2f</span></div>',
          tip, sn, bar_css, pct, v)
      }, character(1)), collapse = "\n")
      signal_html <- sprintf('<div class="sig-grid">%s</div>', sig_bars)
    }

    # Positioning stress
    pos_html <- ""
    if (!is.null(positioning)) {
      pos_css <- if (positioning$crowding_score > 0.5) "sc-high" else if (positioning$crowding_score > 0.3) "sc-mid" else "sc-low"
      cot_str <- if (length(positioning$cot_extremes) > 0) paste("COT extremes:", paste(positioning$cot_extremes, collapse = ", ")) else "No COT extremes"
      pos_html <- sprintf('<div class="pos-bar"><span class="pos-label" title="Positioning stress from sector RS extremes + COT data. >0.3 = fragile, boosts liquidation probability.">Positioning stress:</span> <span class="sc-prob %s">%.2f</span> <span class="pos-detail">%s</span></div>',
        pos_css, positioning$crowding_score, cot_str)
    }

    sec8 <- paste0(
      '<div class="sc-container">', regime_cards, '</div>',
      signal_html,
      pos_html
    )
  } else {
    sec8 <- '<div class="sc-card"><div class="sc-header">No regime data</div></div>'
  }

  # Stress banner: simple VIX > 25 warning (backtested — regime signals don't predict BOT outcomes,
  # but VIX > 25 + credit stress marks the rare liquidity events worth avoiding)
  stress_banner <- ""
  if (!is.na(vix$vix) && vix$vix >= 25) {
    stress_banner <- sprintf(
      '<div class="stress-banner">MARKET STRESS &mdash; VIX at %.1f<span class="stress-sub">Backtest shows BOT entries during liquidity stress carry elevated risk. Consider waiting for VIX to settle below 25.</span></div>',
      vix$vix)
  }

  list(sec1 = sec1, sec2 = sec2, sec3 = sec3,
       sec4 = paste0(sec4, collapse = ""),
       sec5 = paste0(sec5, collapse = ""),
       sec6 = paste0(sec6, collapse = ""),
       sec7 = paste0(sec7, collapse = ""),
       sec8 = sec8,
       stress_banner = stress_banner)
}

# ── Render final HTML ───────────────────────────────────────────────────────
render_macro_html <- function(sections, synthesis, breadth, out_dir) {
  template_file <- file.path(SCRIPT_DIR, "template.html")
  template <- paste(readLines(template_file, encoding = "UTF-8"), collapse = "\n")

  replacements <- list(
    "{{RUN_DATE}}"    = format(Sys.time(), "%d %B %Y — %H:%M"),
    "{{BIAS_CSS}}"    = badge_class(synthesis$bias_zone),
    "{{BIAS}}"        = synthesis$bias,
    "{{SL}}"          = as.character(synthesis$sl),
    "{{SS}}"          = as.character(synthesis$ss),
    "{{BIAS_EXPLAIN}}" = synthesis$bias_explain,
    "{{SEC1}}"        = sections$sec1,
    "{{SEC2}}"        = sections$sec2,
    "{{SEC3}}"        = sections$sec3,
    "{{SEC4}}"        = sections$sec4,
    "{{SEC5}}"        = sections$sec5,
    "{{SEC6}}"        = sections$sec6,
    "{{SEC7}}"        = sections$sec7,
    "{{SEC8}}"        = sections$sec8,
    "{{STRESS_BANNER}}" = sections$stress_banner,
    "{{FOOTER_BREADTH}}" = sprintf("Breadth: %.1f%% (%d/%d stocks &gt; MA50) &mdash; computed in %.1fs",
                                    breadth$pct, breadth$n_above, breadth$n_valid, breadth$elapsed)
  )

  html <- render_template(template, replacements)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_file <- file.path(out_dir, sprintf("macro_context_%s.html", format(Sys.Date(), "%Y%m%d")))
  writeLines(html, out_file)
  message(sprintf("HTML written: %s", out_file))
  out_file
}
