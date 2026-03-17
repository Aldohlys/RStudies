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
build_sections <- function(vix, rates, breadth, spy, commodities, mismatches, synthesis, events) {
  zc <- zone_class

  # Section 1: VIX Complex
  sec1 <- paste0(
    html_row("VIX spot", vix$vix, vix$vix_zone,
      paste0(vix$vix_label, " | 1d:", fmt_chg(vix$vix, vix$vix_prev))),
    if (!is.na(vix$vvix)) html_row("VVIX", vix$vvix, vix$vvix_zone, vix$vvix_label) else "",
    html_sub(paste0("Term structure: ", vix$ts_r), "", vix$ts_zone, vix$ts_n),
    if (!is.na(vix$ratio)) html_sub("VIX/VIX3M", sprintf("%.3f", vix$ratio), vix$r_zone, vix$r_note) else "",
    if (!is.na(vix$vix_20d)) html_sub("VIX 20d ago", sprintf("%.2f", vix$vix_20d),
      ifelse(vix$vix > vix$vix_20d, "RED", "GREEN"), vix$vix_tend) else ""
  )

  # Section 2: Rates
  sec2 <- paste0(
    html_row("10Y Yield", rates$y10, rates$y10_zone,
      paste0(rates$y10_label, " | 1d:", fmt_chg(rates$y10, rates$y10_prev))),
    if (!is.na(rates$y30)) html_row("30Y Yield", rates$y30, rates$y30_zone,
      paste0("1d:", fmt_chg(rates$y30, rates$y30_prev))) else "",
    if (!is.na(rates$sp)) html_sub("2Y/10Y spread", sprintf("%+.2f%%", rates$sp), rates$sp_zone, rates$sp_label) else "",
    if (!is.na(rates$tlt_pct)) html_sub("TLT 20d", sprintf("%+.1f%%", rates$tlt_pct), rates$tlt_zone, rates$tlt_n) else "",
    if (!is.na(rates$tips_pct)) html_sub("TIP 20d", sprintf("%+.1f%%", rates$tips_pct), rates$tips_zone, rates$tips_n) else ""
  )

  # Section 3: Breadth
  S5FI_VALUE <- breadth$pct
  s5fi_zone  <- zone(S5FI_VALUE, c(20,35,50,75), c("DARKRED","RED","ORANGE","GREEN","ORANGE"))
  s5fi_label <- zone(S5FI_VALUE, c(20,35,50,75),
    c("Capitulation (<20)","Oversold (20-35) \u2014 shorts matures",
      "Bearish/transition (35-50)","Bull market (50-75)","Surachat (>75)"))

  sec3 <- paste0(
    html_row(sprintf("MA50 Breadth (%d/%d stocks)", breadth$n_above, breadth$n_valid),
             S5FI_VALUE, s5fi_zone,
             paste0(s5fi_label, sprintf(" | %.1fs", breadth$elapsed))),
    if (!is.na(spy$spy_c)) html_sub(
      sprintf("SPY $%.2f", spy$spy_c),
      sprintf("MA20:%.0f MA50:%.0f", spy$spy_m20, spy$spy_m50),
      spy$spy_z, spy$spy_n) else ""
  )

  # Section 4: Dollar + Commodities
  sec4 <- paste0(lapply(commodities$comm_rows, function(r)
    if (is.na(r$cur)) html_sub(r$tk, "NO DATA", "ORANGE", "")
    else html_row(r$tk, r$cur, r$z, r$note)))

  # Section 5: Mismatches
  mm_css <- c("FAIBLESSE INSOLITE" = "zone-darkred", "FORCE INSOLITE" = "zone-orange",
    "SHORT CONFIRME" = "zone-red", "HAUSSE FRAGILE" = "zone-orange", "RETARD vs MACRO" = "zone-green")
  priority_order <- c("FAIBLESSE INSOLITE","FORCE INSOLITE","SHORT CONFIRME","HAUSSE FRAGILE","RETARD vs MACRO")

  sec5 <- if (length(mismatches) == 0) {
    '<tr><td colspan="3" class="no-data">Aucun mismatch significatif detecte.</td></tr>'
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
      m$n_tw, ifelse(m$n_tw > 0, paste(m$tw_names, collapse = ", "), "aucun"),
      m$n_hw, ifelse(m$n_hw > 0, paste(m$hw_names, collapse = ", "), "aucun"),
      m$note, m$signal)))
  }

  # Section 6: Synthesis
  sec6 <- paste0(lapply(synthesis$synth_notes, function(n)
    sprintf('<li class="%s">%s</li>', zc(n$z), n$t)))

  # Section 7: Events
  ev_row_css <- c(CRITIQUE = "zone-darkred", ELEVE = "zone-red", MODERE = "zone-orange")
  ev_badge_css <- c(CRITIQUE = "ev-critique", ELEVE = "ev-eleve", MODERE = "ev-modere")
  sec7 <- paste0(lapply(events, function(ev) sprintf(
    '<tr class="event-row %s">
      <td class="ev-date">%s</td>
      <td class="ev-event">%s</td>
      <td><span class="ev-badge %s">%s</span></td>
      <td class="ev-action">%s</td></tr>',
    ev_row_css[ev$impact], ev$date, ev$event, ev_badge_css[ev$impact], ev$impact, ev$action)))

  list(sec1 = sec1, sec2 = sec2, sec3 = sec3,
       sec4 = paste0(sec4, collapse = ""),
       sec5 = paste0(sec5, collapse = ""),
       sec6 = paste0(sec6, collapse = ""),
       sec7 = paste0(sec7, collapse = ""))
}

# ── Render final HTML ───────────────────────────────────────────────────────
render_macro_html <- function(sections, synthesis, breadth, out_dir) {
  template_file <- file.path(SCRIPT_DIR, "template.html")
  template <- paste(readLines(template_file, encoding = "UTF-8"), collapse = "\n")

  replacements <- list(
    "{{RUN_DATE}}"    = format(Sys.Date(), "%d %B %Y"),
    "{{BIAS_CSS}}"    = badge_class(synthesis$bias_zone),
    "{{BIAS}}"        = synthesis$bias,
    "{{SL}}"          = as.character(synthesis$sl),
    "{{SS}}"          = as.character(synthesis$ss),
    "{{SEC1}}"        = sections$sec1,
    "{{SEC2}}"        = sections$sec2,
    "{{SEC3}}"        = sections$sec3,
    "{{SEC4}}"        = sections$sec4,
    "{{SEC5}}"        = sections$sec5,
    "{{SEC6}}"        = sections$sec6,
    "{{SEC7}}"        = sections$sec7,
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
