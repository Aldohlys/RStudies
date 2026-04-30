# reports/analyze/report.R — neutral HTML output for /analyze.
#
# Emits PASS / SKIP / NO SIGNAL phase rows, vol-funnel grid with mechanical
# labels, structures table with $300/lot cap flag. NO verdicts, conviction,
# or "Best/Alternative/Avoid" framing.

.css <- '
:root{
  --pass-bg:#0072B2; --pass-light:rgba(0,114,178,.12);
  --fail-bg:#D55E00; --fail-light:rgba(213,94,0,.10);
  --warn-bg:#E69F00; --warn-light:rgba(230,159,0,.10);
  --skip-bg:#666; --skip-light:rgba(102,102,102,.10);
  --ink:#222; --rule:#ddd; --bg:#fafafa;
}
body{font-family:-apple-system,Segoe UI,Roboto,sans-serif;color:var(--ink);background:var(--bg);max-width:1100px;margin:24px auto;padding:0 20px;line-height:1.45;font-size:14px}
h1{font-size:22px;margin:0 0 4px}
h2{font-size:16px;margin:24px 0 8px;padding-bottom:4px;border-bottom:1px solid var(--rule)}
h3{font-size:14px;margin:14px 0 6px;color:#555;text-transform:uppercase;letter-spacing:.04em}
.sub{color:#666;font-size:12px;margin-bottom:14px}
.badges{display:flex;gap:6px;flex-wrap:wrap;margin:8px 0 14px}
.badge{display:inline-block;color:#fff;padding:3px 10px;border-radius:3px;font-size:12px;font-weight:600}
.badge-pass{background:var(--pass-bg)}
.badge-fail{background:var(--fail-bg)}
.badge-warn{background:var(--warn-bg)}
.badge-skip{background:var(--skip-bg)}
table{width:100%;border-collapse:collapse;margin:6px 0 14px;background:#fff}
th,td{padding:6px 10px;text-align:left;border-bottom:1px solid var(--rule);vertical-align:top}
th{background:#f3f3f3;font-size:12px;text-transform:uppercase;letter-spacing:.03em;color:#444}
tr.row-pass{background:var(--pass-light)}
tr.row-fail{background:var(--fail-light)}
tr.row-warn{background:var(--warn-light)}
tr.row-skip{background:var(--skip-light)}
td.value{font-variant-numeric:tabular-nums;font-weight:600}
td.note{color:#555;font-size:13px}
.meta-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin:10px 0}
.meta-grid .cell{background:#fff;padding:8px 12px;border:1px solid var(--rule);border-radius:4px}
.meta-grid .lbl{font-size:11px;color:#666;text-transform:uppercase;letter-spacing:.04em}
.meta-grid .val{font-size:15px;font-weight:600;font-variant-numeric:tabular-nums}
'

.row_class <- function(result) {
  switch(result %||% "STALE",
         PASS = "row-pass",
         SKIP = "row-fail",
         `NO SIGNAL` = "row-skip",
         STALE = "row-warn",
         "row-warn")
}

.badge_class <- function(result) {
  switch(result %||% "STALE",
         PASS = "badge-pass",
         SKIP = "badge-fail",
         `NO SIGNAL` = "badge-skip",
         STALE = "badge-warn",
         "badge-skip")
}

.fmt_num <- function(x, digits = 2) {
  if (is.null(x) || all(is.na(x))) return("n/a")
  if (is.numeric(x)) sprintf(paste0("%.", digits, "f"), x) else as.character(x)
}

# ── Main render entry ────────────────────────────────────────────────────
render_analyze_html <- function(ctx, out_dir) {
  ticker <- ctx$ticker; direction <- ctx$direction; date <- ctx$date
  pa <- ctx$phase_a; pb <- ctx$phase_b; pc <- ctx$phase_c
  pd <- ctx$phase_d; pe <- ctx$phase_e

  # Header strip
  meta <- sprintf(paste0(
    '<div class="meta-grid">',
    '<div class="cell"><div class="lbl">Spot</div><div class="val">$%s</div></div>',
    '<div class="cell"><div class="lbl">Sector</div><div class="val">%s</div></div>',
    '<div class="cell"><div class="lbl">v5 classification</div><div class="val">%s</div></div>',
    '<div class="cell"><div class="lbl">phase_of_drop</div><div class="val">%s</div></div>',
    '</div>'),
    .fmt_num(pb$price), pb$sector %||% "n/a",
    pe$v5_classification, pe$phase_of_drop)

  badges <- sprintf(paste0(
    '<div class="badges">',
    '<span class="badge %s">A · %s</span>',
    '<span class="badge %s">B · %s</span>',
    '<span class="badge %s">C · %s</span>',
    '<span class="badge %s">D · %s</span>',
    '<span class="badge %s">E · %s</span>',
    '</div>'),
    .badge_class(pa$result), pa$result,
    .badge_class(pb$result), pb$result,
    .badge_class(pc$result), pc$result,
    .badge_class(pd$result), pd$result,
    .badge_class(pe$v5_classification), pe$v5_classification)

  # Phase A
  sec_a <- sprintf(paste0(
    '<h2>Phase A — Universe Rich-Options Gate</h2>',
    '<table><tr><th>Result</th><th>Source</th><th>Note</th></tr>',
    '<tr class="%s"><td class="value">%s</td><td>%s</td><td class="note">%s</td></tr>',
    '</table>'),
    .row_class(pa$result), pa$result, pa$source %||% "n/a", pa$reason %||% "")

  # Phase B
  sec_b <- sprintf(paste0(
    '<h2>Phase B — Pull Score</h2>',
    '<table>',
    '<tr><th>Component</th><th>Value</th><th>Pts</th><th>Note</th></tr>',
    '<tr class="%s"><td>Stage</td><td class="value">%s</td><td>%s</td><td class="note"></td></tr>',
    '<tr class="%s"><td>Sector RS rank</td><td class="value">%s</td><td>%s</td><td class="note">sector: %s</td></tr>',
    '<tr class="%s"><td>Footprint</td><td class="value">%s/3</td><td></td><td class="note"></td></tr>',
    '<tr class="%s"><td>pull_direction</td><td class="value">%s</td><td></td><td class="note">user direction <code>%s</code> alignment: <b>%s</b></td></tr>',
    '<tr class="%s"><td><b>pull_score</b></td><td class="value">%s/10</td><td></td><td class="note">cutoff &ge; 6 (extended &ge; 8)</td></tr>',
    '</table>'),
    .row_class(pb$result), pb$stage %||% "n/a", .fmt_num(pb$stage_pts, 0),
    .row_class(pb$result), .fmt_num(pb$sector_rs_rank, 0),
      .fmt_num(pb$sector_pts, 0), pb$sector %||% "n/a",
    .row_class(pb$result), .fmt_num(pb$footprint_pts, 0),
    .row_class(pb$result), pb$pull_direction %||% "n/a", direction,
      pb$direction_match %||% "n/a",
    .row_class(pb$result), .fmt_num(pb$pull_score, 0))

  # Phase C — cheap score components + funnel grid
  sec_c <- .render_phase_c(pc, direction, ctx$config)

  # Phase D
  sec_d <- .render_phase_d(pd, direction)

  # Phase E result table
  sec_e <- sprintf(paste0(
    '<h2>Phase E — Classification (mechanical)</h2>',
    '<table>',
    '<tr><th>Phase</th><th>Result</th></tr>',
    '<tr class="%s"><td>A &mdash; Universe rich-options</td><td class="value">%s</td></tr>',
    '<tr class="%s"><td>B &mdash; Pull</td><td class="value">%s</td></tr>',
    '<tr class="%s"><td>C &mdash; Cheap + Vol Funnel</td><td class="value">%s</td></tr>',
    '<tr class="%s"><td>D &mdash; Setup / Chain / R:R</td><td class="value">%s</td></tr>',
    '<tr class="%s"><td>E &mdash; v5 classification</td><td class="value">%s &nbsp; <span class="sub">phase_of_drop=%s</span></td></tr>',
    '</table>'),
    .row_class(pa$result), pa$result,
    .row_class(pb$result), pb$result,
    .row_class(pc$result), pc$result,
    .row_class(pd$result), pd$result,
    .row_class(pe$v5_classification), pe$v5_classification, pe$phase_of_drop)

  # Structures table
  sec_struct <- .render_structures(pd$structures, ctx$config)

  # Data summary
  sec_summary <- .render_summary(ctx)

  html <- paste0(
    '<!doctype html><html><head><meta charset="utf-8">',
    sprintf('<title>/analyze %s %s &mdash; %s</title>', ticker, direction, date),
    '<style>', .css, '</style></head><body>',
    sprintf('<h1>/analyze %s %s</h1>', ticker, direction),
    sprintf('<div class="sub">%s &middot; data-only report (no verdicts, no rankings)</div>',
            format(date)),
    badges, meta,
    sec_a, sec_b, sec_c, sec_d, sec_e, sec_struct, sec_summary,
    '<div class="sub" style="margin-top:30px">Generated by reports/analyze/main.R. ',
    'Sources: latest swing_scanner v5 CSV; mydb.db (Prices, option_skew_history, ',
    'option_chain_oi_history, scanner_rich_universe); Tdata helpers; ',
    'tdata_py.compute_spread_risk_reward when TWS reachable.</div>',
    '</body></html>')

  out_file <- file.path(out_dir, sprintf("analyze_%s_%s.html",
                                          ticker, format(date, "%Y%m%d")))
  writeLines(html, out_file, useBytes = TRUE)
  out_file
}

# ── Phase C section ──────────────────────────────────────────────────────
.render_phase_c <- function(pc, direction, config) {
  rows_c1 <- sprintf(paste0(
    '<tr class="%s"><td>cheap_score</td><td class="value">%s/10</td><td class="note">cutoff &ge; 6</td></tr>',
    '<tr class="%s"><td>cheap_side</td><td class="value">%s</td><td class="note">user direction: %s</td></tr>',
    '<tr class="%s"><td>IVP (used)</td><td class="value">%s%%</td><td class="note"></td></tr>',
    '<tr class="%s"><td>VRP (log-ratio, persisted)</td><td class="value">%s</td><td class="note"></td></tr>'),
    .row_class(pc$result), .fmt_num(pc$cheap_score, 0),
    .row_class(pc$result), pc$cheap_side %||% "n/a", direction,
    .row_class(pc$result), .fmt_num(pc$ivp_used, 1),
    .row_class(pc$result), .fmt_num(pc$vrp, 2))

  c1 <- sprintf(paste0(
    '<h3>C.1 Cheap Score</h3>',
    '<table><tr><th>Component</th><th>Value</th><th>Note</th></tr>',
    '%s</table>'), rows_c1)

  if (is.null(pc$funnel)) {
    return(paste0('<h2>Phase C — Cheap Score</h2>', c1,
                  '<p class="sub">Vol funnel skipped (--no-vol-funnel).</p>'))
  }

  fn <- pc$funnel
  grid_rows <- paste0(
    sapply(fn$grid, function(row) sprintf(
      '<tr class="row-warn"><td>%s</td><td>%s</td><td>%s</td></tr>',
      row$signal, row$reading, row$label)),
    collapse = "\n")
  tally <- fn$tally
  c2 <- sprintf(paste0(
    '<h3>C.2 Vol Funnel (data-only)</h3>',
    '<table><tr><th>Signal</th><th>Reading</th><th>Mechanical label</th></tr>',
    '%s</table>',
    '<p class="sub">Funnel tally for direction=<code>%s</code>: ',
    '<b>%d favorable / %d unfavorable / %d unavailable</b>. ',
    'Tally is a count, not a verdict.</p>'),
    grid_rows, direction, tally$favorable, tally$unfavorable, tally$unavailable)

  paste0('<h2>Phase C — Cheap Score + Vol Funnel</h2>', c1, c2)
}

# ── Phase D section ──────────────────────────────────────────────────────
.render_phase_d <- function(pd, direction) {
  t <- pd$targets
  targets_html <- sprintf(paste0(
    '<h3>Structural target sources</h3>',
    '<table><tr><th>Field</th><th>Value</th><th>Note</th></tr>',
    '<tr><td>spot_target_low</td><td class="value">%s</td><td class="note"></td></tr>',
    '<tr><td>spot_target_high</td><td class="value">%s</td><td class="note"></td></tr>',
    '<tr><td>targets_agreeing</td><td class="value">%s/3</td><td class="note">cutoff &ge; 2</td></tr>',
    '<tr><td>fib_confirms</td><td class="value">%s</td><td class="note">overlay only</td></tr>',
    '</table>'),
    .fmt_num(t$spot_target_low), .fmt_num(t$spot_target_high),
    .fmt_num(t$targets_agreeing, 0), as.character(t$fib_confirms %||% NA))

  chain_html <- sprintf(paste0(
    '<h3>Chain</h3>',
    '<table><tr><th>Field</th><th>Value</th></tr>',
    '<tr><td>oi_cap_call</td><td class="value">%s</td></tr>',
    '<tr><td>oi_cap_put</td><td class="value">%s</td></tr>',
    '<tr><td>chain_state</td><td class="value">%s</td></tr>',
    '<tr><td>effective_target</td><td class="value">%s</td></tr>',
    '<tr><td>R:R</td><td class="value">%s</td></tr>',
    '<tr><td>entry_floor / entry_ceiling</td><td class="value">%s / %s</td></tr>',
    '<tr><td>headroom_band</td><td class="value">%s</td></tr>',
    '<tr><td>entry_state</td><td class="value">%s</td></tr>',
    '</table>'),
    .fmt_num(pd$oi_cap_call), .fmt_num(pd$oi_cap_put),
    pd$chain_state %||% "n/a", .fmt_num(pd$effective_target),
    .fmt_num(pd$rr), .fmt_num(pd$entry_floor), .fmt_num(pd$entry_ceiling),
    pd$headroom_band %||% "n/a", pd$entry_state %||% "n/a")

  paste0('<h2>Phase D — Setup, Chain, R:R</h2>', targets_html, chain_html)
}

# ── Structures table ─────────────────────────────────────────────────────
.render_structures <- function(structures, config) {
  if (is.null(structures) || nrow(structures) == 0) {
    return(paste0(
      '<h2>Structures within $', config$risk_cap_lot_usd, '/lot cap</h2>',
      '<p class="sub">No structures enumerated (live pricer unavailable; ',
      'no DB cache hit). Run when TWS is reachable to populate this table.</p>'))
  }

  cols <- intersect(c("structure", "spread_type", "short_strike", "long_strike",
                      "width", "expiry", "net_premium", "debit", "max_risk",
                      "max_reward", "reward_risk_ratio", "prob_success_delta",
                      "edge", "expected_value", "within_cap", "surface_fact",
                      "source"), names(structures))
  hdr <- paste0(sprintf('<th>%s</th>', cols), collapse = "")
  rows <- apply(structures[, cols, drop = FALSE], 1, function(r) {
    cells <- paste0(sprintf('<td>%s</td>',
                            ifelse(is.na(r), "&mdash;", as.character(r))),
                    collapse = "")
    sprintf('<tr class="row-warn">%s</tr>', cells)
  })

  paste0(
    '<h2>Structures within $', config$risk_cap_lot_usd, '/lot cap</h2>',
    '<table><tr>', hdr, '</tr>', paste(rows, collapse = "\n"), '</table>',
    '<p class="sub">Neutral enumeration. Rows are not ranked or recommended. ',
    '<code>within_cap</code> indicates max_risk &le; $',
    config$risk_cap_lot_usd, ' per lot.</p>')
}

# ── Data summary ─────────────────────────────────────────────────────────
.render_summary <- function(ctx) {
  pb <- ctx$phase_b; pc <- ctx$phase_c; pd <- ctx$phase_d; pe <- ctx$phase_e
  fn <- pc$funnel
  rows <- list(
    c("v5 classification", pe$v5_classification),
    c("phase_of_drop", pe$phase_of_drop),
    c("pull_score / direction",
      sprintf("%s / %s", .fmt_num(pb$pull_score, 0), pb$pull_direction %||% "n/a")),
    c("sector_rs_rank", .fmt_num(pb$sector_rs_rank, 0)),
    c("cheap_score / side",
      sprintf("%s / %s", .fmt_num(pc$cheap_score, 0), pc$cheap_side %||% "n/a")),
    c("IVP / VRP (log)",
      sprintf("%s%% / %s",
              .fmt_num(if (!is.null(fn)) fn$ivp_used else pc$ivp_used, 1),
              .fmt_num(if (!is.null(fn)) fn$vrp_log else pc$vrp, 2))),
    c("Term shape",
      if (!is.null(fn)) fn$term_shape else "n/a"),
    c("RR 25Δ (vp)",
      if (!is.null(fn)) .fmt_num(fn$rr_vp, 1) else "n/a"),
    c("Earnings",
      if (!is.null(fn) && !is.na(fn$earnings_dte))
        sprintf("%s (%dd)", as.character(fn$earnings_date), fn$earnings_dte)
      else "n/a"),
    c("Targets agreeing", .fmt_num(pd$targets_agreeing, 0)),
    c("Effective target", .fmt_num(pd$effective_target)),
    c("R:R / entry_state",
      sprintf("%s / %s", .fmt_num(pd$rr), pd$entry_state %||% "n/a")),
    c("Structures within cap",
      .fmt_num(pd$n_structures_within_cap, 0))
  )
  body <- paste0(sapply(rows, function(r)
    sprintf('<tr><td>%s</td><td class="value">%s</td></tr>', r[1], r[2])),
    collapse = "")
  paste0('<h2>Data Summary</h2>',
         '<table><tr><th>Metric</th><th>Value</th></tr>',
         body, '</table>')
}
