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
[title]{cursor:help;border-bottom:1px dotted #999}
.retrieved{font-size:11px;color:#888;margin:4px 0 8px;font-style:italic}
details{margin:6px 0 14px;background:#fff;border:1px solid var(--rule);border-radius:4px}
details>summary{padding:8px 12px;cursor:pointer;font-weight:600;font-size:13px;color:#444;user-select:none;list-style:none}
details>summary::-webkit-details-marker{display:none}
details>summary::before{content:"▸ ";color:#888;font-size:11px}
details[open]>summary::before{content:"▾ "}
details>summary:hover{background:#f3f3f3}
details table{margin:0;border-top:1px solid var(--rule)}
.crit-pass{color:#0072B2;font-weight:600}
.crit-fail{color:#D55E00;font-weight:600}
.crit-info{color:#888}
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

# ── Tooltip glossary (field → explanation) ───────────────────────────────
.TOOLTIPS <- list(
  # Header / classification
  "Spot"                 = "Latest IBKR last-trade price for the underlying.",
  "Sector"               = "GICS-style sector membership from the ScannerUniverse table.",
  "classification"       = "TOP PICK = passes A,B,C,D and at least one structure within $/lot cap. WATCH = passes A,B,C only. SKIP = drops earlier.",
  "phase_of_drop"        = "Which phase the ticker dropped at (A/B/C/D), or 'none' if all passed.",
  # Phase B aggregate
  "Stage"                = "Stage classification from the Pull screen: extended (>15% above MA50), early (BOT setup+breakout fired), continuation (Tdata::isTrendContinuation), or none.",
  "Sector RS rank"       = "Rank of this ticker's sector among LONG-passing sectors by 20-day relative strength vs SPY.",
  "Footprint"            = "Confirmation footprint points (0-3): OBV slope, volume surge, up/down ratio.",
  "pull_direction"       = "Implied direction of the Pull setup: up / down / neutral.",
  "pull_score"           = "0-10 composite of stage (4) + sector (3) + footprint (3). Cutoff for PASS is >=6 (>=8 if extended).",
  # Phase C aggregate
  "cheap_score"          = "0-10 composite from IVP / VRP / term shape / RR alignment. Cutoff for PASS is >=6.",
  "cheap_side"           = "Implied directional bias from skew: long (calls bid), short (puts bid), neutral.",
  "IVP (used)"           = "Implied Volatility Percentile actually used (1y native if present, else 2y fallback). Lower = options cheaper.",
  "VRP (log-ratio, persisted)" = "Volatility Risk Premium = log(IV30/RV30)*100. Negative = IV below realised, options cheap. Positive = IV rich.",
  # Vol funnel grid
  "IV Rank 1Y"           = "Where current IV30 sits in its 1-year history (0-100%).",
  "VRP"                  = "Volatility Risk Premium in two forms: log-ratio and vol-points (IV-RV)*100.",
  "Term IV30/IV90"       = "Front-vs-back-month IV term structure. Negative = contango (back > front). Positive = backwardation.",
  "Skew (RR 25Δ)"   = "Risk-reversal at 25-delta in vol-points: (call25 IV - put25 IV) * 100. Positive = calls bid.",
  "Earnings"             = "Days until next earnings (from yfinance). Negative = past, 0-14 = inside event window.",
  "Sector x-rank"        = "Cross-sectional rank passthrough (handled by Phase B sector_rs_rank).",
  # Phase D
  "spot_target_low"      = "Lower bound of the structural-target consensus from prior swing high / 52w high / round number.",
  "spot_target_high"     = "Upper bound of the structural-target consensus.",
  "targets_agreeing"     = "Count of consensus sources agreeing within ±2% (range 0-3). Cutoff for PASS is >=2.",
  "fib_confirms"         = "TRUE if Fibonacci 1.272/1.618 lands within ±2% of the structural target.",
  "expiry"               = "Selected expiration (YYYYMMDD). Picked live from IBKR ~45 DTE if scanner CSV is silent.",
  "oi_cap_call"          = "Strike with the largest call open interest in [-25%, +25%] of spot — magnetic resistance.",
  "oi_cap_put"           = "Strike with the largest put open interest in [-25%, +25%] of spot — magnetic support.",
  "chain_state"          = "open (top-3 OI < 40% of total), crowded (40-60%), or chain-capped (>=60%, expect price pinning).",
  "effective_target"     = "Lower of structural target and OI cap — whichever the chain says is reachable.",
  "R:R"                  = "Reward / risk on the proposed entry. Cutoff for PASS is rr_min (default 0.5).",
  "entry_floor / entry_ceiling" = "Acceptable entry-price band that yields R:R >= rr_min.",
  "headroom_band"        = "Mechanical label: tight / moderate / wide based on entry-band width.",
  "entry_state"          = "IN BAND (current premium fits) / ABOVE / BELOW / FAILED.",
  # Per-indicator breakdown
  "S1"                   = "Setup S1: price above MA50 (uptrend established).",
  "S2"                   = "Setup S2: MA50 5-day slope > 0 (trend confirmed).",
  "S4"                   = "Setup S4: OBV 20-day slope > 0 (accumulation).",
  "S5"                   = "Setup S5: 20d-range / 40d-range < 0.65 (contraction; squeeze).",
  "S6"                   = "Setup S6: 20d-vol / 50d-vol < 0.95 (supply drying up).",
  "BK1"                  = "Breakout BK1: RSI(14) crossing 50 with positive 5-day slope.",
  "BK2"                  = "Breakout BK2: 10-day up-volume / down-volume > 1.1 (buying pressure).",
  "BK3"                  = "Breakout BK3: range position >= 70% of 20d range (pushing high).",
  "BK4"                  = "Breakout BK4: today's volume >= 1.2x of 20d average."
)

#' Wrap a label in a tooltip span if a glossary entry exists.
.tt <- function(label) {
  desc <- .TOOLTIPS[[label]]
  if (is.null(desc)) return(label)
  sprintf('<span title="%s">%s</span>',
          gsub('"', '&quot;', desc, fixed = TRUE),
          label)
}

#' Render a "data retrieved at" caption for a phase block.
.retrieved_caption <- function(...) {
  parts <- list(...)
  parts <- Filter(function(x) !is.null(x) && length(x) > 0 &&
                              !is.na(x[1]) && nzchar(as.character(x[1])),
                  parts)
  if (length(parts) == 0) return("")
  txt <- paste(sapply(names(parts), function(k)
    sprintf("%s: %s", k, format(parts[[k]]))), collapse = " &middot; ")
  sprintf('<div class="retrieved">data retrieved &mdash; %s</div>', txt)
}

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

#' Like .fmt_num but, when the value is NA and `reason` is non-empty, returns
#' "FETCH FAILED: <reason>" instead of "n/a". Surfaces the cause directly in
#' the report (see feedback_analyze_live_data_fallback.md).
.fmt_cell <- function(x, reason = NULL, digits = 2) {
  if (!is.null(x) && !all(is.na(x))) {
    if (is.numeric(x)) return(sprintf(paste0("%.", digits, "f"), x))
    return(as.character(x))
  }
  if (!is.null(reason) && length(reason) > 0 && nzchar(reason))
    return(paste0("FETCH FAILED: ", reason))
  "n/a"
}

# ── Main render entry ────────────────────────────────────────────────────
render_analyze_html <- function(ctx, out_dir) {
  ticker <- ctx$ticker; direction <- ctx$direction; date <- ctx$date
  pa <- ctx$phase_a; pb <- ctx$phase_b; pc <- ctx$phase_c
  pd <- ctx$phase_d; pe <- ctx$phase_e

  # Header strip
  meta <- sprintf(paste0(
    '<div class="meta-grid">',
    '<div class="cell"><div class="lbl">%s</div><div class="val">$%s</div></div>',
    '<div class="cell"><div class="lbl">%s</div><div class="val">%s</div></div>',
    '<div class="cell"><div class="lbl">%s</div><div class="val">%s</div></div>',
    '<div class="cell"><div class="lbl">%s</div><div class="val">%s</div></div>',
    '</div>'),
    .tt("Spot"), .fmt_num(pb$price),
    .tt("Sector"), pb$sector %||% "n/a",
    .tt("classification"), pe$classification,
    .tt("phase_of_drop"), pe$phase_of_drop)

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
    .badge_class(pe$classification), pe$classification)

  # Phase A
  sec_a <- paste0(
    '<h2>Phase A — Universe Rich-Options Gate</h2>',
    .retrieved_caption(`scanner CSV` = pa$retrieved_at),
    sprintf(paste0(
      '<table><tr><th>Result</th><th>Source</th><th>Note</th></tr>',
      '<tr class="%s"><td class="value">%s</td><td>%s</td><td class="note">%s</td></tr>',
      '</table>'),
      .row_class(pa$result), pa$result, pa$source %||% "n/a", pa$reason %||% ""))

  # Phase B — aggregate table + collapsible per-indicator breakdown
  sec_b_aggregate <- paste0(
    '<h2>Phase B — Pull Score</h2>',
    .retrieved_caption(`scanner CSV` = pb$scanner_csv_mtime,
                       `OHLC live`   = pb$breakdown_retrieved_at),
    sprintf(paste0(
      '<table>',
      '<tr><th>Component</th><th>Value</th><th>Pts</th><th>Note</th></tr>',
      '<tr class="%s"><td>%s</td><td class="value">%s</td><td>%s</td><td class="note"></td></tr>',
      '<tr class="%s"><td>%s</td><td class="value">%s</td><td>%s</td><td class="note">sector: %s</td></tr>',
      '<tr class="%s"><td>%s</td><td class="value">%s/3</td><td></td><td class="note"></td></tr>',
      '<tr class="%s"><td>%s</td><td class="value">%s</td><td></td><td class="note">user direction <code>%s</code> alignment: <b>%s</b></td></tr>',
      '<tr class="%s"><td><b>%s</b></td><td class="value">%s/10</td><td></td><td class="note">cutoff &ge; 6 (extended &ge; 8)</td></tr>',
      '</table>'),
      .row_class(pb$result), .tt("Stage"), pb$stage %||% "n/a", .fmt_num(pb$stage_pts, 0),
      .row_class(pb$result), .tt("Sector RS rank"), .fmt_num(pb$sector_rs_rank, 0),
        .fmt_num(pb$sector_pts, 0), pb$sector %||% "n/a",
      .row_class(pb$result), .tt("Footprint"), .fmt_num(pb$footprint_pts, 0),
      .row_class(pb$result), .tt("pull_direction"), pb$pull_direction %||% "n/a", direction,
        pb$direction_match %||% "n/a",
      .row_class(pb$result), .tt("pull_score"), .fmt_num(pb$pull_score, 0)))

  sec_b <- paste0(sec_b_aggregate, .render_phase_b_breakdown(pb$breakdown))

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
    '<tr class="%s"><td>E &mdash; classification</td><td class="value">%s &nbsp; <span class="sub">phase_of_drop=%s</span></td></tr>',
    '</table>'),
    .row_class(pa$result), pa$result,
    .row_class(pb$result), pb$result,
    .row_class(pc$result), pc$result,
    .row_class(pd$result), pd$result,
    .row_class(pe$classification), pe$classification, pe$phase_of_drop)

  # Structures table — vehicle-aware (shared/vehicle_rule.R)
  sec_struct <- .render_structures(pd$structures, ctx$config,
                                   vehicle = pd$vehicle,
                                   vehicle_reason = pd$vehicle_reason,
                                   structures_retrieved_at = pd$structures_retrieved_at)

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
    'Sources: latest swing_scanner CSV; mydb.db (Prices, option_skew_history, ',
    'option_chain_oi_history, scanner_rich_universe); Tdata helpers; ',
    'tdata_py.compute_spread_risk_reward when TWS reachable.</div>',
    '</body></html>')

  out_file <- file.path(out_dir, sprintf("analyze_%s_%s.html",
                                          ticker, format(date, "%Y%m%d")))
  writeLines(html, out_file, useBytes = TRUE)
  out_file
}

# ── Phase B per-indicator breakdown (collapsible) ────────────────────────
.render_phase_b_breakdown <- function(breakdown) {
  if (is.null(breakdown) || nrow(breakdown) == 0) {
    return(paste0(
      '<details><summary>Per-indicator breakdown</summary>',
      '<p class="sub" style="padding:8px 12px;margin:0">',
      'Breakdown unavailable (live OHLC fetch failed or insufficient history).',
      '</p></details>'))
  }

  setup_n <- attr(breakdown, "setup_count") %||% 0
  bk_n    <- attr(breakdown, "breakout_count") %||% 0

  cells <- function(rr) {
    pass <- rr$pass
    badge <- if (is.na(pass)) '<span class="crit-info">info</span>'
             else if (isTRUE(pass)) '<span class="crit-pass">PASS</span>'
             else '<span class="crit-fail">FAIL</span>'
    id_html <- if (!is.null(.TOOLTIPS[[rr$id]])) .tt(rr$id) else rr$id
    sprintf(paste0(
      '<tr><td><code>%s</code></td><td>%s</td>',
      '<td class="value">%s</td><td class="note">%s</td>',
      '<td>%s</td><td class="note">%s</td></tr>'),
      id_html, rr$label, rr$value, rr$threshold, badge, rr$note)
  }
  rows_html <- paste0(
    vapply(seq_len(nrow(breakdown)),
           function(i) cells(breakdown[i, , drop = FALSE]),
           character(1)),
    collapse = "\n")

  paste0(
    sprintf(paste0(
      '<details open><summary>Per-indicator breakdown ',
      '(setup %d/5 &middot; breakout %d/4) — click to collapse</summary>'),
      setup_n, bk_n),
    '<table>',
    '<tr><th>ID</th><th>Indicator</th><th>Value</th><th>Threshold</th>',
    '<th>Result</th><th>Note</th></tr>',
    rows_html,
    '</table></details>')
}

# ── Phase C section ──────────────────────────────────────────────────────
.render_phase_c <- function(pc, direction, config) {
  rows_c1 <- sprintf(paste0(
    '<tr class="%s"><td>%s</td><td class="value">%s/10</td><td class="note">cutoff &ge; 6</td></tr>',
    '<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">user direction: %s</td></tr>',
    '<tr class="%s"><td>%s</td><td class="value">%s%%</td><td class="note"></td></tr>',
    '<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note"></td></tr>'),
    .row_class(pc$result), .tt("cheap_score"), .fmt_num(pc$cheap_score, 0),
    .row_class(pc$result), .tt("cheap_side"), pc$cheap_side %||% "n/a", direction,
    .row_class(pc$result), .tt("IVP (used)"), .fmt_num(pc$ivp_used, 1),
    .row_class(pc$result), .tt("VRP (log-ratio, persisted)"), .fmt_num(pc$vrp, 2))

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
      .tt(row$signal), row$reading, row$label)),
    collapse = "\n")
  tally <- fn$tally
  retrieved <- if (!is.null(fn$retrieved))
    .retrieved_caption(`Prices DB` = fn$retrieved$prices_db,
                       `Skew DB`   = fn$retrieved$skew_db,
                       `live now`  = fn$retrieved$live_now) else ""
  c2 <- paste0(
    '<h3>C.2 Vol Funnel (data-only)</h3>',
    retrieved,
    sprintf(paste0(
      '<table><tr><th>Signal</th><th>Reading</th><th>Mechanical label</th></tr>',
      '%s</table>',
      '<p class="sub">Funnel tally for direction=<code>%s</code>: ',
      '<b>%d favorable / %d unfavorable / %d unavailable</b>. ',
      'Tally is a count, not a verdict.</p>'),
      grid_rows, direction, tally$favorable, tally$unfavorable, tally$unavailable))

  paste0('<h2>Phase C — Cheap Score + Vol Funnel</h2>', c1, c2)
}

# ── Phase D section ──────────────────────────────────────────────────────
.render_phase_d <- function(pd, direction) {
  t <- pd$targets
  t_reason <- t$reason
  src_caption <- if (!is.null(t$source) && t$source == "live OHLC")
    '<p class="sub" style="margin:0 0 6px">Targets re-derived live from OHLC history (scanner did not emit).</p>'
  else if (!is.null(pd$entry_source) && pd$entry_source == "live")
    '<p class="sub" style="margin:0 0 6px">R:R / entry framework re-derived live (scanner did not emit).</p>'
  else ''
  targets_html <- paste0(src_caption, sprintf(paste0(
    '<h3>Structural target sources</h3>',
    '<table><tr><th>Field</th><th>Value</th><th>Note</th></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note"></td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note"></td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">cutoff &ge; 2</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">overlay only</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    '</table>'),
    .tt("spot_target_low"),  .fmt_cell(t$spot_target_low, t_reason),
    .tt("spot_target_high"), .fmt_cell(t$spot_target_high, t_reason),
    .tt("targets_agreeing"), .fmt_cell(t$targets_agreeing, t_reason, digits = 0),
    .tt("fib_confirms"),
      if (is.na(t$fib_confirms)) (.fmt_cell(NA, t_reason)) else as.character(t$fib_confirms),
    .tt("expiry"), .fmt_cell(pd$expiry, pd$expiry_reason),
    if (!is.null(pd$expiry_reason)) "live-picked from IBKR" else "from scanner CSV"))

  c_reason <- pd$chain_reason
  e_reason <- pd$entry_reason
  chain_html <- sprintf(paste0(
    '<h3>Chain</h3>',
    '<table><tr><th>Field</th><th>Value</th></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s / %s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td></tr>',
    '</table>'),
    .tt("oi_cap_call"), .fmt_cell(pd$oi_cap_call, c_reason),
    .tt("oi_cap_put"),  .fmt_cell(pd$oi_cap_put,  c_reason),
    .tt("chain_state"), .fmt_cell(pd$chain_state, c_reason),
    .tt("effective_target"), .fmt_cell(pd$effective_target, e_reason),
    .tt("R:R"), .fmt_cell(pd$rr, e_reason),
    .tt("entry_floor / entry_ceiling"),
      .fmt_cell(pd$entry_floor, e_reason), .fmt_cell(pd$entry_ceiling, e_reason),
    .tt("headroom_band"), .fmt_cell(pd$headroom_band, e_reason),
    .tt("entry_state"), .fmt_cell(pd$entry_state, e_reason))

  paste0('<h2>Phase D — Setup, Chain, R:R</h2>', targets_html, chain_html)
}

# ── Structures table ─────────────────────────────────────────────────────
#
# Vehicle rule (shared/vehicle_rule.R) selects ONE applicable vehicle.
# We render the matching section open by default; the others (if data is
# available) collapse to <details> the user can expand on demand.
.render_structures <- function(structures, config, vehicle = "spread",
                               vehicle_reason = NULL,
                               structures_retrieved_at = NULL) {
  cap <- config$risk_cap_lot_usd

  vehicle_banner <- sprintf(
    '<h2>Recommended vehicle: <code>%s</code></h2>',
    vehicle %||% "n/a")
  if (!is.null(vehicle_reason) && nzchar(vehicle_reason)) {
    vehicle_banner <- paste0(vehicle_banner,
      sprintf('<p class="sub">Per shared vehicle rule: %s</p>',
              vehicle_reason))
  }

  retrieved_html <- .retrieved_caption(`live pricer` = structures_retrieved_at)

  spread_table <- .render_structures_table(structures, cap)

  # Conditional rendering by vehicle
  body <- if (identical(vehicle, "spread")) {
    paste0(
      sprintf('<details open><summary>Vertical spreads ($%d/lot cap) — applicable</summary>',
              cap),
      retrieved_html, spread_table, '</details>')
  } else if (identical(vehicle, "call")) {
    paste0(
      '<p class="sub"><b>Outright call</b> selected by vehicle rule. ',
      'A single-leg long call is the recommended structure for this name; ',
      'no spread tabulation is needed.</p>',
      sprintf('<details><summary>Spread enumeration (not applicable per vehicle rule) — click to expand</summary>'),
      retrieved_html, spread_table, '</details>')
  } else if (identical(vehicle, "stock")) {
    paste0(
      '<p class="sub"><b>Stock</b> selected by vehicle rule (price &lt; $10 ',
      'or option spread too wide). Option structures not applicable.</p>',
      sprintf('<details><summary>Spread enumeration (not applicable per vehicle rule) — click to expand</summary>'),
      retrieved_html, spread_table, '</details>')
  } else {
    paste0(retrieved_html, spread_table)
  }

  paste0(vehicle_banner, body)
}

#' Inner spread enumeration table (no h2, no banner). Returns "&mdash; none &mdash;" placeholder when empty.
.render_structures_table <- function(structures, cap) {
  if (is.null(structures) || nrow(structures) == 0) {
    return(paste0('<p class="sub">No structures enumerated (live pricer unavailable; ',
                  'no DB cache hit). Run when TWS is reachable to populate this table.</p>'))
  }
  cols <- intersect(c("structure", "spread_type", "short_strike", "long_strike",
                      "width", "expiry", "net_premium", "debit", "max_risk",
                      "max_reward", "reward_risk_ratio", "prob_success_delta",
                      "edge", "expected_value", "within_cap", "surface_fact",
                      "source"), names(structures))
  hdr <- paste0(sprintf('<th>%s</th>', cols), collapse = "")
  rows <- vapply(seq_len(nrow(structures)), function(i) {
    r <- structures[i, cols, drop = FALSE]
    cells <- paste0(vapply(cols, function(cc) {
      v <- r[[cc]]
      sprintf('<td>%s</td>',
              if (is.null(v) || length(v) == 0 || is.na(v)) "&mdash;" else as.character(v))
    }, character(1)), collapse = "")
    sprintf('<tr class="row-warn">%s</tr>', cells)
  }, character(1))
  paste0('<table><tr>', hdr, '</tr>', paste(rows, collapse = "\n"), '</table>',
         sprintf('<p class="sub">Neutral enumeration. Rows are not ranked or recommended. ',
                 '<code>within_cap</code> indicates max_risk &le; $%d per lot.</p>',
                 cap))
}

# ── Data summary ─────────────────────────────────────────────────────────
.render_summary <- function(ctx) {
  pb <- ctx$phase_b; pc <- ctx$phase_c; pd <- ctx$phase_d; pe <- ctx$phase_e
  fn <- pc$funnel
  fn_reasons <- if (!is.null(fn)) fn$reasons else list()
  c_reason <- pd$chain_reason
  e_reason <- pd$entry_reason
  t_reason <- pd$targets$reason

  rows <- list(
    c("classification", pe$classification),
    c("phase_of_drop", pe$phase_of_drop),
    c("pull_score / direction",
      sprintf("%s / %s", .fmt_num(pb$pull_score, 0), pb$pull_direction %||% "n/a")),
    c("sector_rs_rank", .fmt_num(pb$sector_rs_rank, 0)),
    c("cheap_score / side",
      sprintf("%s / %s", .fmt_num(pc$cheap_score, 0), pc$cheap_side %||% "n/a")),
    c("IVP / VRP (log)",
      sprintf("%s / %s",
              .fmt_cell(if (!is.null(fn)) fn$ivp_used else pc$ivp_used,
                        fn_reasons$ivp, 1),
              .fmt_cell(if (!is.null(fn)) fn$vrp_log else pc$vrp,
                        fn_reasons$vrp, 2))),
    c("Term shape",
      if (!is.null(fn)) fn$term_shape else "n/a"),
    c("RR 25Δ (vp)",
      .fmt_cell(if (!is.null(fn)) fn$rr_vp else NA, fn_reasons$rr, 1)),
    c("Earnings",
      if (!is.null(fn) && !is.na(fn$earnings_dte))
        sprintf("%s (%dd)", as.character(fn$earnings_date), fn$earnings_dte)
      else .fmt_cell(NA, fn_reasons$earnings)),
    c("Targets agreeing", .fmt_cell(pd$targets_agreeing, t_reason, 0)),
    c("Effective target", .fmt_cell(pd$effective_target, e_reason)),
    c("R:R / entry_state",
      sprintf("%s / %s",
              .fmt_cell(pd$rr, e_reason),
              .fmt_cell(pd$entry_state, e_reason))),
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
