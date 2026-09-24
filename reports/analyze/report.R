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
table.sortable th{cursor:pointer;user-select:none;position:relative;padding-right:18px}
table.sortable th:hover{background:#e7e7e7}
table.sortable th::after{content:"";position:absolute;right:6px;top:50%;transform:translateY(-50%);border:4px solid transparent;opacity:.25}
table.sortable th.sort-asc::after{border-bottom-color:#444;border-top:0;opacity:1;margin-top:-2px}
table.sortable th.sort-desc::after{border-top-color:#444;border-bottom:0;opacity:1;margin-top:2px}
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
.warn-box{background:var(--warn-light);border-left:4px solid var(--warn-bg);padding:8px 12px;margin:6px 0 14px;font-size:13px;color:#6b4e00}
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
  # Phase B aggregate
  "Sector x-rank"        = "Cross-sectional context (see Phase B sector_rs_rank).",
  # Phase C aggregate
  "cheap_score"          = "0-9 composite from IVP / VRP / term shape / RR alignment. Higher = options structurally cheaper for buying premium (informational, not a gate).",
  "cheap_side"           = "Implied directional bias from skew: long (calls bid), short (puts bid), neutral.",
  "IVP (used)"           = "Implied Volatility Percentile actually used (1y native if present, else 2y fallback). Lower = options cheaper.",
  "VRP (log-ratio, persisted)" = "Volatility Risk Premium = log(IV30/RV30)*100. Negative = IV below realised, options cheap. Positive = IV rich.",
  # Vol funnel grid
  "IV Rank 1Y"           = "Where current IV30 sits in its 1-year history (0-100%).",
  "VRP"                  = "Volatility Risk Premium in two forms: log-ratio and vol-points (IV-RV)*100.",
  "Term IV30/IV90"       = "Front-vs-back-month IV term structure. Negative = contango (back > front). Positive = backwardation.",
  "Skew (RR 25Δ)"   = "Risk-reversal at 25-delta in vol-points: (call25 IV - put25 IV) * 100. Positive = calls bid.",
  "Earnings"             = "Days until next earnings (from yfinance). Negative = past, 0-14 = inside event window.",
  # Phase D
  "spot_target_low"      = "NEAREST structural target to spot (drives effective target & R:R), from prior swing high/low, 52w high/low, or round number. Long: lower price; short: higher price (first downside level).",
  "spot_target_high"     = "FARTHER structural target. Long: higher price; short: lower price. '—' when a single corroborated level (e.g. swing high == 52w high).",
  "targets_agreeing"     = "Count of consensus sources agreeing within ±2% (range 0-3). More agreement = a sharper structural target.",
  "fib_confirms"         = "TRUE if Fibonacci 1.272/1.618 lands within ±2% of the structural target.",
  "move_position"        = "Move maturity: how far the move has travelled from its swing base to the nearest structural target — (a) % of that leg, (b) the nearest Fibonacci rung, and the next extension rung priced out as a forward level. The base is the most recent swing pivot (local low for longs / high for shorts) within the move_lookback_days window (default 40d, sized to the 2-4 week breakout horizon; tunable in config.yml), NOT the multi-month swing low. High % / near the 1.0 rung = an extended move with little room left to the wall; low % = early. Descriptive only — does not feed scoring or flow.",
  "expiry"               = "Selected expiration (YYYYMMDD). Picked live from IBKR ~45 DTE if scanner CSV is silent.",
  "oi_cap_call"          = "Strike with the largest call open interest in [-25%, +25%] of spot — magnetic resistance.",
  "oi_cap_put"           = "Strike with the largest put open interest in [-25%, +25%] of spot — magnetic support.",
  "chain_state"          = "open (top-3 OI < 40% of total), crowded (40-60%), or chain-capped (>=60%, expect price pinning).",
  "effective_target"     = "Lower of structural target and OI cap — whichever the chain says is reachable.",
  "R:R"                  = "Reward / risk on the proposed entry (rr_min reference default 0.5).",
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

#' Wrap a label in a tooltip span. If `override` is supplied, use it as the
#' tooltip text; otherwise fall back to the .TOOLTIPS glossary entry.
.tt <- function(label, override = NULL) {
  desc <- if (!is.null(override) && nzchar(override)) override
          else .TOOLTIPS[[label]]
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

# Vanilla-JS sortable: click any <th> in a table.sortable to toggle ASC/DESC.
# Numeric columns sort numerically; text columns lexicographically.
.sortable_script <- function() {
  '<script>
(function(){
  function parseCell(td){
    var t = td.textContent.trim();
    if (t === "" || t === "—" || t === "n/a") return {n: NaN, s: ""};
    var n = parseFloat(t.replace(/[$,%]/g, "").replace(/[+]/g, ""));
    return {n: n, s: t.toLowerCase()};
  }
  function sortBy(table, idx, dir){
    var tbody = table.tBodies[0];
    var rows = Array.prototype.slice.call(tbody.rows);
    var allNumeric = rows.every(function(r){
      var c = parseCell(r.cells[idx]);
      return c.s === "" || !isNaN(c.n);
    });
    rows.sort(function(a,b){
      var ca = parseCell(a.cells[idx]), cb = parseCell(b.cells[idx]);
      var x, y;
      if (allNumeric){ x = ca.n; y = cb.n; }
      else { x = ca.s; y = cb.s; }
      // NaN/empty always last
      var aEmpty = (allNumeric ? isNaN(x) : x === "");
      var bEmpty = (allNumeric ? isNaN(y) : y === "");
      if (aEmpty && bEmpty) return 0;
      if (aEmpty) return 1;
      if (bEmpty) return -1;
      if (x < y) return dir === "asc" ? -1 : 1;
      if (x > y) return dir === "asc" ?  1 : -1;
      return 0;
    });
    rows.forEach(function(r){ tbody.appendChild(r); });
  }
  document.querySelectorAll("table.sortable").forEach(function(table){
    var ths = table.tHead ? table.tHead.rows[0].cells : table.rows[0].cells;
    Array.prototype.forEach.call(ths, function(th, i){
      th.addEventListener("click", function(){
        var dir = th.classList.contains("sort-asc") ? "desc" : "asc";
        Array.prototype.forEach.call(ths, function(o){
          o.classList.remove("sort-asc","sort-desc");
        });
        th.classList.add("sort-" + dir);
        sortBy(table, i, dir);
      });
    });
  });
})();
</script>'
}

.row_class <- function(result) {
  switch(result %||% "STALE",
         PASS = "row-pass", LIVE = "row-pass", CACHED = "row-pass",
         SKIP = "row-fail", `FETCH FAILED` = "row-fail",
         `NO DATA` = "row-warn",
         `NO SIGNAL` = "row-skip",
         STALE = "row-warn",
         "row-warn")
}

.fmt_num <- function(x, digits = 2) {
  if (is.null(x) || all(is.na(x))) return("n/a")
  if (is.numeric(x)) sprintf(paste0("%.", digits, "f"), x) else as.character(x)
}

#' Like .fmt_num but, when the value is NA and `reason` is non-empty, returns
#' "<status>: <reason>" instead of "n/a". `status` distinguishes
#' NO DATA (request OK, response empty/NaN) from FETCH FAILED (no response).
#' Surfaces the cause directly in the report
#' (see feedback_analyze_live_data_fallback.md).
.fmt_cell <- function(x, reason = NULL, digits = 2, status = "FETCH FAILED") {
  if (!is.null(x) && !all(is.na(x))) {
    if (is.numeric(x)) return(sprintf(paste0("%.", digits, "f"), x))
    return(as.character(x))
  }
  if (!is.null(reason) && length(reason) > 0 && nzchar(reason))
    return(paste0(status %||% "FETCH FAILED", ": ", reason))
  "n/a"
}

#' Map a provenance status to a row/badge CSS class. Neutral palette:
#' LIVE/CACHED read as informational-good, NO DATA as warn, FETCH FAILED as fail.
.status_class <- function(status, kind = c("row", "badge")) {
  kind <- match.arg(kind)
  base <- switch(status %||% "FETCH FAILED",
                 LIVE = "pass", CACHED = "pass",
                 `NO DATA` = "warn",
                 `FETCH FAILED` = "fail",
                 SKIPPED = "skip", `SKIPPED (--no-vol-funnel)` = "skip",
                 "skip")
  paste0(kind, "-", base)
}

#' Render the Phase A bid/ask-spread sub-table. One row per probed leg
#' (ATM call, ATM put, 30Δ call, 30Δ put). Neutral: bare numbers, no verdict.
#' `spread` cell shows the normalized (ask-bid)/mid as a percentage.
.render_phase_a_spread <- function(pa) {
  sp <- pa$spread
  if (is.null(sp)) {
    # Probe didn't return a value — surface the provenance status + reason.
    st <- pa$spread_status %||% "FETCH FAILED"
    rs <- pa$spread_reason
    return(sprintf(paste0(
      '<h3>Option bid/ask spread (liquidity)</h3>',
      '<p class="sub">%s%s</p>'),
      st, if (!is.null(rs) && nzchar(rs)) paste0(" &mdash; ", rs) else ""))
  }

  leg_row <- function(label, g) {
    if (is.null(g)) g <- list(strike = NA, spread = NA, bid = NA, ask = NA, delta = NA)
    spread_txt <- if (is.null(g$spread) || is.na(g$spread)) "n/a"
                  else sprintf("%.1f%%", g$spread * 100)
    sprintf(paste0(
      '<tr><td>%s</td><td class="value">%s</td><td class="value">%s</td>',
      '<td class="value">%s</td><td class="value">%s</td>',
      '<td class="value">%s</td></tr>'),
      label,
      .fmt_num(g$strike, 2), .fmt_num(g$delta, 2),
      .fmt_num(g$bid, 2), .fmt_num(g$ask, 2), spread_txt)
  }

  rows <- paste0(
    leg_row("ATM call", sp$atm_call),
    leg_row("ATM put",  sp$atm_put),
    leg_row("30&Delta; call", sp$c30),
    leg_row("30&Delta; put",  sp$p30))

  paste0(
    '<h3>Option bid/ask spread (liquidity)</h3>',
    .retrieved_caption(`source` = pa$spread_retrieved_at),
    sprintf('<p class="sub">Expiry probed: %s (%s DTE). Spread = (ask &minus; bid) / mid. ',
            sp$expiration %||% "n/a", sp$dte %||% "n/a"),
    sprintf('ATM bid/ask %s%% feeds the vehicle rule (&gt;8%% &rarr; trade stock, not options).</p>',
            .fmt_num(sp$atm_bid_ask_pct, 1)),
    '<table><tr><th>Leg</th><th>Strike</th><th>&Delta;</th>',
    '<th>Bid</th><th>Ask</th><th>Spread</th></tr>',
    rows, '</table>')
}

# ── Main render entry ────────────────────────────────────────────────────
render_analyze_html <- function(ctx, out_dir) {
  ticker <- ctx$ticker; direction <- ctx$direction; date <- ctx$date
  pa <- ctx$phase_a; pb <- ctx$phase_b; pc <- ctx$phase_c
  pd <- ctx$phase_d; pe <- ctx$phase_e

  # Header strip — neutral facts only (no classification/verdict).
  meta <- sprintf(paste0(
    '<div class="meta-grid">',
    '<div class="cell"><div class="lbl">%s</div><div class="val">$%s</div></div>',
    '<div class="cell"><div class="lbl">%s</div><div class="val">%s</div></div>',
    '<div class="cell"><div class="lbl">%s</div><div class="val">%s</div></div>',
    '<div class="cell"><div class="lbl">%s</div><div class="val">%s</div></div>',
    '</div>'),
    .tt("Spot"), .fmt_num(pb$price),
    .tt("Sector"), pb$sector %||% "n/a",
    .tt("Stage"), pb$stage %||% "n/a",
    "Direction", direction)

  # Coverage strip — one badge per dimension showing data provenance, NOT a
  # verdict. Replaces the old B/C/D/E PASS-SKIP-classification badges.
  badges <- paste0('<div class="badges">',
    paste0(vapply(pe$coverage, function(cv)
      sprintf('<span class="badge %s">%s · %s</span>',
              .status_class(cv$status, "badge"), cv$dimension, cv$status),
      character(1)), collapse = ""),
    '</div>')

  # Phase A — informational only. Never SKIPs downstream phases.
  sec_a <- paste0(
    '<h2>Phase A — Universe Option Liquidity (informational)</h2>',
    .retrieved_caption(`source` = pa$retrieved_at),
    sprintf(paste0(
      '<table><tr><th>Field</th><th>Value</th><th>Note</th></tr>',
      '<tr><td>Expiries available</td><td class="value">%s</td><td class="note">total expiration count from IBKR/cache</td></tr>',
      '<tr><td>Tradeable (14-90 DTE)</td><td class="value">%s</td><td class="note">expirations in the swing window</td></tr>',
      '<tr><td>Source</td><td class="value">%s</td><td class="note">%s</td></tr>',
      '</table>'),
      .fmt_num(pa$n_expiries, 0),
      .fmt_num(pa$tradeable_expiries, 0),
      pa$source %||% "n/a",
      pa$reason %||% "live IBKR probe"),
    .render_phase_a_spread(pa))

  # Phase B — direction-aware trend + sector RS context + collapsible breakdown
  sec_b <- paste0(
    .render_phase_b_context(pb, direction),
    .render_phase_b_breakdown(pb$breakdown))

  # Phase C — cheap score components + funnel grid
  sec_c <- .render_phase_c(pc, direction, ctx$config)

  # Volatility character — spot/vol corr, vol-of-vol, optional VIX put/call skew
  sec_vol_char <- .render_vol_character(pc$vol_character)

  # Phase D
  sec_d <- .render_phase_d(pd, direction)

  # BOT_daily per-name read, next to Phase D so the two target engines sit together
  sec_bot <- .render_bot_read(ctx$bot_read, pb, pd, direction, ctx$config)

  # Phase E — data-coverage summary. One row per dimension
  # reporting provenance (LIVE / CACHED / NO DATA / FETCH FAILED). NO verdict.
  cov_rows <- paste0(vapply(pe$coverage, function(cv)
    sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
            .status_class(cv$status, "row"), cv$dimension, cv$status, cv$detail),
    character(1)), collapse = "")
  sec_e <- paste0(
    '<h2>Data Coverage (per dimension)</h2>',
    '<p class="sub">How much of this report is live vs cached vs unavailable. ',
    'This is provenance, not a verdict — /analyze never ranks or scores a single name.</p>',
    '<table><tr><th>Dimension</th><th>Provenance</th><th>Detail</th></tr>',
    cov_rows, '</table>')

  # Structures section — outrights table (when computed) + spreads table.
  # Vehicle banner sits above both. Both rendered always when data available.
  sec_struct <- .render_structures(pd$structures, ctx$config,
                                   vehicle = pd$vehicle,
                                   vehicle_reason = pd$vehicle_reason,
                                   structures_retrieved_at = pd$structures_retrieved_at,
                                   outrights = pd$outrights,
                                   stock_struct = pd$stock_struct,
                                   direction = direction,
                                   earnings_expiry = pd$earnings_expiry)

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
    sec_a, sec_b, sec_c, sec_vol_char, sec_d, sec_bot, sec_e, sec_struct, sec_summary,
    '<div class="sub" style="margin-top:30px">Generated by reports/analyze/main.R. ',
    'Sources: latest swing_scanner CSV; mydb.db (Prices, option_skew_history, ',
    'option_chain_oi_history, scanner_rich_universe); Tdata helpers; ',
    'tdata_py.compute_spread_risk_reward when TWS reachable.</div>',
    .sortable_script(),
    '</body></html>')

  # Space-form symbols ("BRK B") would put a literal space in the filename;
  # the slug collapses it while the report title keeps the canonical name.
  slug <- gsub("[^A-Za-z0-9]+", "_", ticker)
  out_file <- file.path(out_dir, sprintf("analyze_%s_%s.html",
                                          slug, format(date, "%Y%m%d")))
  writeLines(html, out_file, useBytes = TRUE)
  out_file
}

# ── Phase B trend + sector-RS context (top table) ────────────────────────
#
# Direction-aware rendering of:
#   - stage label (early/continuation/extended/none)
#   - direction alignment (ALIGNED / MISMATCH / n/a)
#   - sector, sector ETF
#   - stock-vs-sector RS at 20d + 60d (leader/laggard within sector)
#   - sector-vs-SPY RS at 20d (strong/weak sector)
#   - sector rank — direction-aware: long ranks descending (rank 1 = strongest),
#     short ranks ascending (rank 1 = weakest). Cutoff = top half of sectors.
.render_phase_b_context <- function(pb, direction) {
  ctx <- pb$sector_context
  hdr <- paste0(
    '<h2>Phase B — Trend &amp; Sector RS Context</h2>',
    .retrieved_caption(`OHLC live` = pb$breakdown_retrieved_at))

  if (is.null(ctx)) {
    return(paste0(hdr,
      '<table>',
      '<tr><th>Field</th><th>Value</th><th>Note</th></tr>',
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
              .row_class(pb$result), .tt("Result"), pb$result,
              "no sector context (live fetch failed)"),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">user direction <code>%s</code></td></tr>',
              .row_class(pb$result), .tt("Direction alignment"),
              pb$direction_match %||% "n/a", direction),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
              .row_class(pb$result), .tt("Stage"), pb$stage %||% "n/a",
              .cluster_note(pb)),
      '</table>'))
  }

  # Rank cutoff: top half of sectors (PASS-eligible)
  rank_cutoff <- if (!is.na(ctx$n_sectors)) as.integer(ceiling(ctx$n_sectors / 2)) else NA_integer_
  rank_note <- if (!is.na(ctx$sector_rank) && !is.na(ctx$n_sectors)) {
    sprintf("%s/%d sectors %s — cutoff &le; %d for %s",
            ctx$sector_rank, ctx$n_sectors,
            if (direction == "long") "(rank 1 = strongest vs SPY)" else "(rank 1 = weakest vs SPY)",
            rank_cutoff, direction)
  } else "rank unavailable"

  .fmt_pct <- function(x) if (is.na(x)) "<span class='crit-fail'>n/a</span>" else
    sprintf("%s%.2f%%", if (x > 0) "+" else "", x)
  .row <- function(label, value, note, tooltip = NULL,
                    row_class = .row_class(pb$result)) {
    lbl <- if (!is.null(tooltip)) .tt(label, tooltip) else label
    sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
            row_class, lbl, value, note)
  }

  rs_sec_20_note <- if (!is.na(ctx$rs_vs_sector_20d)) {
    sign_label <- if (ctx$rs_vs_sector_20d > 0) "leader" else "laggard"
    sprintf("stock %s · ETF %s · sector %s",
            .fmt_pct(ctx$stock_ret20), .fmt_pct(ctx$etf_ret20), sign_label)
  } else "live OHLC unavailable"

  rs_sec_60_note <- if (!is.na(ctx$rs_vs_sector_60d)) {
    sign_label <- if (ctx$rs_vs_sector_60d > 0) "leader" else "laggard"
    sprintf("stock %s · ETF %s · sector %s",
            .fmt_pct(ctx$stock_ret60), .fmt_pct(ctx$etf_ret60), sign_label)
  } else "live OHLC unavailable"

  rs_spy_20_note <- if (!is.na(ctx$sector_rs_vs_spy_20d)) {
    sprintf("ETF %s vs SPY %s",
            .fmt_pct(ctx$etf_ret20), .fmt_pct(ctx$spy_ret20))
  } else "live OHLC unavailable"

  tbl <- paste0(
    '<table>',
    '<tr><th>Field</th><th>Value</th><th>Note</th></tr>',
    .row(.tt("Stage", "Mechanical label from MA50 position + setup/breakout counts. extended = stock >15% above MA50 (long) or <-15% below (short). early = setup count >=4/6 AND breakout >=3/4. continuation = MA50 sloping with you. none = otherwise."),
         pb$stage %||% "n/a",
         .cluster_note(pb)),
    .row(.tt("Direction alignment", "Long ALIGNED iff price > MA50; short ALIGNED iff price < MA50."),
         pb$direction_match %||% "n/a",
         sprintf("user direction <code>%s</code>", direction)),
    .row(.tt("Sector", "GICS sector from ScannerUniverse."),
         ctx$sector %||% "n/a",
         sprintf("ETF: <code>%s</code>", ctx$etf_sym %||% "n/a")),
    .row(.tt("Stock vs Sector ETF (20d)", "Stock 20d return minus sector ETF 20d return. Positive = leader within sector; negative = laggard."),
         .fmt_pct(ctx$rs_vs_sector_20d),
         rs_sec_20_note),
    .row(.tt("Stock vs Sector ETF (60d)", "Stock 60d return minus sector ETF 60d return. Captures slower rotation than 20d."),
         .fmt_pct(ctx$rs_vs_sector_60d),
         rs_sec_60_note),
    .row(.tt("Sector vs SPY (20d)", "Sector ETF 20d return minus SPY 20d return. Positive = strong sector; negative = weak."),
         .fmt_pct(ctx$sector_rs_vs_spy_20d),
         rs_spy_20_note),
    .row(.tt("Sector rank (direction-aware)",
              if (direction == "long")
                "Rank among all sectors by (etf_ret20 - spy_ret20), descending. Rank 1 = strongest sector."
              else
                "Rank among all sectors by (etf_ret20 - spy_ret20), ascending. Rank 1 = weakest sector."),
         if (!is.na(ctx$sector_rank))
           sprintf("%d / %d", ctx$sector_rank, ctx$n_sectors)
         else "n/a",
         rank_note),
    '</table>')

  paste0(hdr, tbl)
}

# Report the three measured clusters rather than one flat count. The nine
# criteria carry ~4.9 effective dimensions: trend/position is six criteria
# voting near-together, while compression (S5) and the supply pair (S6/BK4) are
# independent of it and of each other. "setup 5/6 - breakout 4/4" invites
# reading ten confirmations where there are nearer three.
.cluster_note <- function(pb) {
  if (is.null(pb$trend_count) || is.na(pb$trend_count)) {
    return(sprintf("setup %s/6 &middot; breakout %s/4",
                   .fmt_num(pb$setup_count, 0), .fmt_num(pb$breakout_count, 0)))
  }
  sprintf("trend %s/6 &middot; compression %s/1 &middot; supply %s/2 &nbsp;<span class=\"sub\">(setup %s/6 &middot; breakout %s/4)</span>",
          .fmt_num(pb$trend_count, 0), .fmt_num(pb$compression_count, 0),
          .fmt_num(pb$supply_count, 0),
          .fmt_num(pb$setup_count, 0), .fmt_num(pb$breakout_count, 0))
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

# ── Volatility character section ─────────────────────────────────────────
# Realized-vol behaviour (spot/vol correlation, vol-of-vol) + optional VIX
# put/call skew decomposition. Informational/provenance — never scored.
.render_vol_character <- function(vc) {
  if (is.null(vc)) return("")
  rows <- ""

  if (!is.null(vc$spot_vol)) {
    rows <- paste0(rows, sprintf(
      '<tr><td>Spot/Vol correlation</td><td class="value">%.3f</td><td class="note">%s</td></tr>',
      vc$spot_vol$correlation, vc$spot_vol$interpretation))
  } else {
    rows <- paste0(rows,
      '<tr class="skip"><td>Spot/Vol correlation</td><td class="value">n/a</td><td class="note">unavailable</td></tr>')
  }

  if (!is.null(vc$vov)) {
    ### Vol-of-vol is reported as a percentile of the stored reference basket.
    ### Its absolute level is uninformative - the 10d rolling estimator returns
    ### ~1.94 even when true vol-of-vol is zero - so the raw figure is kept only
    ### as provenance for the percentile above it.
    rows <- paste0(rows,
      sprintf('<tr><td>Vol-of-vol (percentile)</td><td class="value">%s</td><td class="note">%s</td></tr>',
              if (is.na(vc$vov$vov_percentile)) "n/a"
              else sprintf("%d%%", vc$vov$vov_percentile),
              vc$vov$interpretation),
      sprintf('<tr><td>Vol-of-vol (raw)</td><td class="value">%.3f</td><td class="note">uncalibrated - not comparable across tickers</td></tr>',
              vc$vov$vol_of_vol),
      sprintf('<tr><td>Recent vol-of-vol (30d)</td><td class="value">%.3f</td><td class="note">noise-dominated, indicative only</td></tr>',
              vc$vov$recent_vol_of_vol),
      sprintf('<tr><td>Current RV / percentile</td><td class="value">%.1f%% / %d%%</td><td class="note"></td></tr>',
              vc$vov$current_rv, vc$vov$rv_percentile))
  } else {
    rows <- paste0(rows,
      '<tr class="skip"><td>Vol-of-vol</td><td class="value">n/a</td><td class="note">unavailable</td></tr>')
  }

  if (!is.null(vc$vix_skew)) {
    fv <- format_vix_results(vc$vix_skew)
    rows <- paste0(rows, sprintf(
      '<tr><td>VIX (put/call decomposition)</td><td class="value">%.1f</td><td class="note">call %.1f / put %.1f &middot; P/C skew %s &middot; %s</td></tr>',
      vc$vix_skew$VIX, vc$vix_skew$VIX_call_component, vc$vix_skew$VIX_put_component,
      if (!is.na(vc$vix_skew$skew_ratio)) sprintf("%.2f", vc$vix_skew$skew_ratio) else "n/a",
      fv$skew_interpretation))
  } else {
    rows <- paste0(rows, sprintf(
      '<tr class="skip"><td>VIX put/call skew</td><td class="value">&mdash;</td><td class="note">%s</td></tr>',
      vc$skew_status %||% "n/a"))
  }

  paste0(
    '<h2>Volatility character</h2>',
    '<p class="sub">Realized-vol behaviour (Yahoo history): leverage effect &amp; vol stability. ',
    'VIX put/call decomposition is opt-in (<code>--skew</code>, IBKR chains). ',
    'Informational &mdash; not scored.</p>',
    '<table><tr><th>Metric</th><th>Value</th><th>Detail</th></tr>', rows, '</table>')
}

# ── Phase C section ──────────────────────────────────────────────────────
.render_phase_c <- function(pc, direction, config) {
  cm <- pc$components
  rc <- .row_class(pc$result)
  max_score <- pc$cheap_max %||% 9L
  .fmt_band <- function(value, max, band, fmt = "%s") {
    if (is.na(value)) "<span class='crit-fail'>n/a</span>"
    else sprintf("%s pts <span class='crit-info'>(%s)</span>", value, band)
  }
  rows_c1 <- if (!is.null(cm)) {
    paste0(
      sprintf('<tr class="%s"><td><b>%s</b></td><td class="value">%s/%d</td><td class="note">cutoff &ge; 6. PASS if all four components total &ge; 6.</td></tr>',
              rc, .tt("cheap_score",
                       "Composite from IVP (max 4) + VRP (max 2) + Term (max 2) + RR alignment (max 1). Max 9."),
              .fmt_num(pc$cheap_score, 0), max_score),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">implied directional bias from skew · user direction: %s</td></tr>',
              rc, .tt("cheap_side"), pc$cheap_side %||% "n/a", direction),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">IV30=%s · IVP=%s (band %s)</td></tr>',
              rc, .tt("IV30 / IVP component",
                       "Current 30-day implied vol and where it sits in 1y history (IVP). Lower IVP = options cheaper. Scored on IVP only — bands: <=25→4, <=40→3, <=60→2, <=75→1, else 0."),
              sprintf("%d / %d", cm$ivp_pts, cm$ivp_max),
              if (is.null(cm$iv30_value) || is.na(cm$iv30_value)) "n/a"
                else sprintf("%.1f%%", cm$iv30_value * 100),
              if (is.na(cm$ivp_value)) "n/a" else sprintf("%.1f%%", cm$ivp_value),
              cm$ivp_band),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">RV30=%s · RVP=%s</td></tr>',
              rc, .tt("RV30 / RVP (info)",
                       "Current 30-day realized vol and where it sits in 1y history (RVP). Informational — not scored. Compare to IV30/IVP: IV>RV with IVP>RVP means premium is rich vs realized AND vs its own history."),
              "&mdash;",
              if (is.null(cm$rv30_value) || is.na(cm$rv30_value)) "n/a"
                else sprintf("%.1f%%", cm$rv30_value * 100),
              if (is.null(cm$rvp_value) || is.na(cm$rvp_value)) "n/a"
                else sprintf("%.1f%%", cm$rvp_value)),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">VRP_log=%s (band %s)</td></tr>',
              rc, .tt("VRP component",
                       "Vol Risk Premium = log(IV30/RV30)*100. Negative = options cheap vs realised. Bands: <=0→2, <=10→1, else 0."),
              sprintf("%d / %d", cm$vrp_pts, cm$vrp_max),
              if (is.na(cm$vrp_value)) "n/a" else sprintf("%+.1f", cm$vrp_value),
              cm$vrp_band),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">term_pct=%s (band %s)</td></tr>',
              rc, .tt("Term component",
                       "Front-vs-back IV term. Negative = contango (back > front; useful when buying short-dated). Bands: <=-5→2, <=0→1, else 0."),
              sprintf("%d / %d", cm$term_pts, cm$term_max),
              if (is.na(cm$term_value)) "n/a" else sprintf("%+.1f%%", cm$term_value),
              cm$term_band),
      sprintf('<tr class="%s"><td>%s</td><td class="value">%s</td><td class="note">RR_25Δ=%s · %s</td></tr>',
              rc, .tt("RR alignment",
                       "Risk-reversal 25Δ sign aligning with trade direction (long: calls bid; short: puts bid). 1 pt if aligned, 0 otherwise."),
              sprintf("%d / %d", cm$rr_pts, cm$rr_max),
              if (is.na(cm$rr_value)) "n/a" else sprintf("%+.1f vp", cm$rr_value),
              cm$rr_band))
  } else {
    sprintf('<tr class="%s"><td>%s</td><td class="value">n/a</td><td class="note">funnel data unavailable</td></tr>',
            rc, .tt("cheap_score"))
  }

  c1 <- sprintf(paste0(
    '<h3>C.1 Cheap Score (components)</h3>',
    '<table><tr><th>Component</th><th>Points</th><th>Note</th></tr>',
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
  # spot_target_high is intentionally NA when the structural target is a single
  # level corroborated by >=2 sources (e.g. swing high == 52w high) — render it
  # as "—" with a note, distinct from a genuine fetch failure.
  single_target <- !is.na(t$spot_target_low) && is.na(t$spot_target_high)
  high_cell <- if (single_target) "&mdash;" else .fmt_cell(t$spot_target_high, t_reason)
  high_note <- if (single_target) "single corroborated target (see agreeing)" else ""

  # Move position — how far the move has travelled from its swing base toward the
  # nearest target (1a: % of leg), expressed on the Fib ladder (1b: rung + next
  # extension as a forward price). Descriptive only; no flow/scoring side-effects.
  mv_pct <- t$move_pct; mv_base <- t$move_base
  mv_fib <- t$move_fib; mv_next <- t$move_next_ext; mv_lb <- t$move_lookback
  move_val <- if (is.null(mv_pct) || is.na(mv_pct)) .fmt_cell(NA, t_reason)
    else sprintf("%.1f%% of base&rarr;target leg &middot; ~%s rung%s",
                 mv_pct, mv_fib %||% "n/a",
                 if (!is.null(mv_next) && !is.na(mv_next))
                   sprintf(" &middot; next ext %s", mv_next) else "")
  move_note <- if (!is.null(mv_base) && !is.na(mv_base))
    sprintf("base = swing %s %.2f%s", if (direction == "long") "low" else "high", mv_base,
            if (!is.null(mv_lb) && !is.na(mv_lb)) sprintf(" (%dd lookback)", as.integer(mv_lb)) else "")
    else ""

  targets_html <- paste0(src_caption, sprintf(paste0(
    '<h3>Structural target sources</h3>',
    '<table><tr><th>Field</th><th>Value</th><th>Note</th></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note"></td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">cutoff &ge; 2</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">overlay only</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    '<tr><td>%s</td><td class="value">%s</td><td class="note">%s</td></tr>',
    '</table>'),
    .tt("Target &mdash; near", override = .TOOLTIPS[["spot_target_low"]]),
      .fmt_cell(t$spot_target_low, t_reason),
    .tt("Target &mdash; far", override = .TOOLTIPS[["spot_target_high"]]),
      high_cell, high_note,
    .tt("targets_agreeing"), .fmt_cell(t$targets_agreeing, t_reason, digits = 0),
    .tt("fib_confirms"),
      if (is.na(t$fib_confirms)) (.fmt_cell(NA, t_reason)) else as.character(t$fib_confirms),
    .tt("Move position", override = .TOOLTIPS[["move_position"]]),
      move_val, move_note,
    .tt("expiry"), .fmt_cell(pd$expiry, pd$expiry_reason),
    if (!is.null(pd$expiry_reason)) "live-picked from IBKR" else "from scanner CSV"))

  c_reason <- pd$chain_reason
  e_reason <- pd$entry_reason
  cs <- pd$chain_status_prov %||% "FETCH FAILED"
  es <- pd$entry_status_prov %||% "FETCH FAILED"
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
    .tt("oi_cap_call"), .fmt_cell(pd$oi_cap_call, c_reason, status = cs),
    .tt("oi_cap_put"),  .fmt_cell(pd$oi_cap_put,  c_reason, status = cs),
    .tt("chain_state"), .fmt_cell(pd$chain_state, c_reason, status = cs),
    .tt("effective_target"), .fmt_cell(pd$effective_target, e_reason, status = es),
    .tt("R:R"), .fmt_cell(pd$rr, e_reason, status = es),
    .tt("entry_floor / entry_ceiling"),
      .fmt_cell(pd$entry_floor, e_reason, status = es),
      .fmt_cell(pd$entry_ceiling, e_reason, status = es),
    .tt("headroom_band"), .fmt_cell(pd$headroom_band, e_reason, status = es),
    .tt("entry_state"), .fmt_cell(pd$entry_state, e_reason, status = es))

  paste0('<h2>Phase D — Setup, Chain, R:R</h2>', targets_html, chain_html)
}

# ── Structures table ─────────────────────────────────────────────────────
#
# Vehicle rule (shared/vehicle_rule.R) selects ONE applicable vehicle.
# We render the matching section open by default; the others (if data is
# available) collapse to <details> the user can expand on demand.
.render_structures <- function(structures, config, vehicle = "spread",
                               vehicle_reason = NULL,
                               structures_retrieved_at = NULL,
                               outrights = NULL,
                               stock_struct = NULL,
                               direction = "long",
                               earnings_expiry = NULL) {
  cap <- config$risk_cap_lot_usd

  vehicle_banner <- sprintf(
    '<h2>Recommended vehicle: <code>%s</code></h2>',
    vehicle %||% "n/a")
  if (!is.null(vehicle_reason) && nzchar(vehicle_reason)) {
    vehicle_banner <- paste0(vehicle_banner,
      sprintf('<p class="sub">Per shared vehicle rule: %s</p>',
              vehicle_reason))
  }

  # Earnings-inside-the-hold warning (structure-relative; see
  # .earnings_vs_expiries). Renders only when a proposed expiry carries the print.
  if (!is.null(earnings_expiry) && isTRUE(earnings_expiry$any_inside) &&
      !is.null(earnings_expiry$message)) {
    vehicle_banner <- paste0(vehicle_banner,
      sprintf('<p class="warn-box">&#9888; %s</p>',
              earnings_expiry$message))
  }

  retrieved_html <- .retrieved_caption(`live pricer` = structures_retrieved_at)

  outright_table <- .render_outright_table(outrights, direction)
  spread_table   <- .render_structures_table(structures, cap)
  stock_table    <- .render_stock_table(stock_struct, direction)

  # The vehicle-preferred table renders OPEN; the others collapse.
  # The vehicle rule is informational, not gating.
  open_outright <- vehicle %in% c("call", "put")
  open_stock    <- identical(vehicle, "stock")
  open_spread   <- identical(vehicle, "spread")

  hint <- if (open_spread) ""
          else if (open_outright)
            sprintf('<p class="sub"><b>Outright %s</b> preferred by vehicle rule (cheap IV). Spreads/stock shown below for reference.</p>',
                    vehicle)
          else if (open_stock)
            '<p class="sub"><b>Stock</b> preferred by vehicle rule (price &lt; $10 or option spread too wide). Spreads shown below for reference.</p>'
          else ""

  .wrap <- function(open, summary, body) {
    if (!nzchar(body)) return("")
    sprintf('<details%s><summary>%s — click to toggle</summary>%s</details>',
            if (open) " open" else "", summary, body)
  }

  right_label <- if (identical(direction, "short")) "put" else "call"
  outright_wrapped <- .wrap(open_outright,
    sprintf("Outright %s grid — strike × expiry (single-leg long-option pricing)",
            right_label),
    outright_table)
  stock_wrapped <- .wrap(open_stock, "Stock-only structure (R:R off stop band)",
                         stock_table)
  spread_wrapped <- .wrap(open_spread,
    sprintf("Vertical spreads — DEBIT only, within $%d/lot cap", cap),
    paste0(retrieved_html, spread_table))

  paste0(vehicle_banner, hint, outright_wrapped, stock_wrapped, spread_wrapped)
}

#' Render the stock-only structure as a one-row table. Returns "" when NULL.
.render_stock_table <- function(stock_struct, direction) {
  if (is.null(stock_struct) || nrow(stock_struct) == 0) return("")
  r <- stock_struct[1, , drop = FALSE]
  fmt <- function(v) if (is.null(v) || is.na(v)) "&mdash;" else sprintf("$%.2f", v)
  rr  <- if (is.na(r$rr)) "&mdash;" else sprintf("%.2f", r$rr)
  paste0(
    '<table><tr><th>Entry</th><th>Stop</th><th>Target (near/far)</th>',
    '<th>Risk/sh</th><th>Reward/sh</th><th>R:R</th></tr>',
    sprintf(paste0('<tr class="row-pass"><td>%s</td><td>%s</td>',
                   '<td>%s / %s</td><td>%s</td><td>%s</td><td class="value">%s</td></tr>'),
            fmt(r$entry), fmt(r$stop), fmt(r$target_low), fmt(r$target_high),
            fmt(r$risk_per_share), fmt(r$reward_per_share), rr),
    '</table>',
    sprintf('<p class="sub">Stock entry at spot; mechanical stop %s of spot; ',
            "5%"),
    sprintf('reward to the structural target (%s tail). R:R = reward/risk per share.</p>',
            direction))
}

#' Render the outright-option strike × expiry grid. Returns "" when outrights
#' is NULL/empty (e.g. TWS down, or vehicle = stock).
.render_outright_table <- function(outrights, direction) {
  if (is.null(outrights) || nrow(outrights) == 0) return("")
  right_label <- if (identical(direction, "short")) "Put" else "Call"

  fmt_money <- function(v) if (is.na(v)) "&mdash;" else sprintf("$%.2f", v)
  fmt_strike <- function(v) if (is.na(v)) "&mdash;" else sprintf("$%g", v)
  fmt_ratio <- function(v) if (is.na(v)) "&mdash;" else sprintf("%.2f", v)
  fmt_expiry <- function(v, d) if (is.na(v)) "&mdash;"
                                else sprintf("%s <span class='crit-info'>(%dd)</span>",
                                             as.character(as.Date(as.character(v), format="%Y%m%d")), d)

  hdr_cells <- c(
    '<th><span title="Expiration date and DTE.">Expiry</span></th>',
    sprintf('<th><span title="Strike of the long %s.">Strike</span></th>', tolower(right_label)),
    '<th><span title="Black-Scholes premium at current spot, current IV30. Per lot (×100). Equal to maximum loss for an outright long option.">Entry premium</span></th>',
    '<th><span title="Black-Scholes premium at the effective target (DTE − 5d theta buffer, IV bumped +2pp).">Fwd @ target</span></th>',
    '<th><span title="Fwd premium − entry premium per lot.">Reward</span></th>',
    '<th><span title="Reward / risk on the move to effective target.">R:R</span></th>',
    '<th><span title="BOT outright test: premium within the per-lot budget and payoff above 2:1 at the target after the hold&apos;s decay.">Accept</span></th>')

  fmt_accept <- function(r) {
    a <- r$accept
    if (is.null(a) || length(a) == 0 || is.na(a)) "&mdash;"
    else if (identical(a, "ACCEPT")) "ACCEPT"
    else sprintf("REJECT (%s)", r$reject_reason %||% "")
  }

  rows <- vapply(seq_len(nrow(outrights)), function(i) {
    r <- outrights[i, , drop = FALSE]
    cells <- c(
      sprintf('<td>%s</td>', fmt_expiry(r$expiry, r$dte)),
      sprintf('<td>%s</td>', fmt_strike(r$strike)),
      sprintf('<td>%s</td>', fmt_money(r$entry_premium)),
      sprintf('<td>%s</td>', fmt_money(r$fwd_premium)),
      sprintf('<td>%s</td>', fmt_money(r$reward)),
      sprintf('<td>%s</td>', fmt_ratio(r$rr)),
      sprintf('<td>%s</td>', fmt_accept(r)))
    sprintf('<tr class="row-pass">%s</tr>', paste0(cells, collapse = ""))
  }, character(1))

  # h3 removed — caller wraps in <details>/<summary>.
  paste0(
    '<table class="sortable"><thead><tr>',
    paste0(hdr_cells, collapse = ""),
    '</tr></thead><tbody>',
    paste(rows, collapse = "\n"),
    '</tbody></table>',
    '<p class="sub">Strike × expiry grid. Entry priced at current spot/IV30. ',
    'Forward priced at effective target with theta buffer (DTE − 5d) and IV +2pp. ',
    'ACCEPT first, then R:R desc.</p>')
}

#' Inner spread enumeration table (no h2, no banner).
#' Step 4 rewrite 2026-05-12: DEBIT-only, within-cap-only rows arrive
#' pre-filtered from enumerate_structures. spread_type column dropped.
#' Currency amounts formatted with $ suffix, probabilities with % suffix.
#' Rounded to 2 decimals.
.render_structures_table <- function(structures, cap) {
  if (is.null(structures) || nrow(structures) == 0) {
    return(paste0('<p class="sub">No DEBIT spreads enumerated within $', cap,
                  ' per lot. (Live pricer unavailable, or no qualifying ',
                  'structures after the filter.)</p>'))
  }
  # If an unavailable-structures placeholder row sneaks through (FETCH FAILED
  # or NO DATA), render its surface_fact rather than an empty table.
  ph <- !is.na(structures$structure) &
        structures$structure %in% c("FETCH FAILED", "NO DATA")
  if (any(ph)) {
    msgs <- structures$surface_fact[ph]
    return(paste0('<p class="sub">', paste(msgs, collapse = "<br>"), '</p>'))
  }

  fmt_money <- function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) "&mdash;"
    else sprintf("$%.2f", as.numeric(v))
  }
  fmt_strike <- function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) "&mdash;"
    else sprintf("$%g", as.numeric(v))
  }
  fmt_pct <- function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) "&mdash;"
    else sprintf("%.1f%%", as.numeric(v) * 100)
  }
  fmt_ratio <- function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v)) "&mdash;"
    else sprintf("%.2f", as.numeric(v))
  }
  fmt_expiry <- function(v) {
    if (is.null(v) || length(v) == 0 || is.na(v) || !nzchar(v)) "&mdash;"
    else {
      dt <- tryCatch(as.Date(as.character(v), format = "%Y%m%d"),
                     error = function(e) NA)
      if (inherits(dt, "Date") && !is.na(dt)) {
        dte <- as.integer(dt - Sys.Date())
        sprintf("%s <span class='crit-info'>(%dd)</span>", as.character(dt), dte)
      } else as.character(v)
    }
  }

  has <- function(cc) cc %in% names(structures)

  # Column spec: (header, value-getter, tooltip)
  col_spec <- list(
    list("Expiry",       function(r) fmt_expiry(r$expiry),
         "Expiration date (and DTE)."),
    list("Long strike",  function(r) fmt_strike(r$long_strike),
         "Strike of the long leg (the leg you buy)."),
    list("Short strike", function(r) fmt_strike(r$short_strike),
         "Strike of the short leg (the leg you sell)."),
    list("Width",        function(r) fmt_money(r$width),
         "Difference between the two strikes ($ value)."),
    list("Debit",        function(r) fmt_money(abs(r$net_premium %||% NA)),
         "Net premium paid up-front. Also equals maximum loss per lot."),
    list("Max reward",   function(r) fmt_money(r$max_reward),
         "Maximum dollar gain per lot (width × 100 − debit)."),
    list("R:R",          function(r) fmt_ratio(r$reward_risk_ratio),
         "max_reward / debit."),
    # fmt_pct() takes a FRACTION and multiplies by 100; debit_pct_width is
    # already a percentage per the spec, so it is scaled back here.
    list("Debit % width", function(r) fmt_pct(r$debit_pct_width / 100),
         "debit / (width x multiplier). 33% or less is the 2:1 payoff the BOT profile needs."),
    list("Accept",       function(r) {
           a <- r$accept %||% NA
           if (is.na(a)) "n/a"
           else if (identical(a, "ACCEPT")) "ACCEPT"
           else sprintf("REJECT (%s)", r$reject_reason %||% "")
         },
         "BOT per-vehicle test: debit at most 33% of width, and breakeven not past the structural target."),
    list("P(success)",   function(r) fmt_pct(r$prob_success_delta),
         "Probability the spread expires fully ITM (from delta)."),
    list("Edge",         function(r) fmt_pct(r$edge),
         "Excess probability vs market-implied (edge in probability-points)."),
    list("EV",           function(r) fmt_money(r$expected_value),
         "Expected dollar value per lot. Reported, not the sort key.")
  )

  hdr <- paste0(vapply(col_spec, function(cs) {
    sprintf('<th><span title="%s">%s</span></th>',
            gsub('"', '&quot;', cs[[3]], fixed = TRUE), cs[[1]])
  }, character(1)), collapse = "")

  rows <- vapply(seq_len(nrow(structures)), function(i) {
    r <- structures[i, , drop = FALSE]
    cells <- paste0(vapply(col_spec, function(cs) {
      sprintf('<td>%s</td>', cs[[2]](r))
    }, character(1)), collapse = "")
    sprintf('<tr class="row-pass">%s</tr>', cells)
  }, character(1))

  paste0('<table class="sortable"><thead><tr>', hdr, '</tr></thead><tbody>',
         paste(rows, collapse = "\n"), '</tbody></table>',
         sprintf(paste0('<p class="sub">DEBIT spreads within $%d-per-lot cap, ',
                        'ACCEPT first, then cheapest share of width ',
                        '(payoff per dollar). Expected Value is reported, not ',
                        'the sort key. Click any column header to re-sort. ',
                        'CREDIT spreads and phantom/zero-priced legs filtered out.</p>'),
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

  ctx <- pb$sector_context
  rs_sec_str <- if (!is.null(ctx) && !is.na(ctx$rs_vs_sector_20d))
                  sprintf("%+.2f%% (20d) / %s (60d)",
                          ctx$rs_vs_sector_20d,
                          if (!is.na(ctx$rs_vs_sector_60d))
                            sprintf("%+.2f%%", ctx$rs_vs_sector_60d) else "n/a")
                else "n/a"
  rows <- list(
    c("stage / direction alignment",
      sprintf("%s / %s", pb$stage %||% "n/a", pb$direction_match %||% "n/a")),
    c("sector_rs_rank",
      if (!is.null(ctx) && !is.na(ctx$sector_rank))
        sprintf("%d/%d", ctx$sector_rank, ctx$n_sectors)
      else "n/a"),
    c("stock vs sector RS", rs_sec_str),
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
        sprintf("%s (%dd)%s", as.character(fn$earnings_date), fn$earnings_dte,
                if (!is.null(pd$earnings_expiry) &&
                    isTRUE(pd$earnings_expiry$any_inside))
                  " &#9888; inside proposed expiry" else "")
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

# ── BOT_daily per-name read ──────────────────────────────────────────────
#
# Only what BOT_daily alone provides: the gate cluster states are already in
# the Phase B breakdown (same eval_gates()), so they are not repeated here.
# Level labels are on the trade's axis ("ahead" = toward the target), which is
# how bot_read_row() emits them for both directions.
.render_bot_read <- function(br, pb, pd, direction, config) {
  head <- '<h2>BOT_daily &mdash; per-name read</h2>'
  if (is.null(br)) return("")
  if (is.null(br$row))
    return(paste0(head, sprintf('<p class="sub">%s: %s</p>', br$status, br$reason %||% "")))
  r <- br$row
  spot <- suppressWarnings(as.numeric(pb$price))
  f <- function(x, d = 2) .fmt_num(x, d)
  band <- function(lo, hi) if (is.na(lo) || is.na(hi)) "n/a" else sprintf("%.2f &ndash; %.2f", lo, hi)
  vs_spot <- function(lvl) if (is.null(lvl) || is.na(lvl) || !isTRUE(spot > 0)) ""
                           else sprintf("%+.1f%%", (lvl / spot - 1) * 100)

  bar_vs <- if (isTRUE(spot > 0))
    sprintf(" &middot; /analyze spot %.2f (bar %+.2f%%)", spot, (r$px / spot - 1) * 100) else ""
  sub <- sprintf(paste0('<p class="sub">shared/bot_read.R &mdash; the row bot_daily_&lt;date&gt;.csv ',
                        'carries for this name. Daily bar %s &middot; px %.2f%s</p>'),
                 r$date, r$px, bar_vs)
  stale <- if (isTRUE(r$bar_lag > 0))
    sprintf(paste0('<div class="warn-box">The daily bar is %d weekday(s) behind today ',
                   '(holidays count). Every level below is read at that bar, not at the live spot.</div>'),
            r$bar_lag) else ""
  # The two engines are priced at different points: bot_read_row() at the Yahoo
  # daily bar, Phase D at the /analyze spot, which falls back to the Prices
  # table when TWS is down (XOP, 2026-09-24: spot 167.82 vs bar 182.65).
  gap <- if (isTRUE(spot > 0)) abs(r$px / spot - 1) * 100 else NA_real_
  mixed <- if (isTRUE(gap > 2))
    sprintf(paste0('<div class="warn-box">The daily bar (%.2f) and the /analyze spot (%.2f) ',
                   'differ by %.1f%%%s. Phase D&rsquo;s targets are computed from that spot, ',
                   'BOT_daily&rsquo;s from the bar, so the comparison below mixes two prices.</div>'),
            r$px, spot, gap,
            if (!isTRUE(config$tws_reachable)) " &mdash; TWS unreachable, the spot is the Prices table&rsquo;s last value" else "")
    else ""
  note <- paste0('<div class="warn-box">Zones describe the chart; they show no measured reaction ',
                 'edge over a random level (BOT spec &sect;3.3, TODO 94). ',
                 '<code>zone_state</code> is informational and no longer vetoes.</div>')

  entry <- sprintf(paste0(
    '<h3>Entry</h3><table><tr><th>Field</th><th>Value</th><th>Note</th></tr>',
    '<tr><td>tradable</td><td class="value">%s</td><td class="note">only gap_through_stop vetoes</td></tr>',
    '<tr><td>veto_reason</td><td class="value">%s</td><td class="note"></td></tr>',
    '<tr><td>zone_state</td><td class="value">%s</td><td class="note">spot inside a zone &mdash; informational</td></tr>',
    '<tr><td>gap_vs_stop</td><td class="value">%s</td><td class="note">p95 overnight move %s%% of px, as a share of the stop distance</td></tr>',
    '<tr><td>entry_factors</td><td class="value">%s</td><td class="note">ATR percentile %s &middot; prior 20 sessions %s ATR</td></tr>',
    '<tr><td>confluence</td><td class="value">%s</td><td class="note">daily vs weekly agreement on S5 / S6 / BK4</td></tr>',
    '<tr><td>rs_state</td><td class="value">%s</td><td class="note">%s</td></tr>',
    '</table>'),
    r$tradable, if (nzchar(r$veto_reason)) r$veto_reason else "&mdash;",
    if (nzchar(r$zone_state)) r$zone_state else "&mdash;",
    f(r$gap_vs_stop), f(r$gap_p95_pct), r$entry_factors, f(r$atr_pctile, 0), f(r$prior20_atr),
    r$confluence, r$rs_state,
    if (identical(r$rs_state, "n/a")) "no benchmark wired (TODO 93.4)" else "S3 vs benchmark")

  row <- function(lab, lvl, dist, pem, touch, last)
    sprintf('<tr><td>%s</td><td class="value">%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
            lab, lvl, dist, pem, touch, last)
  levels <- paste0(
    sprintf('<h3>Levels on the trade&rsquo;s axis (%s: ahead = %s)</h3>', direction,
            if (direction == "short") "below" else "above"),
    '<table><tr><th>Level</th><th>Price</th><th>Dist (ATR, from bar px)</th><th>% of EM10</th><th>Touches</th><th>Last</th></tr>',
    row("zone ahead", band(r$res_zone_lo, r$res_zone_hi), f(r$res_dist_atr), f(r$res_pct_of_em10, 0),
        f(r$res_touches, 0), f(r$res_last)),
    row("next zone", band(r$res2_zone_lo, r$res2_zone_hi), "", "", f(r$res2_touches, 0), ""),
    row("zones to fib 1.272", f(r$n_res_to_ext, 0), sprintf("ext %s", f(r$fib_ext_1272)), "", "", ""),
    row("zone behind", band(r$sup_zone_lo, r$sup_zone_hi), f(r$sup_dist_atr), f(r$sup_pct_of_em10, 0),
        f(r$sup_touches, 0), f(r$sup_last)),
    row("target", sprintf("%s (%s)", f(r$target), r$target_source),
        sprintf("asym %s", f(r$asym)), sprintf("basis %s", r$level_basis), "", ""),
    row("stop", sprintf("%s (%s)", f(r$stop), r$stop_source), "", "", "", ""),
    '</table>')

  # Second BOT level: the next zone's near edge, else the next Fibonacci rung.
  long <- direction != "short"
  nxt <- if (!is.na(r$res2_zone_lo)) (if (long) r$res2_zone_lo else r$res2_zone_hi) else
    if (identical(r$target_source, "fib_ext")) r$fib_ext_1618 else r$fib_ext_1272
  nxt_src <- if (!is.na(r$res2_zone_lo)) "next zone" else
    if (identical(r$target_source, "fib_ext")) "fib 1.618" else "fib 1.272"
  t <- pd$targets %||% list()
  stop_pct <- as.numeric(config$stock_stop_pct %||% 0.05) * 100
  cmp <- sprintf(paste0(
    '<h3>Targets compared</h3>',
    '<p class="sub">Distances from the /analyze spot for both engines. Which one to keep is TODO 93.6.</p>',
    '<table><tr><th></th><th>BOT_daily (zones / Fibonacci)</th><th>Phase D (structural)</th></tr>',
    '<tr><td>target</td><td class="value">%s %s &nbsp;%s</td><td class="value">%s &nbsp;%s</td></tr>',
    '<tr><td>next level</td><td class="value">%s %s &nbsp;%s</td><td class="value">%s &nbsp;%s</td></tr>',
    '<tr><td>stop</td><td class="value">%s %s &nbsp;%s</td><td class="note">none &mdash; the stock row uses a fixed %.0f%% parameter</td></tr>',
    '</table>'),
    f(r$target), r$target_source, vs_spot(r$target),
    f(t$spot_target_low), vs_spot(t$spot_target_low),
    f(nxt), nxt_src, vs_spot(nxt),
    f(t$spot_target_high), vs_spot(t$spot_target_high),
    f(r$stop), r$stop_source, vs_spot(r$stop), stop_pct)

  all_rows <- paste0(vapply(names(r), function(k)
    sprintf('<tr><td>%s</td><td class="value">%s</td></tr>', k,
            if (is.null(r[[k]]) || all(is.na(r[[k]]))) "n/a" else as.character(r[[k]])),
    character(1)), collapse = "")
  all_tbl <- sprintf('<details><summary>All %d fields</summary><table><tr><th>Field</th><th>Value</th></tr>%s</table></details>',
                     length(r), all_rows)

  paste0(head, sub, stale, mixed, note, entry, levels, cmp, all_tbl)
}
