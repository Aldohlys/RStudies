# methodology.R — "Methodology" tab: every computation in the report, built from the live config
#
# Scenario fingerprints (archetypes.R), regime signals and weights (scenarios.R), mismatch rules
# (analyze.R) and thresholds are read from the objects the report uses, so this tab cannot drift
# from the code. Zone thresholds of sections 01-04 live in render_html.R tooltips and are restated here.

md_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

md_table <- function(head, rows) {
  th <- paste0("<tr class='im-head'>", paste0("<td>", head, "</td>", collapse = ""), "</tr>")
  tr <- vapply(rows, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>"), "")
  paste0("<div class='im-tw'><table>", th, paste(tr, collapse = ""), "</table></div>")
}

md_section <- function(num, title, body, open = FALSE) {
  sprintf("<details class='section' style='margin-bottom:16px'%s><summary class='stitle'><span class='snum'>%s</span>%s</summary><div class='im-body md-body'>%s</div></details>",
          if (open) " open" else "", num, title, body)
}

# ── Scenario match (section 00) ─────────────────────────────────────────────
md_scenario_method <- function() {
  paste0(
    "<p>Each scenario is a <b>fingerprint</b>: the direction a set of assets typically takes when that scenario plays out, ",
    "with a weight per asset (2 = core tell, 1 = usual, 0.5 = secondary; the sign gives the expected direction).</p>",
    "<p><b>Step 1, move of each asset.</b> z = (21-day change) / (standard deviation of daily changes over the last 120 days &times; &radic;21). ",
    "Changes are log returns for prices and ratios, and differences in percentage points for yields (^TNX) and the 10y&minus;3m curve. ",
    "The z-score puts assets on a common scale: a move equal to the asset's usual one-month move scores about 1, whatever its volatility. ",
    "Some quotes are inverted so that up always means the same thing (e.g. USD/JPY falling = yen up; Bund and gilt ETF prices falling = yields up). ",
    "A fingerprint asset with several symbols (EM currencies) uses the mean of their z-scores.</p>",
    "<p><b>Step 2, cap.</b> Each z is divided by 1.5 and capped to [&minus;1, +1], so one extreme asset cannot dominate.</p>",
    "<p><b>Step 3, match score.</b> score = &Sigma; (weight &times; capped z) / &Sigma; |weight|, over the fingerprint assets with data. ",
    "Range &minus;100% (every asset moving opposite to the fingerprint) to +100% (every asset moving at least 1.5 z in the expected direction). ",
    "The sign is agreement, not a probability and not good or bad news. Scores are not exclusive: several scenarios can be positive together when they share assets.</p>",
    "<p><b>3-month score.</b> Same computation on 63-day changes (&radic;63 scaling). 1-month above 3-month = emerging; below = fading.</p>",
    sprintf("<p><b>In place and age.</b> A scenario is in place when its 1-month score is &ge; %.0f%%. Scores are recomputed for each of the last %d trading days from price history (same fingerprints). Labels:</p>",
            100 * SCEN_ACTIVE, HIST_DAYS),
    md_table(c("Label", "Rule"), list(
      c("NEW", "In place today, in place in at most one of the previous five reports"),
      c("BUILDING", "In place for fewer than 15 consecutive days, score &ge; mean of the previous five"),
      c("WAVERING", "In place for fewer than 15 consecutive days, score below the mean of the previous five"),
      c("ESTABLISHED", "In place 15&ndash;39 consecutive days (reinforcing if the score is above its level five days ago)"),
      c("MATURE", "In place 40 consecutive days or more"),
      c("FADED", "Not in place today but in place in one of the previous five reports"),
      c("INACTIVE", "None of the above"))),
    "<p><b>Agree / contradict lists.</b> An asset agrees when sign(weight) &times; z &ge; 0.5 and contradicts when it is &le; &minus;0.5.</p>",
    sprintf("<p><b>Chains.</b> A chain is a sequence of fingerprint assets that must each be clearly up (z &ge; %g). Status: firing (all links up), triggered (first link up), ",
            CHAIN_ON),
    "not triggered (later links up without the first, i.e. for another reason).</p>",
    "<p><b>Provenance.</b> Fingerprints, weights, the 1.5 cap and the in-place threshold were set by judgment when the layer was built (2026-10-02), ",
    "from the historical episodes listed under each scenario. They have not been calibrated or backtested.</p>"
  )
}

md_fp_assets <- function() {
  rows <- lapply(names(FP_ASSETS), function(k) {
    a <- FP_ASSETS[[k]]
    sy <- paste(a[[1]], collapse = ", ")
    kind <- if (any(a[[1]] %in% c("^TNX", "^TNX-^IRX"))) "yield change (pp)" else "log return"
    read <- if (a[[2]] < 0) "inverted (quote down = asset up)" else "as quoted"
    c(md_esc(a[[3]]), k, md_esc(sy), kind, read)
  })
  md_table(c("Asset", "Key", "Symbol(s)", "Move", "Reading"), rows)
}

md_scenarios <- function() {
  paste(vapply(ARCHETYPES, function(a) {
    w <- a$fp[order(-abs(a$fp))]
    rows <- lapply(names(w), function(k) c(md_esc(fp_label(k)), sprintf("%+g", w[[k]]), if (w[[k]] > 0) "up" else "down"))
    chain <- if (!is.null(a$chain)) sprintf("<p><b>Chain &mdash; %s:</b> %s. %s</p>", md_esc(a$chain$name),
      paste(vapply(a$chain$steps, fp_label, ""), collapse = " &rarr; "), md_esc(a$chain$text)) else ""
    sprintf("<details class='im-panel'><summary class='im-h2'>%s</summary>%s<p><b>Movie.</b> %s</p><p><b>Past episodes.</b> %s</p><p><b>Not this scenario if.</b> %s</p><p><b>Tells (not scored).</b> %s</p>%s<p><b>BOT.</b> %s</p></details>",
            md_esc(a$name),
            md_table(c("Fingerprint asset", "Weight", "Expected"), rows),
            md_esc(a$movie), md_esc(a$analogs), md_esc(a$invalid), md_esc(a$tells), chain, md_esc(a$bot$note))
  }, ""), collapse = "")
}

# ── Regime model (sections 06-07) ───────────────────────────────────────────
md_regimes <- function() {
  sig_rows <- lapply(names(SIGNAL_TOOLTIPS), function(s) {
    p <- strsplit(SIGNAL_TOOLTIPS[[s]], " | ", fixed = TRUE)[[1]]
    c(s, md_esc(p[1]), md_esc(if (length(p) > 1) p[2] else ""), md_esc(if (length(p) > 2) paste(p[-(1:2)], collapse = "; ") else ""))
  })
  regs <- Filter(function(r) length(REGIME_WEIGHTS[[r]]$weights) > 0, names(REGIME_WEIGHTS))
  sigs <- unique(unlist(lapply(regs, function(r) names(REGIME_WEIGHTS[[r]]$weights))))
  w_rows <- lapply(sigs, function(s) c(s, vapply(regs, function(r) {
    v <- REGIME_WEIGHTS[[r]]$weights[[s]]; if (is.null(v)) "" else sprintf("%+.2f", v) }, "")))
  paste0(
    "<p>Three regimes from continuous signals. Each signal maps a market measure to [0, 1] with a sigmoid ",
    "sig(x, center, scale) = 1 / (1 + e<sup>&minus;(x &minus; center)/scale</sup>): 0.5 at the center, about 0.12 / 0.88 at center &plusmn; 2 scales. ",
    "Centers and scales are 10-year medians and standard deviations (calibrate_from_history.R, 2026-03-26), except breadth (manual).</p>",
    md_table(c("Signal", "Measure", "Formula", "Reading"), sig_rows),
    "<p>credit_stress uses HYG's own 20-day price return, which also falls when Treasury yields rise; it is not a spread measure. ",
    "The spread view is the High yield / Treasuries ratio in panel 09.</p>",
    "<p><b>Raw score</b> of a regime = &Sigma; weight &times; signal. Weights (set by hand, see comments in scenarios.R):</p>",
    md_table(c("Signal", vapply(regs, function(r) REGIME_WEIGHTS[[r]]$label, "")), w_rows),
    "<p><b>Neutral</b> = 0.5 &times; (1 &minus; max(other raw scores) / 0.5) + 0.3 &times; vix_calm + 0.2 &times; breadth_mid, ",
    "where breadth_mid = 1 &minus; |breadth_bull &minus; 0.5| / 0.5 (peaks when 50% of stocks are above their MA50).</p>",
    "<p><b>Modifiers</b>, applied in this order:</p>",
    md_table(c("Modifier", "Rule"), list(
      c("Positioning stress", "crowding = 0.5 &times; share of COT markets at a 5-year extreme (net percentile &ge; 90 or &le; 10). If crowding &gt; 0.3, Liquidity Stress raw score + 0.10 &times; crowding"),
      c("Event catalyst", "Second-ranked regime + boost from the nearest event within 5 days: CRITICAL +0.15 (&le; 2 days) / +0.08; HIGH +0.08 / +0.04; MODERATE +0.04 (&le; 1 day). Events with boost = FALSE are ignored"),
      c("Inertia", sprintf("Previous run's dominant regime + %.2f", REGIME_INERTIA)),
      c("Probabilities", sprintf("softmax(raw scores / %.1f); lower temperature = more decisive", REGIME_TEMPERATURE)))),
    sprintf("<p><b>Daily Bias.</b> The dominant regime sets the bias only when its probability is &ge; %d%% and &ge; %d points above the second; ",
            BIAS_MIN_PROB, BIAS_MIN_MARGIN),
    "Liquidity Stress &rarr; DEFENSIVE, Directional Flow &rarr; LONG BIAS, otherwise NEUTRAL. ",
    "The For / Against drivers rank the dominant regime's signals by pull = weight &times; (signal &minus; 0.5).</p>"
  )
}

# ── Mismatches (section 05) ─────────────────────────────────────────────────
md_mismatches <- function() {
  flags <- list(
    c("vix_stress", "VIX &gt; 25"), c("backwardation", "VIX9D &gt; VIX3M"), c("rates_high", "10Y &gt; 4.5%"),
    c("curve_inv", "10Y &minus; 2Y &lt; 0"), c("dxy_strong", "DXY 20-day return &gt; +2%"), c("dxy_weak", "DXY 20-day return &lt; &minus;2%"),
    c("oil_surging", "USO 20-day return &gt; +10%"), c("gold_rising", "GLD 20-day return &gt; +5%"),
    c("s5fi_bear", "breadth &lt; 35%"), c("s5fi_bull", "breadth &gt; 50%"))
  rule_rows <- lapply(names(SECTOR_RULES), function(s) c(s, SECTOR_ETFS_MAP[[s]],
    paste(SECTOR_RULES[[s]]$tw, collapse = ", "), paste(SECTOR_RULES[[s]]$hw, collapse = ", ")))
  paste0(
    "<p>Macro flags (true / false):</p>", md_table(c("Flag", "Condition"), flags),
    "<p>Each sector ETF has tailwind and headwind flags:</p>",
    md_table(c("Sector", "ETF", "Tailwinds", "Headwinds"), rule_rows),
    "<p>ETF trend: UP = close above MA20 and MA20 rising over 5 days; DOWN = mirror; else FLAT. RS = 20-day return minus SPY's. Classification (first match):</p>",
    md_table(c("Type", "Rule"), list(
      c("UNUSUAL STRENGTH", "UP, &ge; 2 headwinds, 0 tailwinds"),
      c("FRAGILE RALLY", "UP, &ge; 2 headwinds, &ge; 1 tailwind"),
      c("UNUSUAL WEAKNESS", "DOWN, &ge; 2 tailwinds, 0 headwinds"),
      c("LAGGING vs MACRO", "FLAT, &ge; 2 tailwinds, 0 headwinds, RS &lt; &minus;3%"),
      c("CONFIRMED SHORT", "DOWN, &ge; 2 headwinds, RS &lt; &minus;4%"))))
}

# ── Static sections ─────────────────────────────────────────────────────────
md_basic_sections <- function() {
  md_table(c("Section", "Measure", "Rule"), list(
    c("01 VIX", "VIX spot", "GREEN &lt; 20, ORANGE 20&ndash;25, RED 25&ndash;30, DARKRED &gt; 30"),
    c("01 VIX", "VVIX", "GREEN &lt; 80, ORANGE 80&ndash;100, RED 100&ndash;120, DARKRED &gt; 120"),
    c("01 VIX", "VIX / VIX3M", "GREEN &lt; 0.90 contango, ORANGE 0.90&ndash;1.00, RED &ge; 1.00 backwardation (10-year median 0.88, p95 1.02)"),
    c("01 VIX", "VX1 &minus; VX2", "CBOE settlement, front minus second future: GREEN &lt; &minus;0.5, ORANGE &minus;0.5 to 0, RED 0 to +0.5, DARKRED &gt; +0.5"),
    c("01 VIX", "VIX 20d ago", "RED when VIX is above its level 20 sessions ago"),
    c("02 Rates", "10Y / 30Y", "GREEN &lt; 4.5%, ORANGE 4.5&ndash;5%, RED &gt; 5%"),
    c("02 Rates", "3M/10Y spread", "^TNX minus ^IRX; colour from the 20-day move when it is &ge; 5 bp: RED bear steepening (10Y up more), GREEN bull steepening, ORANGE flattening"),
    c("02 Rates", "TLT 20d", "GREEN &gt; +1%, RED &lt; &minus;1%"),
    c("02 Rates", "TIP 20d", "Real-yield proxy: GREEN &gt; +0.5%, RED &lt; &minus;0.5%"),
    c("03 Breadth", "% S&amp;P 500 above MA50", "Constituents from Wikipedia, each stock's close vs its 50-day average. DARKRED &lt; 20, RED 20&ndash;35, ORANGE 35&ndash;50, GREEN 50&ndash;75, ORANGE &gt; 75. Absolute measure, unlike the equal-weight / S&amp;P ratio used in the scenarios"),
    c("03 Breadth", "SPY vs MA20 / MA50", "GREEN above both, RED below both"),
    c("04 Dollar + commodities", "DXY, USO, GLD, XLE", "Position vs MA20 and 20-day return; DXY GREEN &lt; +1%, RED &gt; +3%")))
}

md_panels <- function() {
  paste0(
    "<p><b>09 panels.</b> 1D / 1W / 1M / 3M = 1, 5, 21, 63-session changes (yields in bp). 200-day = close vs 200-day simple average. ",
    "Trend: UP = close above EMA50 and EMA20 above EMA50; DOWN = mirror; MIXED otherwise. Ratios divide adjusted closes. ",
    "FX is joined as-of on the date because Yahoo stamps FX one day early in BST.</p>",
    "<p><b>10 sector map.</b> Each correlation group is the equal-weight index of its members (mean daily return, compounded; a day counts when at least half trade). ",
    "RS ratio = 100 &times; (group / S&amp;P 500) / its 50-day average; RS mom = 10-day change of RS ratio. ",
    "Rotation: Leading (ratio &ge; 100, mom &ge; 0), Weakening (&ge; 100, &lt; 0), Improving (&lt; 100, &ge; 0), Lagging (&lt; 100, &lt; 0). ",
    "Drivers = mean of sensitivity sign &times; driver trend (UP +1, DOWN &minus;1, MIXED 0). ",
    "Verdict: LONG = trend UP, Leading or Improving, drivers &ge; 0; SHORT = trend DOWN, Lagging or Weakening, drivers &le; 0; ",
    "AVOID = trend MIXED, or drivers &le; &minus;0.5 against an UP trend (&ge; +0.5 against DOWN); WATCH = the rest.</p>",
    "<p><b>11 COT.</b> CFTC weekly report (Tuesday positions). Index = 100 &times; (current &minus; lowest) / (highest &minus; lowest) over 52, 156 or 260 reports. ",
    "Speculative group: managed money (commodities), leveraged funds (financials); other side: commercials, asset managers; ",
    "legacy large speculators for comparison with COTSignal. Extreme = 5-year net percentile &ge; 90 or &le; 10.</p>")
}

md_limits <- function() {
  paste0("<ul class='md-list'>",
    "<li>Scenario fingerprints, weights and the in-place threshold are judgment, not calibrated (section M1).</li>",
    "<li>BREADTH in the fingerprints is relative (equal weight vs cap weight), not the share of stocks above their MA50. ",
    "A falling ratio means the average stock lags the index, which can happen with breadth good or bad in absolute terms.</li>",
    "<li>CONS (XLY / XLP) is partly a mega-cap measure: Amazon and Tesla are a large share of XLY.</li>",
    "<li>credit_stress in the regime model reads HYG's price, so it rises with Treasury yields even when spreads are stable.</li>",
    "<li>All scenario moves use 21- and 63-day windows; shocks that play out in days (carry unwind) show up late and are diluted.</li>",
    "<li>The report uses the previous close when it runs before the US close (Yahoo data without today's bar).</li>",
    "</ul>")
}

#' Full Methodology tab
methodology_html <- function() {
  paste0(
    "<p class='md-intro'>How every number in the report is computed. Scenario fingerprints, regime weights and mismatch rules are read from the code at each run.</p>",
    md_section("M1", "Scenario match &mdash; section 00", md_scenario_method(), open = TRUE),
    md_section("M2", "Fingerprint assets", md_fp_assets()),
    md_section("M3", sprintf("Scenario rules (%d scenarios)", length(ARCHETYPES)), md_scenarios()),
    md_section("M4", "Sections 01&ndash;04 &mdash; zone thresholds", md_basic_sections()),
    md_section("M5", "Section 05 &mdash; mismatches", md_mismatches()),
    md_section("M6", "Sections 06&ndash;07 &mdash; regime model and Daily Bias", md_regimes()),
    md_section("M7", "Sections 09&ndash;11 &mdash; panels, sector map, COT", md_panels()),
    md_section("M8", "Known limitations", md_limits())
  )
}
