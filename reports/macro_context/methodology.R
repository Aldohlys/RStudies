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
    sprintf(paste0("<p><b>Absolute breadth</b> (share of S&amp;P 500 stocks above their 50-day average) is a level, not a move: it enters as (level &minus; %g) / %g, ",
            "so %g%% = 0 and %.1f%% or %.1f%% reach the cap. The 1-month value is the latest reading; the 3-month value is the mean of the readings over 63 sessions. ",
            "Readings come from the daily runs (stored since 2026-03-17); a reading made on a given morning is assigned to the previous session. ",
            "It complements the equal-weight / S&amp;P ratio, which only says whether the average stock beats the index.</p>"),
            ABS_BREADTH_CENTER, ABS_BREADTH_SCALE, ABS_BREADTH_CENTER,
            ABS_BREADTH_CENTER - 1.5 * ABS_BREADTH_SCALE, ABS_BREADTH_CENTER + 1.5 * ABS_BREADTH_SCALE),
    "<p><b>Step 2, cap.</b> Each z is divided by 1.5 and capped to [&minus;1, +1], so one extreme asset cannot dominate.</p>",
    "<p><b>Step 3, move score.</b> score = &Sigma; (weight &times; capped z) / &Sigma; |weight|, over the fingerprint assets with data. ",
    "Range &minus;100% (every asset moving opposite to the fingerprint) to +100% (every asset moving at least 1.5 z in the expected direction). ",
    "The sign is agreement, not a probability and not good or bad news. Scores are not exclusive: several scenarios can be positive together when they share assets. ",
    "The 3-month move score is the same computation on 63-session changes (&radic;63 scaling).</p>",
    sprintf(paste0("<p><b>Step 4, state score.</b> The move says what changed, not where markets stand: a scenario can describe the situation and score low because its assets ",
            "stopped moving, or score high on assets that moved but are still far from that scenario. Each asset's state is its position in its range of the last %d sessions ",
            "(at least %d): 2 &times; (last &minus; min) / (max &minus; min) &minus; 1, so the bottom of the range is &minus;1 and the top +1, with the same sign inversions as the move. ",
            "Absolute breadth uses its capped level. State score = &Sigma; (weight &times; state) / &Sigma; |weight|, same weights as the move.</p>"),
            STATE_WINDOW, STATE_MIN_ROWS),
    sprintf(paste0("<p><b>Status.</b> The state decides whether a scenario is in place (state &ge; %.0f%%); the 1-month move gives the direction (strong at &ge; %.0f%%). ",
            "Both are recomputed for each of the last %d trading days from price history. Scenarios are ranked by state.</p>"),
            100 * STATE_ACTIVE, 100 * SCEN_ACTIVE, HIST_DAYS),
    md_table(c("Status", "State", "Move (1 month)", "Reading"), list(
      c("BUILDING", "in place", sprintf("&ge; %.0f%%", 100 * SCEN_ACTIVE), "In place and still strengthening"),
      c("ESTABLISHED", "in place, under 40 days", sprintf("0 to %.0f%%", 100 * SCEN_ACTIVE), "In place; moves no longer add much"),
      c("MATURE", "in place, 40 days or more", sprintf("0 to %.0f%%", 100 * SCEN_ACTIVE), "Nearer its end than its start"),
      c("FADING", "in place", "below 0", "Its assets now move against it; often the start of its end"),
      c("EMERGING", "not in place", sprintf("&ge; %.0f%%", 100 * SCEN_ACTIVE), "Moving toward it from elsewhere; watch whether the state follows"),
      c("FADED", "not in place, in place in one of the previous five reports", "any", "Markets have moved away from it"),
      c("INACTIVE", "not in place", sprintf("below %.0f%%", 100 * SCEN_ACTIVE), ""))),
    "<p><b>Why both.</b> On 2004&ndash;2026 episodes (compare_state_move.py, NewTrading/Reports/scenario_state_vs_move_20261008.md) the state separates episodes better ",
    "on average (held-out AUC 0.81 vs 0.78 for the move), but detects them later (median two to four times as many sessions after the start), stays in place up to 36 sessions after they end, ",
    "and misses turns: policy pivot 3 of 3 episodes, leadership unwind 2 of 3, because on the first day of a turn assets still sit at the old extreme. ",
    "The move catches starts, turns and ends. The state threshold (50%) is the lowest at which scenarios are in place on at most 10% of their non-episode days on average (hit rate 42%).</p>",
    "<p><b>Agree / contradict lists.</b> An asset agrees when sign(weight) &times; z &ge; 0.5 and contradicts when it is &le; &minus;0.5.</p>",
    sprintf("<p><b>Chains.</b> A chain is a sequence of fingerprint assets that must each be clearly up (z &ge; %g). Status: firing (all links up), triggered (first link up), ",
            CHAIN_ON),
    "not triggered (later links up without the first, i.e. for another reason).</p>",
    "<p><b>Provenance.</b> Fingerprints, weights, the 1.5 cap and the in-place threshold were set by judgment when the layer was built (2026-10-02), ",
    "from the historical episodes listed under each scenario. ",
    "Calibration check on 2004&ndash;2026 daily data (calibrate_scenarios.py, report NewTrading/Reports/scenario_calibration_20261008.md): ",
    "with two to four dated episodes per scenario, 40% is the lowest move threshold at which scenarios reach that level on at most 10% of the days outside their episodes, on average (now the level for a strong move); ",
    "per-scenario thresholds (35&ndash;60%) were not adopted because so few episodes would mostly fit noise. ",
    "The same study adjusted six fingerprints on 2026-10-08 where a change improved separation on held-out episodes and has a market explanation ",
    "(policy pivot, stagflation, goldilocks, dollar wrecking ball, bond rout, debasement; details in NewTrading/Reports/scenario_weight_calibration_20261008.md). ",
    "The credit-event scenario, added the same day, was checked on five episodes (2007&ndash;2023) and kept as written: a refit did no better on held-out episodes. ",
    "Absolute breadth is excluded from the study (no history before 2026-03), so its weights remain judgment.</p>"
  )
}

md_fp_assets <- function() {
  rows <- lapply(names(FP_ASSETS), function(k) {
    a <- FP_ASSETS[[k]]
    sy <- paste(a[[1]], collapse = ", ")
    kind <- if (k == "ABS_BREADTH") sprintf("level: (%% &minus; %g) / %g", ABS_BREADTH_CENTER, ABS_BREADTH_SCALE)
            else if (any(a[[1]] %in% YIELD_SYMBOLS)) "yield change (pp)" else "log return"
    if (k == "ABS_BREADTH") sy <- "daily runs (macro_context_results.s5fi)"
    read <- if (a[[2]] < 0) "inverted (quote down = asset up)" else "as quoted"
    c(md_esc(a[[3]]), k, md_esc(sy), kind, read)
  })
  md_table(c("Asset", "Key", "Symbol(s)", "Move", "Reading"), rows)
}

md_carry <- function() {
  ca <- CARRY_ALERT
  paste0(
    "<p>The yen carry-trade unwind plays out in one to three sessions, so it is not a scenario (21- and 63-day windows would catch it late and diluted). ",
    sprintf("It is checked on %d-session z-scores (same definition as M1 with n = %d), for each of the last %d sessions.</p>", ca$window, ca$window, CARRY_LOOKBACK),
    md_table(c("Input", "Rule"), list(
      c("Yen", sprintf("&minus;z of USD/JPY (yen up = positive). Fires at &ge; +%g, watch at &ge; +%g", ca$yen_fire, ca$yen_watch)),
      c("AUD/JPY", sprintf("confirms at z &le; &minus;%g", ca$confirm_z)),
      c("Nikkei 225", sprintf("confirms at z &le; &minus;%g", ca$confirm_z)),
      c("Bitcoin", sprintf("confirms at z &le; &minus;%g", ca$confirm_z)),
      c("VIX term structure", "confirms when VIX closes at or above VIX3M"),
      c("FIRING", "yen at the firing level and at least 2 of the 4 confirmations"),
      c("WATCH", "yen at the watch level and at least 1 confirmation"))),
    "<p>Check on 2019&ndash;2026 daily data: FIRING on 19 sessions in 6 episodes, all yen-up risk-off events: August 2019, February&ndash;March 2020, ",
    "November 2021, December 2022 (Bank of Japan yield-cap change), 25 July&ndash;7 August 2024 (first fire ten days before the 5 August crash), April 2025. WATCH on 31 sessions.</p>",
    sprintf("<p><b>Past episodes.</b> %s</p><p><b>What came next.</b> %s</p><p><b>BOT.</b> %s</p>", md_esc(ca$analogs), md_esc(ca$after), md_esc(ca$bot)))
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
    "<p>credit_stress uses the 20-day return of the high yield / Treasuries ratio (HYG/IEF, adjusted closes), the same series as the CREDIT asset in the scenarios. ",
    "Until 2026-10-08 it used HYG's own price, which also falls when Treasury yields rise (June 2022: HYG &minus;3.1%, ratio &minus;0.2%); the ratio isolates the spread. ",
    "Center and scale recalibrated on 2016&ndash;2026 data.</p>",
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
    "<li>Weights and the in-place threshold were checked against dated episodes (M1), but episode dates are judgment and each scenario has only two to four. ",
    "The state uses 1-year ranges, which rebase with the regime: a level held for a year drifts toward mid-range. Goldilocks is the weakest detector on either score.</li>",
    "<li>BREADTH in the fingerprints is relative (equal weight vs cap weight): a falling ratio means the average stock lags the index, whatever absolute breadth is. ",
    "ABS_BREADTH adds the level, but its history starts on 2026-03-17, so it cannot be backtested and the 3-month value needs 10 readings.</li>",
    "<li>CONS (XLY / XLP) is partly a mega-cap measure: Amazon and Tesla are a large share of XLY.</li>",
    "<li>Scenario moves use 21- and 63-day windows; shocks that play out in days show up late. The carry unwind is handled by the short-window alert (M4) for that reason.</li>",
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
    md_section("M4", "Yen carry-unwind alert (short window)", md_carry()),
    md_section("M5", "Sections 01&ndash;04 &mdash; zone thresholds", md_basic_sections()),
    md_section("M6", "Section 05 &mdash; mismatches", md_mismatches()),
    md_section("M7", "Sections 06&ndash;07 &mdash; regime model and Daily Bias", md_regimes()),
    md_section("M8", "Sections 09&ndash;11 &mdash; panels, sector map, COT", md_panels()),
    md_section("M9", "Known limitations", md_limits())
  )
}
