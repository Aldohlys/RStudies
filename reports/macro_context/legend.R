# legend.R — "Legend" tab: L1 glossary (every term used on Main and Detail), L2 notes that explain a
# table (collected while the report is rendered, so no footnote sits under a table), then M1-M9 (methodology.R).
# One dictionary (LEGEND_GLOSSARY) for every definition: add a term here when a new label appears in the report.

.leg <- new.env()
.leg$notes <- list()

#' Register a table note for the Legend tab; returns "" so it can replace a footnote in place
leg_note <- function(where, html) {
  if (!where %in% vapply(.leg$notes, `[[`, "", "where"))   # a fragment rendered twice (two cards) registers once
    .leg$notes[[length(.leg$notes) + 1]] <- list(where = where, html = html)
  ""
}
leg_reset <- function() .leg$notes <- list()

LEGEND_GLOSSARY <- list(
  "Report structure" = list(
    c("Main / Detail / Legend", "Main: what is needed every day. Detail: the market panels 01&ndash;04 and the full scenario material (00b). Legend: definitions and methods."),
    c("Section numbers (00&ndash;11)", "Fixed numbers of the report panels, kept so that cross-references stay valid: 00 global movie, 01 VIX, 02 rates, 03 breadth, 04 dollar and commodities, 08 events, 09 intermarket, 10 BOT sector map, 11 COT positioning. 05 (mismatches), 06 (synthesis) and 07 (regime model) were removed on 2026-10-20."),
    c("Tiles", "The boxes at the top of the summary: last value, 1-month change and z-score of one market, with a 3-month sparkline. Border colour: blue = the move supports risk and liquidity, red = it drains them, black = a large move (|z| &ge; 1) whose reading depends on the scenario."),
    c("Changed since", "Regime, VIX zone, breadth zone, scenario status and sector-verdict changes compared with the previous report run."),
    c("BOT / BOT-", "The user's breakout strategy and its two entry tiers in the bot_daily workbook (BOT = trend &ge; 4/6 and asymmetry &ge; 1.5; BOT- = weaker). BOT-eligible = in the BOT universe (Tickers.BOT_Eligible)."),
    c("Panels 01&ndash;04 measures (VX1&minus;VX2, policy priced, 2Y/10Y, &hellip;)", "Each Detail panel measure and its colour thresholds are defined in M5."),
    c("Sparkline", "Small line chart without axes (3 months on the tiles, 60 sessions on the scenario cards); only the shape matters.")),
  "Regime and risk" = list(
    c("Regime", "Direction of the section-00 scenarios in place (status BUILDING, ESTABLISHED, MATURE or FADING, for at least 3 sessions in a row; PENDING scenarios do not count). DEFENSIVE: dash for cash, dollar wrecking ball, bond rout, stagflation, growth scare, EM/China shock, credit event, leadership unwind. LONG BIAS: reflation, goldilocks, policy pivot, narrow leadership. NEUTRAL: none in place, or only dollar debasement. MIXED: scenarios in place point both ways. EMERGING scenarios are listed, not counted."),
    c("Risk (CALM / ELEVATED / STRESS)", "Short-term stress from VIX and the US high-yield credit spread (FRED BAMLH0A0HYM2). Each is on when its 20-session z-score reaches +1 and off when it falls back below +0.5. CALM = both off, ELEVATED = one on, STRESS = both on. Thresholds provisional."),
    c("Framework", "The user's fixed BOT rule for each regime &times; risk cell (number of trades, size, entries). Not computed; the report only selects the cell."),
    c("Regime &times; risk table", "The six framework rules: rows = regime (long, neutral/mixed, defensive), columns = risk (calm, elevated or stress). Today's cell is outlined. What followed historically in each cell: M7.")),
  "Scenarios (section 00)" = list(
    c("Scenario", "One of 13 named macro situations (M3), each described by a fingerprint."),
    c("Fingerprint", "The set of assets a scenario moves, with the expected direction and a weight per asset (M1, M3)."),
    c("State", "Where markets stand against a scenario's fingerprint: each asset's position in its one-year range, weighted. &minus;100% = opposite of the scenario, +100% = exactly the scenario. In place from +50%."),
    c("Move (1M / 3M)", "How far the last 1 or 3 months of moves point toward the scenario, from &minus;100% to +100%; strong from +40% (M1)."),
    c("In place / NOT IN PLACE", "State &ge; 50% for at least 3 sessions in a row / otherwise (map regions use the 50% line)."),
    c("BUILDING", "In place and move &ge; 40%: still strengthening."),
    c("ESTABLISHED", "In place for under 40 days, move 0 to 40%: in place, moves no longer add much."),
    c("MATURE", "In place for 40 days or more, move 0 to 40%."),
    c("FADING", "In place but move below 0: its assets now move against it."),
    c("PENDING n/3", "State &ge; 50% for fewer than 3 sessions in a row: not yet counted as in place (band, status and Regime use the same rule)."),
    c("EMERGING", "Not in place but move &ge; 40%: markets moving toward it."),
    c("FADED", "Not in place today but in place in one of the previous five reports."),
    c("INACTIVE", "Not in place and move below 40%."),
    c("Scenario map", "Scatter of all scenarios: x = state, y = 1-month move; tails = the last 5 sessions. Numbered dots match the ranked table in Detail 00b."),
    c("Scenario in place band", "One cell per session over the last 60 sessions, coloured by the top scenario in place. A scenario counts only from 3 sessions in a row; a gap of up to 4 sessions between two runs of the same scenario is bridged; a run of 1&ndash;2 sessions still going today is shown pale as pending. Grey = no scenario."),
    c("Confirms / Ends if", "The scenario's own tells and invalidation condition, as written in M3 ('Signs to watch', 'Not this scenario if')."),
    c("What the month says", "The interpretive sentences of the global movie (full text in Detail 00b)."),
    c("Last session", "Change of each index from its previous close, in local currency. A market's own date is shown only when its last bar is older than the others'. A bar dated on the report day can be intraday if that market was still open when the report ran."),
    c("Chains", "A sequence of fingerprint assets that must each be clearly up (z &ge; 1). Firing = all links up; triggered = first link up; not triggered otherwise."),
    c("Yen carry (QUIET / WATCH / FIRING)", "Short-window alert on 3-session z-scores (M4). FIRING = yen z &ge; +2 with at least 2 confirmations (AUD/JPY, Nikkei, Bitcoin at z &le; &minus;2, VIX at or above VIX3M); WATCH = yen z &ge; +1.5 with at least 1; QUIET otherwise."),
    c("Scenario LONG / SHORT", "Groups the scenario expects to rise / fall: the sector groups tagged with the ETFs listed for the scenario in archetypes.R (bot$long, bot$short).")),
  "Sector map (section 10)" = list(
    c("Sector map", "Section 10: one row per correlation group of BOT-eligible names, read as an equal-weight index of its members."),
    c("Group", "A correlation cluster of stocks (ScannerUniverse) with at least one BOT-eligible name, e.g. 'Energy - Refiners'."),
    c("LONG / SHORT / WATCH / AVOID (sector map)", "Mechanical verdict per group, not a recommendation. LONG = trend UP, rotation Leading or Improving, drivers not against; SHORT = trend DOWN, rotation Lagging or Weakening, drivers not against; AVOID = trend MIXED or drivers clearly against the trend; WATCH = trend and rotation disagree."),
    c("Trend (UP / DOWN / MIXED)", "UP = close above its 50-day exponential average (EMA50) and EMA20 above EMA50; DOWN = the mirror; MIXED otherwise."),
    c("RS ratio", "100 &times; (group / S&amp;P 500) divided by its own 50-day average. Above 100 = stronger than the S&amp;P recently."),
    c("RS mom", "10-day change of the RS ratio."),
    c("Rotation (Leading / Weakening / Improving / Lagging)", "Leading = RS ratio &ge; 100 and RS mom &ge; 0; Weakening = &ge; 100 and &lt; 0; Improving = &lt; 100 and &ge; 0; Lagging = &lt; 100 and &lt; 0."),
    c("Drivers", "Macro series assigned to each group (e.g. oil, 10-year yield, XLY/XLP), each scored +1 / &minus;1 / 0 by its trend and sensitivity sign; the mean is the driver score, from &minus;1 to +1."),
    c("BOT / members", "BOT-eligible names in the group / all members.")),
  "Statistics and notation" = list(
    c("z-score", "A change divided by the usual size of changes of that series: (change over n sessions) / (standard deviation of daily changes over a trailing window &times; &radic;n). About 1 = a normal-sized move for that horizon; &ge; 2 = unusually large."),
    c("1D / 1W / 1M / 3M", "Changes over 1, 5, 21 and 63 sessions."),
    c("Session", "One trading day of the US market."),
    c("bp", "Basis point = 0.01 percentage point."),
    c("YTD band", "Range of returns since the last close of the previous year; the dot is today."),
    c("MA20 / MA50 / EMA20 / EMA50", "20- and 50-session simple (MA) or exponential (EMA) moving averages of the close."),
    c("Contango / backwardation", "Futures curve shape: contango = later contracts priced above the front; backwardation = below. For VIX, VIX/VIX3M &ge; 1 is backwardation (stress)."),
    c("Base rate", "In M7: the share of all Fridays on which the stated move happened; the comparison point for a condition's hit rate.")),
  "Events (section 08)" = list(
    c("CPI, PMI, claims, &hellip;", "Economic releases: consumer price index, purchasing managers' index (flash = first estimate), weekly unemployment claims."),
    c("CRITICAL / HIGH / MODERATE", "Expected market impact of a calendar event, set by keyword rules in events.R (e.g. central-bank decisions HIGH, US elections CRITICAL).")),
  "COT positioning (section 11)" = list(
    c("COT", "CFTC Commitments of Traders report: futures positions by trader group, as of each Tuesday, published Friday."),
    c("Managed money / Leveraged funds / Asset managers", "Trader groups of the CFTC reports: managed money (commodities, disaggregated report); leveraged funds and asset managers (financial futures, TFF report). USD uses leveraged funds + asset managers net."),
    c("Net / Long / Short index", "100 &times; (current &minus; lowest) / (highest &minus; lowest) of that position over the last 1, 3 or 5 years of weekly reports; 100 = the most extreme of the window.")),
  "Instruments and symbols" = list(
    c("VIX / VIX3M / VVIX", "Implied volatility of the S&amp;P 500 over 30 days / 3 months; VVIX = implied volatility of VIX."),
    c("MOVE", "Implied volatility of US Treasuries (bond-market VIX)."),
    c("DXY (DX-Y.NYB)", "US dollar index against six major currencies."),
    c("HYG, IEF, LQD, TLT, TIP", "ETFs: US high yield bonds, 7&ndash;10-year Treasuries, investment-grade corporates, 20+-year Treasuries, inflation-protected Treasuries (TIPS)."),
    c("Credit HYG/IEF", "Ratio of the high-yield ETF to the 7&ndash;10-year Treasury ETF; up = credit stronger. Moves partly with yields (duration mismatch)."),
    c("Real yield / breakeven / term premium", "FRED DFII10 (10-year TIPS yield) / T10YIE (10-year nominal minus TIPS) / THREEFYTP10 (Kim-Wright 10-year term premium)."),
    c("High-yield spread", "FRED BAMLH0A0HYM2: ICE BofA US high-yield option-adjusted spread over Treasuries (FRED keeps 3 years)."),
    c("Breadth", "Share of S&amp;P 500 stocks above their 50-day average."),
    c("SPY, QQQ, RSP, IWM, EEM", "ETFs: S&amp;P 500, Nasdaq 100, S&amp;P 500 equal weight, Russell 2000, emerging-market equities."),
    c("XL* (XLE, XLY, XLP, XLV, XLI, XLB, &hellip;)", "SPDR sector ETFs: energy, consumer discretionary, consumer staples, health care, industrials, materials, &hellip;"),
    c("XOP, OIH, FCG, XRT, CARZ, ITB, IGV, DBA, MOO, GDX", "ETFs: oil &amp; gas E&amp;P, oil services, natural gas, retail, autos, homebuilders, software, agriculture futures, agribusiness, gold miners."),
    c("ETF / SPDR", "Exchange-traded fund / the State Street sector ETF family (XL*)."),
    c("^GSPC, ^NDX, ^RUT, ^TNX, ^IRX", "Yahoo index symbols: S&amp;P 500, Nasdaq 100, Russell 2000, US 10-year yield, US 3-month bill yield."),
    c("World indices", "Euro Stoxx 50, DAX, SMI, CAC 40, IBEX 35, FTSE MIB, Nikkei 225, KOSPI, HSCEI (Hong Kong-listed Chinese companies, offshore), Shanghai Composite (onshore), Bovespa, IPC: the main equity index of each market."),
    c("WTI / Brent / CL=F / BZ=F", "US / North Sea crude oil benchmarks and their front futures (roll-free series, M8)."),
    c("ULSD / RBOB", "Ultra-low-sulphur diesel (heating oil) and gasoline futures."),
    c("CFTC", "US Commodity Futures Trading Commission, publisher of the COT reports."),
    c("Stock tickers in group rows", "Exchange symbols of the group's member companies."))
)

legend_glossary_html <- function() {
  body <- paste(vapply(names(LEGEND_GLOSSARY), function(h) paste0(
    "<div class='ck-h'>", h, "</div><dl class='lg-dl'>",
    paste(vapply(LEGEND_GLOSSARY[[h]], function(t) sprintf("<dt>%s</dt><dd>%s</dd>", t[1], t[2]), ""), collapse = ""),
    "</dl>"), ""), collapse = "")
  sprintf("<details class='section' style='margin-bottom:16px' open><summary class='stitle'><span class='snum'>L1</span>Glossary</summary><div class='lg-body'>%s</div></details>", body)
}

legend_notes_html <- function() {
  if (!length(.leg$notes)) return("")
  body <- paste(vapply(.leg$notes, function(n) sprintf("<div class='lg-note'><div class='lg-where'>%s</div><div class='lg-text'>%s</div></div>", n$where, n$html), ""), collapse = "")
  sprintf("<details class='section' style='margin-bottom:16px' open><summary class='stitle'><span class='snum'>L2</span>Notes on the report tables</summary><div class='lg-body'>%s</div></details>", body)
}

#' Whole Legend tab; call after every other fragment is rendered (notes are collected during rendering)
legend_html <- function() {
  paste0("<p class='md-intro'>Definitions of every term used in the report (L1), the notes on how to read each table (L2), ",
         "and how every number is computed (M1&ndash;M9).</p>",
         legend_glossary_html(), legend_notes_html(), methodology_sections_html())
}
