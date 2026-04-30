# Changelog - RStudies

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [2026-04-30] - /analyze ported from slash-command to R script (data-only)

### Added
- **reports/analyze/main.R** (new): single-ticker /analyze pipeline orchestrator. Runs Phases A→B→C→D→E mechanically; produces neutral HTML + terminal data table. Args: `<TICKER> <DIRECTION> [--no-html] [--no-vol-funnel]`.
- **reports/analyze/phases.R** (new): Phase A (Universe rich-options gate), B (Pull score), C.1 (Cheap score), E (mechanical classification). Reads latest `swing_scanner_v5_<DATE>.csv` row; falls back to live Tdata helpers when CSV emits NA.
- **reports/analyze/funnel.R** (new): Phase C.2 directional vol funnel — IV landscape, VRP (both log-ratio and vol-pts forms), term-structure shape detect, RR 25Δ skew, earnings DTE. Outputs a 6-row mechanical-label grid + signal tally for the user's direction (favorable / unfavorable / unavailable counts). Tally is a count, not a verdict.
- **reports/analyze/structures.R** (new): Phase D structural-target read + structures-within-cap enumerator. Calls `tdata_py.spread.compute_spread_risk_reward` via reticulate when TWS is reachable; falls back to placeholder rows otherwise. Applies `risk_cap_lot_usd` filter.
- **reports/analyze/report.R** (new): neutral HTML renderer. Colorblind-safe Okabe-Ito palette, mechanical PASS/SKIP/NO SIGNAL/STALE badges. Sections: phase summaries, vol funnel grid, structural targets, chain, structures table, data summary. NO verdict cards, NO conviction levels, NO Best/Alternative/Avoid framing, NO edge-source narrative.
- **reports/analyze/defaults.R** (new): built-in defaults + deep-merge with optional `config.yml`. The script ships with sensible defaults (`risk_cap_lot_usd: 300`, `rr_min: 0.5`, IVP regime / scoring thresholds, VRP log-ratio bands, term scoring thresholds, spread widths, moneyness, earnings window, skew lookback, out_dir) so a fresh clone runs without any local config.
- **../scripts/run_analyze.bat** (new, in RApplication/scripts): batch wrapper. Activates RStudies renv, runs `Rscript reports/analyze/main.R <TICKER> <DIRECTION>`, opens the produced HTML.

### Configuration model
- `config.yml` is instance-specific and **not tracked** (may contain secrets, paths, account lists). It is NOT required to run the script.
- If `config.yml` exists at the RStudies root and contains a `default.analyze` block, its keys override the built-in defaults via deep-merge — only the keys you set differ; everything else inherits from `defaults.R`.
- To recalibrate (e.g. raise `risk_cap_lot_usd` to 500 for a single instance): add only that key in `config.yml`'s `default.analyze` block. No code change.

### Changed
- **NewTrading/.claude/commands/analyze.md**: shrunk from ~330 lines to a thin wrapper that shells out to the R script and surfaces its HTML in chat. The data-only contract (no verdicts, no rankings) is enforced in `report.R`, not in the prompt — editing the slash-command stub cannot re-introduce verdicts.

### Context
- Drove the split: the slash-command version produced inconsistent verdicts despite a `feedback_analyze_neutral_stance.md` memory note explicitly forbidding them. Mechanical computation belongs in code; LLM stays for follow-up Q&A.
- Layout mirrors `reports/macro_context/` and `reports/swing_scanner/` — sources `shared/html_helpers.R` and `shared/cache.R`.
- Reuses (does not duplicate) Phase B/C scoring logic by reading the most recent v5 CSV. If a future caller wants to compute Phases B/C without the v5 batch having run first, `phases.R` would need a path to invoke `pull_score.R` / `cheap_score.R` directly — deferred until needed.
- Compute_spread_risk_reward called via reticulate with `force_refresh=True` (mandatory cache bypass per pre-existing rule). Live OI walk and direct Tdata `getOptMarketData` integration deferred — placeholder rows surface the data gap in the report.

### Validation
- First-run target: UPS short on 2026-04-30 — should reproduce the data shown in the most recent UPS HTML report (PASS A, SKIP B with pull_score=0, NO SIGNAL D, v5=SKIP, phase_of_drop=B).

## [2026-04-27] - Swing Scanner v5: front-run option flow redesign

### Added
- **swing_scanner/universe_filter.R** (new): Phase A rich-options gate — weeklies in next 14d + ATM bid/ask ≤ 12% (end-of-day). Permissive default until daily fetch wires bid/ask check.
- **swing_scanner/pull_score.R** (new): Phase B Pull screen — Stage classification (early/continuation/extended/none, priority: extended > early > continuation), sector RS rank (top-3 +3, 4-6 +2, bottom-half 0, SHORT-only −2), footprint (OBV/UpDn/RS_3m). Cutoff Pull_Score ≥ 6 AND Stage ∈ {early, continuation}. Reuses `score_breakout()` from scoring.R.
- **swing_scanner/cheap_score.R** (new): Phase C Cheap screen — IVP (4 pts, prefers 1y IBKR-native, falls back to 2y), VRP (2 pts), term structure (2 pts), skew vs own history (1 pt), cross-sectional sector rank (1 pt). Cutoff Cheap_Score ≥ 6 AND Cheap_Side aligns with Pull_Direction.
- **swing_scanner/setup_chain_rr.R** (new): Phase D — vehicle selection (call/spread/stock per BOT checklist), structural target consensus (prior swing high + 52w high + tiered round number; Fib confirmation overlay only), per-strike OI walk via `option_chain_oi_history`, R:R + Entry_Floor/Entry_Ceiling via BS-inversion at R:R_min.
- **swing_scanner/final_classify_v5.R** (new): Phase E — TOP PICK / WATCH / SKIP gate, phase-of-drop tagging (A/B/C/D), sector-cluster badge.
- **swing_scanner/render_html_v5.R** (new): interactive HTML with DataTables — funnel header (Universe → Pull → Cheap → Setup → TOP PICK), filter chips (phase-of-drop, sector, vehicle, show-SKIP), Tier 1 visible / Tier 2 hidden columns, per-day file `swing_scanner_v5_YYYYMMDD.html`. Self-contained: data embedded as JSON, DataTables from CDN, no build step.
- **swing_scanner/main_v5.R** (new): orchestrator wiring all five phases. Persists results to `scanner_results_v5` table (DDL in `NewTrading/scripts/init_scanner_tables.R`).

### Context
- Replaces legacy 2-gate composite (`final_filter.R` preserved during transition).
- Driven by JPM rotation miss (2026-04-24): scanner surfaced BAC + JPM as TOP PICK while Tech led the tape; sector RS data existed but wasn't consumed.
- Edge reframed: "front-run upcoming option flow — buy cheap options, sell expensive but still buyable" (validated against 39 BOT winners, JNJ/DOW/URA exit remarques explicitly state this lens).
- R:R_min = 0.5 calibrated empirically from `Trades` table (25th percentile of realized R:R_max across 37 BOT winners; calibration in `NewTrading/scripts/calibrate_rr_min.R`).
- Anti-noise N.1/N.2: 5-day median IV/OI smoothing wherever numeric inputs feed gates.
- Tier 1 columns visible by default: Ticker, Sector, Stage, Pull_Score, Cheap_Score, Vehicle, Spot_Target, R:R, Entry_Floor, Entry_Ceiling, Entry_State, Chain_State.
- 27/27 unit tests passing (NewTrading/scripts/test_v5_modules.R).
- Depends on Tdata 5.10.8+ for new `get_chain_oi()` chain-walk helper used by daily fetch task.
- Depends on `option_skew_history` and `option_chain_oi_history` DB tables (DDL in NewTrading/scripts/init_scanner_tables.R).

### Known limitations on first runs
- Phase A passes everything until daily option fetch wires the bid/ask check.
- Phase C requires `Prices.ivp` (1y IBKR-native); names with stale Vol metrics return Cheap_Score = 0.
- Phase D.3 returns NO DATA → Entry_State = NO CHAIN until daily fetch populates `option_chain_oi_history`.

### Validation pending
- Replay 30 BOT winners' entry days + JPM 2026-04-24 day to confirm winners surface as TOP PICK and JPM is correctly demoted.

## [2026-04-20] - Earnings-date flag in swing scanner + FAIL-Optionality filter

### Added
- **swing_scanner/main.R**: refresh stale earnings dates (`Tdata::updateStaleEarnings()`) at scan start, LEFT JOIN `NextEarnings` from Tickers into the scored `out` dataframe, derive `EarningsInDays` column
- **swing_scanner/render_html.R**: new `Earnings` column at end of both Signal and BOT tables with colored badge (Okabe-Ito palette)
  - `≤7 days` → vermillion `#D55E00` (don't trade this week)
  - `8-14 days` → yellow `#F0E442` (caution)
  - `>14 days` → plain
  - Tooltip: `"Next earnings: YYYYMMDD (in Nd)"`
- **swing_scanner/render_html.R**: new `fmt_earnings_cell()` helper reused by both tables

### Changed
- **swing_scanner/render_html.R**: `build_bot_section()` now hides rows where `Optionality == "FAIL"` AND `Price > 30` — no viable trading vehicle (no options play and stock position too capital-heavy). FAIL under $30 kept for direct stock-buy fallback (e.g. RIG, LAC). "NO DATA" is preserved (unknown ≠ FAIL).
- **swing_scanner/template.html**: added `.earn-soon` / `.earn-near` CSS classes; updated section 03 description to document the $30 FAIL filter

### Context
- Depends on `Tdata >= 5.10.0` (new `updateStaleEarnings()` function)
- Scanner universe: 49 of 200+ tickers showed earnings within 14 days on first run (Q1 reporting peak)

## [2026-04-16] - Auto-fetch economic events from Equals Money calendar

### Changed
- **events.R**: Replaced hardcoded weekly events with automated fetch from Equals Money calendar
  - Parses HTML (rvest) for upcoming 7-day window from report run date
  - Keyword-based impact classification (CRITICAL/HIGH/MODERATE) with ~50 rules
  - Auto-generates action recommendations per event type
  - Handles month boundaries (fetches 2 months when window spans months)
  - Fixed French locale issue: forces `LC_TIME="C"` for English month abbreviation parsing
  - No longer requires manual Monday editing
- **main.R**: Added `library(logger)`, `library(Tlogger)`, and `setup_namespace_logging("RStudies")` for proper log initialization
- **scenarios.R**: Fixed `compute_catalyst_boost()` locale bug — English month abbreviations failed on French locale systems

## [2026-03-31] - Fix conda race condition in breadth + events week Mar-30

### Fixed
- **breadth.R**: Isolate TEMP/TMP per parallel worker before `library(Tdata)` to prevent conda temp file race condition (`__conda_tmp_*.txt` locking errors with 6 cores)

### Changed
- **events.R**: Updated to week of Mar-30 — Apr-03 (German CPI, Eurozone Flash CPI, ISM Mfg, NFP, ISM Services, Good Friday)

## [2026-03-26] - BOT breakout scoring with Setup/Breakout phases

### Added
- **scoring.R**: New `score_breakout()` function with 10 criteria split into 2 phases:
  - SETUP (S: 0-6): trend (S1 price>MA50, S2 MA50 slope), accumulation (S3 RS>0, S4 OBV rising), consolidation (S5 squeeze<0.65, S6 vol decline<0.90)
  - BREAKOUT (BK: 0-4): momentum (BK1 RSI>50 rising, BK2 up/down>1.1), confirmation (BK3 rng_pct>=70, BK4 vol surge>=1.2x)
- **indicators.R**: New BOT indicators: `squeeze_ratio` (range_20/range_40), `vol_decline` (vol_20/vol_50), `vol_surge` (vol_today/vol_20avg), `high40`, `low40`
- **main.R**: BOT scoring integrated into stock loop — outputs `BOT_Score`, `BOT_Setup`, `BOT_Breakout`, `BOT_Squeeze`, `BOT_VolDec`, `BOT_VolSurge`, `BOT_Flags`
- **render_html.R**: New `build_bot_section()` — dedicated BOT signals table independent of long/short scoring. Color: green (S>=5 BK>=3), amber (partial), grey (no signal). Vehicle hint (call/spread/stock) based on price and IVP.
- **template.html**: New section "03 BOT Breakout Signals — LONG only" with dedicated legend

### Changed
- **template.html**: Section 01 methodology rewritten to describe Setup/Breakout phase scoring instead of old 2-gate system
- **template.html**: Removed Transitions section (noise, low signal)
- **template.html**: Removed Trade/Watch signal tables (replaced by BOT section as primary signal source)
- **template.html**: Sector gate hides non-tradeable sectors (US Stocks, China stocks, Forex)
- **render_html.R**: `SIGNAL_COLS` includes BOT column with Setup/Breakout display
- **render_html.R**: `build_sector_rows()` filters out VXX/QQQ/FXC/Forex sectors
- **template.html**: BOT colors use high-contrast palette for colorblind accessibility (green #1a8a1a / amber #E69F00 / grey #f0f0f0)

## [2026-03-26] - Macro context calibration, bias labels, section reorder

### Changed
- **scenarios.R**: Sigmoid parameters calibrated from 10yr historical medians/SDs instead of manual guesses. Signals now use full 0-1 range (0.5 = historically normal). Major shifts: vix_stress center 25→16.65, rates_press center 4.5→2.66, dxy_strength center 2→0.07, credit_stress center 1→-0.16
- **scenarios.R**: Liquidity Stress weights rebalanced — vix_stress 0.25→0.35, backwardation 0.25→0.05, breadth_bear/credit_stress 0.20→0.25, negative drags reduced
- **scenarios.R**: Softmax temperature 2.0→0.8 for more decisive scenario probabilities
- **backtest_regimes.R**: Updated CURRENT_PARAMS and compute_signals to match new calibration
- **analyze.R**: Bias explanation labels: "backwardation"→"VIX backwardation", "contango"→"VIX contango", "long mismatch"→"long sector mismatch"
- **analyze.R**: Mismatch notes: "flat RS:"→"flat vs. MA20 RS vs. SPY:", "RS:"→"RS vs. SPY:" for clarity
- **render_html.R**: Mismatch meta: "RS:"→"RS vs. SPY:"
- **template.html**: Section numbers swapped — Market Scenarios is now 07, Events is now 08 (matching visual order)

### Added
- **calibrate_from_history.R**: New script to derive sigmoid center/scale from 10yr Yahoo historical data (VIX, TNX, DXY, TLT, HYG, CPER, GLD, USO). Outputs comparison table and copy-paste snippet for scenarios.R

## [2026-03-24] - Fix VIX 20d ago lookback bugs

### Fixed
- **macro_context/analyze.R**: VIX 20d ago showed wrong value due to two bugs:
  - `get_series()` returned duplicate rows for same date, shifting the lookback window
  - Off-by-one: `nrow - 19` selected 19 trading days ago instead of 20
  - Added `!duplicated(date, fromLast = TRUE)` dedup and corrected index to `nrow - 20`

## [2026-03-23] - Scanner universe expansion, RVP column, sector cleanup

### Added
- **vol_profile.R**: Added RVP (30d realized vol percentile) column to Gate 3 results — was computed by getVolMetrics but dropped before output
- **render_html.R**: RVP column in HTML signal tables with tooltip

### Changed
- **ScannerUniverse DB**: Added 47 tickers from Tickers table (all IV=YES, price ≤$500) — total 208 active symbols
- **ScannerUniverse DB**: 2 new sectors: Communications (XLC + 5 stocks), Consumer cyclical (XLY + 3 stocks)
- **ScannerUniverse DB**: Sector names aligned with Tickers table (singular form): Agricultural, Basic Materials, Consumer non cyclical, Financial, Industrial, Precious Metals
- **Tickers DB**: Fixed "Consumer non-cyclical" → "Consumer non cyclical" (hyphen removed)
- **macro_context/breadth.R**: Cap parallel cores at 6
- **macro_context/events.R**: Updated to week of Mar-24

## [2026-03-20] - Regime backtest, scanner simplification, UI overhaul

### Added
- **backtest_regimes.R**: Backtest of regime signals against 98 closed BOT trades (2022-2026). Conclusion: macro signals have near-zero predictive power for BOT trade outcomes (best r=0.16, not significant)
- **scenarios.R, positioning.R, macro_outcomes.R**: Regime detection system (3 regimes: Liquidity Stress, Directional Flow, Neutral) with continuous sigmoid signals, COT positioning, macro surprise modifiers
- **vol_profile.R**: Optionality gate with IV data from Prices DB
- **history.R**: Scanner result persistence, transitions, alerts
- **final_filter.R**: 2-gate composite ranking system
- **scoring.R — L5c/S5c criterion**: Room-to-run check — price must be >1 ATR from 20-day support (short) or resistance (long) to score the point. Prevents entries at exhausted levels (e.g., ABBV at support)
- **macro_context stress banner**: VIX >= 25 triggers a prominent red warning banner at top of macro report
- **Sector gate macro context**: Each sector row now shows active tailwinds (green) and headwinds (red) from macro flags, plus mismatch type
- **Bias explanation**: Macro report bias bar shows brief summary of what drives the daily bias (e.g., "Long: contango, 10Y 4.3% | Short: breadth 27%")
- **Scheduled task**: `\RApplication\RunScanner` runs `run_scanner.bat` daily at 09:00 CET

### Changed
- **Regime system downgraded**: Sigmoid calibration effort abandoned after backtest. Regime system retained for sector flow scoring but no longer used as a trade gate. Replaced by simple VIX > 25 warning
- **Scanner: 3-gate → 2-gate system**: Removed macro score gate (no predictive power). Gates are now: (1) Technical Analysis, (2) Optionality
- **Composite score simplified**: `tech_score + persistence` only (removed macro_score and opt_score components)
- **Optionality gate unified**: Gate3 renamed to Optionality, criteria aligned with display: IV30<40 + IVP<60 + VRP<0 + Contango. PASS (3-4), PARTIAL (2), FAIL (0-1)
- **Scanner table split**: Single stock signals table replaced by two sections: Trade Signals (top 3 picks) and Watch Signals, each with LONG/SHORT direction badges
- **Columns cleaned up**: Removed Macro_Score, Scenario_Ann, Long_Signal, Short_Signal, Best_Signal, Score, Target, ATR_pct from display. Added clear column grouping: technical (ADX10, RSI14, RS_vs_ETF) then optionality (IV30, RV30, IVP, VRP, Optionality, TermStr)
- **Sector gate display**: Replaced confusing Long Gate/Short Gate text with clear LONG/SHORT/BLOCKED direction badges + ETF status line
- **Methodology card**: Expanded from 4 bullet points to 4 themed sections (Relative Strength, Trend Structure, Momentum & Asymmetry, Volume Confirmation) with full criteria descriptions
- **Price cap**: Raised from $300 to $500. Deactivated 14 stocks above $500 from ScannerUniverse
- **Reports date/time**: Both reports now show date + time (e.g., "20 March 2026 — 09:00"), left-aligned
- **Macro report title**: Removed "Step 1 Dashboard" subtitle

### Database
- **ScannerUniverse**: Added CF (Agriculture), TNK and STNG (Energy). GLD set as PreciousMetals ETF. 14 stocks >$500 deactivated
- **backtest_regime_signals**: New table with trade-level signal values and outcomes from backtest
