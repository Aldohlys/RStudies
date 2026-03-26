# Changelog - RStudies

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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
