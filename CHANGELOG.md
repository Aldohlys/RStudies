# Changelog - RStudies

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [2026-05-12] - analyze: surface IV30+IVP and RV30+RVP in cheap-score table

### Why
The C.1 cheap-score table showed only IVP (percentile). To judge vol attractiveness the user also needs to compare with RV30/RVP — realized vol and where it sits in its own 1y history.

### Changed
- **reports/shared/live_sources.R**: new `resolve_rvp()` — mirrors `resolve_ivp`. DB Prices.rvp first if fresh → live via `Tdata::getVolMetrics`.
- **reports/analyze/funnel.R**: funnel pulls rvp via the new resolver; exposed as `funnel$rvp`.
- **reports/analyze/phases.R**: `.compute_cheap_components` carries iv30/rv30/rvp through to the report.
- **reports/analyze/report.R**: C.1 table:
  - Renamed `IVP component` row to `IV30 / IVP component` — now shows both numbers side by side (IV30 as %, IVP as percentile). Scoring still based on IVP alone.
  - New `RV30 / RVP (info)` row — informational, no points. Shows current RV30 and its 1y percentile rank.

### Validated on GM short
- IV30 / IVP: `IV30=32.9% · IVP=52.9%` → 2/4 pts
- RV30 / RVP: `RV30=34.0% · RVP=64.9%` — realized leading implied; elevated vs its own history.

## [2026-05-12] - analyze: outright option pricing grid in report

### Problem
When the vehicle rule picked `call` or `put` (outright single-leg), the report showed the spread enumeration but not the actual outright pricing grid. User had to read the entry framework's single-strike R:R but couldn't compare strikes or expiries for outrights.

### Added
- **reports/analyze/structures.R::enumerate_outrights()**: new helper. Mirrors `enumerate_structures()` but for single-leg long options. Enumerates a strike × expiry grid (4 strikes per direction × 2 expiries). For each: BS-prices entry at current spot + IV30, BS-prices forward at effective target + (DTE−5 theta buffer) + (IV+2pp bump). Returns rows with expiry/dte/strike/entry_premium/fwd_premium/max_loss/reward/rr.
- **reports/analyze/report.R::.render_outright_table()**: renders the grid as a sortable HTML table above the spreads section. `$` suffix for currency, 2-decimal rounding, expiry with DTE annotation.
- `run_phase_d` always invokes `enumerate_outrights()` when TWS reachable; result stored in `phase_d$outrights` and rendered above the spreads. Visible regardless of vehicle rule's preference (informational).

### Validated on UPS short
Outrights grid surfaced 8 rows (4 puts × 2 expiries):
- Best R:R: $90 put / 45d → entry $60, fwd@$99 = $76.10, R:R 0.27.
- ATM $100 put / 31d → entry $305.60 (max loss), fwd $351.90, R:R 0.15.

## [2026-05-12] - analyze: direction-aware vehicle rule, always-open structures, fix entry framework strike-picker

### Problem
First live UPS-short run after the redesign exposed three carry-forward bugs from the long-only era:
1. Vehicle rule labelled `call` for shorts (should be `put`).
2. When vehicle ≠ `spread`, structures table was wrapped in a collapsed `<details>` — friction for users who want to see the spread enumeration regardless.
3. `.live_entry_framework` picked an ATM **call** strike for shorts and BS-priced it as a long call — produced nonsense R:R = −0.07 (vs the correctly-priced bear-put spread that came back at R:R = 0.16).

### Changed
- **reports/shared/vehicle_rule.R**: `pick_vehicle_expiry()` accepts `direction = "long" | "short"`. Outright-option vehicle now labelled `put` for short, `call` for long. Default `"long"` preserves swing_scanner caller behavior.
- **reports/shared/setup_chain_rr.R**: `compute_rr_entry()` accepts `direction`. BS forward pricing uses `type="Put"` for short outrights and bear-put debit spreads, `type="Call"` otherwise. Bear-put `max_payoff = long_strike − short_strike` (vs bull-call `short_strike − long_strike`).
- **reports/analyze/structures.R**:
  - `pick_vehicle_expiry` called with `direction`. Scanner-CSV vehicle override removed (long-only).
  - `.live_entry_framework` strike picker direction-aware: short outright = put rounded DOWN to $5 grid; spread for short = long_put > short_put (bear-put). BS pricing uses `type="Put"` for shorts.
- **reports/analyze/report.R**: `.render_structures()` no longer wraps the spread table in `<details>` when vehicle ∈ {`call`, `put`, `stock`}. Vehicle rule is informational, not gating. Heading now reads "Vertical spreads — DEBIT only, within $N/lot cap" with a one-line hint when vehicle is non-spread.

### Validated on UPS short
| | Before | After |
|---|---|---|
| Vehicle label | `call` | `put` ✓ |
| Structures table visibility | collapsed `<details>` | always open ✓ |
| Entry framework R:R | −0.07 (mispriced call) | 0.16 (bear-put debit) ✓ |
| Top spread by EV | $93/$88 31d, EV=−$11.50 | $93/$88 31d, EV=−$13.00 (unchanged shape) |

## [2026-05-12] - analyze: full per-phase redesign — live-first sourcing, direction-aware targets, filtered structures

### Problem
`/analyze UPS short` on 2026-05-11 surfaced wrong-direction structural targets ($109/$110 above a $100 spot), IVP=`FETCH FAILED` despite a Tdata helper existing, LONG-only `sector_rs_rank`, and a 25-row structures table mixing CREDIT/DEBIT with phantom rows (RR=32x from BS pricing rounding both legs to zero). The data-only neutralization (April 2026) had stripped synthesis but kept stale data paths and direction-blind logic.

### Added
- **reports/shared/live_sources.R** (new, ~450 lines): per-field resolver module with uniform `list(value, source, retrieved_at, reason)` shape. Resolvers: `resolve_spot`, `resolve_sector`, `resolve_sector_etf`, `resolve_expiry`, `resolve_iv30`, `resolve_iv90`, `resolve_rv30`, `resolve_ivp` (closes the IVP gap via `Tdata::getIVPercentileLevels` + linear-interp), `resolve_skew_25d`, `resolve_chain_oi`, `resolve_earnings`, `resolve_returns`, `compute_sector_rs_context`. Escalation: live IBKR / yfinance primary → DB-cache-if-fresh → scanner CSV last-resort.
- **reports/shared/indicators.R**: `ret60` column in `calc_ind` (for stock-vs-sector RS at 60d).
- **reports/shared/setup_chain_rr.R**: `compute_structural_target()` now accepts `direction`. Long path unchanged; short path mirrors with prior swing **lows**, 52w **low**, round number **below**, fib retracement **down**. New `.structural_target_short()` and `.nearest_round_below()`.

### Changed
- **reports/analyze/phases.R**:
  - **Phase A** demoted from gate to INFO-only — never SKIPs downstream. Live IBKR `getExpirationDates` probe → DB `scanner_rich_universe` → scanner CSV.
  - **Phase B** rewritten: `pull_score` / `stage_pts` / `sector_pts` / `footprint_pts` dropped (triple-counted the per-indicator breakdown). New output: stage label (mechanical, MA50-based) + direction alignment + sector + sector ETF + stock-vs-sector RS 20d/60d + sector-vs-SPY RS + **direction-aware** `sector_rs_rank` (long descending = strongest, short ascending = weakest).
  - **Phase C** rewritten: cheap_score always live from funnel, max corrected `/10 → /9`. All 4 components (IVP/4 + VRP/2 + Term/2 + RR/1) surfaced with points / max / value / band. New `.compute_cheap_components()`.
  - **Phase E** no longer gates on Phase A; `phase_of_drop` ∈ {B, C, D, none}.
- **reports/analyze/funnel.R**: entirely rewritten around resolvers. IVP cell now renders `47.8% (live interp) | mid` instead of `FETCH FAILED`.
- **reports/analyze/structures.R**:
  - Scanner CSV reads removed (CSV was LONG-only and can't be reused for shorts).
  - `.live_targets()` direction-aware.
  - `effective_target` switches to `oi_cap_put` for shorts (was always `oi_cap_call`).
  - `enumerate_structures()` accepts `expiries` vector — two expiries enumerated side-by-side (~30 DTE and ~55 DTE). Each row carries explicit `expiry` column.
  - Post-filter: DEBIT-only for direction + within_cap=TRUE + max_risk ≥ $5 (drops phantom rows). Sort by `expected_value` desc.
- **reports/analyze/report.R**:
  - Phase A renders as informational (n_expiries + tradeable_expiries). No badge. Not in Phase E summary table.
  - Phase B aggregate table replaced by direction-aware trend + sector RS context.
  - Phase C.1 shows the 4-component breakdown with bands.
  - Structures table fully reformatted: drop `spread_type` column, `$` suffix for currency, `%` for prob/edge, 2-decimal rounding, expiry column with DTE annotation.
  - Data Summary updated for new field shape.
- **reports/analyze/main.R**: sources new module; log messages match new shape.

### Validated (UPS short, 2026-05-12)
| Field | Before (2026-05-11) | After |
|---|---|---|
| Phase A | STALE (CSV 6d old, SKIPped pipeline) | INFO (live IBKR probe) |
| IV Rank 1Y | `FETCH FAILED: DB Prices.ivp NA` | `47.8% (live interp)` |
| cheap_score | `5/10` (IVP missing) | `7/9` (all four components surfaced) |
| Funnel tally | 4 fav / 0 unfav / 2 unav | 4 fav / 1 unfav / 1 unav |
| sector_rs_rank | LONG-only (n/a for short) | 12/19 (direction-aware) |
| Stock vs Sector ETF 20d | (not surfaced) | −3.32% (laggard) |
| Stock vs Sector ETF 60d | (not surfaced) | −16.63% (deep laggard) |
| spot_target_low / high | $109.84 / $110.00 (wrong-side bug) | $94.06 / $90.00 (downside) ✓ |
| Structures table | 25 rows, CREDIT+DEBIT, phantom RR=32x | Pre-filtered DEBIT-only, within-cap, EV-sorted |

## [2026-05-05] - analyze: click-to-sort headers on spread structures table

### Changed
- **reports/analyze/report.R**: spread enumeration table now `<table class="sortable">` with inline vanilla-JS click handler on every `<th>`. Numeric vs text auto-detected per column; NaN/empty cells sink to bottom regardless of direction. Up/down arrow indicator on the active column. No dependencies — no DataTables, no CDN — report stays self-contained.

### Why
78-row spread enumeration was hard to scan unsorted (default order: live pricer's reward_risk_ratio descending, but users want to drill by max_risk, EV, prob_success_delta, etc.).

## [2026-05-05] - analyze: format OBV slope as M-shares + % of 20d volume

### Fixed
- **reports/shared/indicators.R::compute_breakdown**: `S4 OBV slope (20d)` was rendered as a raw 9-digit cumulative-volume count (e.g. `937612100`). Now displays signed M/B/K with % of 20d total volume in parens — e.g. `+77.6M (+8.3% of 20d vol)`. Threshold check ("> 0 = accumulation") unchanged; only display formatting updated.

## [2026-05-05] - analyze: Phase D always lives — re-derive targets / R:R when scanner is silent

### Problem
When the scanner short-circuits at Phase B or C (TOP PICK / WATCH gates fail), the CSV row exists but Phase D fields (spot_target_*, targets_agreeing, fib_confirms, effective_target, rr, entry_floor / entry_ceiling, headroom_band, entry_state) come back NA. /analyze previously surfaced these as `FETCH FAILED: scanner did not emit ...`. Per design intent, /analyze should always surface every phase's data regardless of upstream drops.

### Changed
- **reports/shared/setup_chain_rr.R** (new): `compute_structural_target()`, `walk_chain_oi()`, `compute_rr_entry()`, `classify_entry_state()` lifted from `swing_scanner/setup_chain_rr.R` so /analyze can re-use them.
- **reports/swing_scanner/setup_chain_rr.R**: now a one-line shim sourcing the shared module (zero behavioral change for scanner).
- **reports/analyze/structures.R::run_phase_d**: when the scanner row lacks targets, calls new `.live_targets()` — fetches 300d OHLC via `fetch_single_ohlcv()`, runs `compute_structural_target()`, returns the same shape. When the scanner row lacks the entry framework, calls new `.live_entry_framework()` — picks strikes off rounded grid (±$5), prices via Black-Scholes (`Tbasics::getOptPrice`), runs `compute_rr_entry()` + `classify_entry_state()`. IV pulled from `phase_c$funnel$iv30` with 0.30 fallback.
- **reports/analyze/report.R::.render_phase_d**: caption "Targets re-derived live from OHLC history (scanner did not emit)" when live path was used.

### Validation
AAPL long (scanner phase_of_drop=B) before/after:

| Field | Before | After |
|---|---|---|
| spot_target_low / high  | FETCH FAILED | 280.91 / 288.62 |
| targets_agreeing         | FETCH FAILED | 3 |
| fib_confirms             | FETCH FAILED | TRUE |
| effective_target         | FETCH FAILED | 280.91 |
| R:R                      | FETCH FAILED | 0.13 |
| entry_floor / ceiling    | FETCH FAILED | 2.48 / 1.86 |
| entry_state              | FETCH FAILED | PRICED OUT |

## [2026-05-05] - analyze + scanner: 4-phase refactor (drop _v5, live refresh, indicator breakdown, vehicle rule)

### Phase 1 — Drop version suffix from code & filenames
- Rename `swing_scanner/main_v5.R` → `main.R`, `render_html_v5.R` → `render_html.R`, `final_classify_v5.R` → `classify.R`.
- Delete legacy bundle: `scoring.R`, `final_filter.R`, `history.R`, `template.html`, `swing_scanner/indicators.R`. `score_breakout()` inlined into its sole consumer `pull_score.R`.
- Output filenames now `swing_scanner_<DATE>.{csv,html}` (no `_v5` suffix).
- DB table `scanner_results_v5` → `scanner_results`. Migration via new `RApplication/scripts/migrate_scanner_results_table.R` (idempotent, retains legacy table for verification).
- Schema version moves to internal constant `SCANNER_SCHEMA_VERSION = 5L` written as `schema_version` column in CSV + DB row, never in filenames.
- `analyze/`: every `v5_classification` / `"v5 CSV"` / `.read_v5_row` / `.find_latest_v5_csv` reference purged. Locals renamed (`v5_cheap_pass` → `cached_cheap_pass`, etc.).

### Phase 2 — Live refresh on stale data (12h cutoff)
- **reports/shared/freshness.R** (new): `resolve_freshness_policy()`, `is_fresh()`, `hours_since()`, `scanner_csv_mtime()`. CLI flags `--refresh` (force live everywhere) and `--max-age <hours>` (custom window). Default `SCANNER_DATA_MAX_AGE_HOURS = 12`.
- `analyze/phases.R::.read_scanner_row` returns `stale=TRUE` when CSV mtime exceeds policy cutoff; phase A/B/C/D signatures take `freshness` argument.
- `analyze/funnel.R`: gates `Prices.datetime` + `option_skew_history.cache_date`; "FETCH FAILED" cell reasons now distinguish `"DB Prices stale (Xh old)"` from `"DB Prices NA"`.
- `analyze/structures.R::.resolve_chain` gates `option_chain_oi_history.cache_date` similarly.
- `hours_since()` parses 9 timestamp formats including SQLite TEXT dense format (`20260428 21:14`) — earlier "Inf h old" caused by unparseable strings is fixed.
- `analyze/main.R` prints policy banner + scanner CSV mtime up front before any phase runs.

### Phase 3 — Phase B per-indicator breakdown via shared module
- **reports/shared/indicators.R** (new): `calc_ind()`, `compute_all_indicators()`, `get_last()` lifted from deleted `swing_scanner/indicators.R` (single source of truth — scanner + analyze share it).
- `compute_breakdown(last, price, direction)`: emits a 12-row data frame for /analyze Phase B drill-down — S1-S6 setup criteria + BK1-BK4 breakout criteria + AUX_ADX/AUX_RET/AUX_ATR informational rows. Mirrored thresholds for `direction = "short"`.
- `fetch_single_ohlcv(ticker)`: 300-day Yahoo pull for /analyze single-ticker case.
- `analyze/phases.R::run_phase_b` calls the breakdown live; falls back silently to NULL on any failure (aggregate row stays).
- `analyze/report.R::.render_phase_b_breakdown`: collapsible `<details open>` block with PASS/FAIL/info badges; summary shows `setup N/5 · breakout N/4`.

### Phase 4 — Conditional structures + tooltips + retrieval timestamps
- **reports/shared/vehicle_rule.R** (new): `pick_vehicle_expiry(price, cheap_score, stage, atm_bid_ask_pct)` — single source of truth. Used by both scanner (replaces inline rule in `setup_chain_rr.R`) and /analyze (re-derives when scanner CSV is silent).
- `analyze/structures.R::run_phase_d` now emits `vehicle_reason` and `structures_retrieved_at`. When the scanner row carries no `vehicle`, the shared rule is invoked with `phase_b$stage` and `phase_c$cheap_score`.
- `analyze/report.R::.render_structures` is now vehicle-aware: only the matching vehicle is rendered open by default; non-applicable structures collapse to `<details>` the user can expand. Banner shows the rule's reasoning ("price $276.83, cheap_score 3 < 7 (vertical spread for IV cost control)").
- 30+ field tooltips via `.TOOLTIPS` glossary in `report.R` — wrapped as `<span title="...">label</span>` across header / Phase A/B/C/D tables / breakdown criterion IDs / funnel signals.
- Per-phase "data retrieved" captions (`<div class="retrieved">`): Prices DB / Skew DB / live now / live pricer timestamps. Sourced from `funnel$retrieved`, `pa$retrieved_at`, `pb$scanner_csv_mtime`, `pb$breakdown_retrieved_at`, `pd$structures_retrieved_at`.
- CSS additions: `[title]{cursor:help;border-bottom:1px dotted #999}`, `.retrieved` styling, `<details>` hover + marker styling.
- `macro_context/scenarios.R`: stale comment about deleted `final_filter.R` updated.

### Migration & launcher updates (RApplication side, commit 22252d4)
- `scripts/run_scanner.bat`: drops `_v5` from code path (`reports/swing_scanner/main.R`) and HTML filename (`swing_scanner_<DATE>.html`).
- `scripts/run_analyze.bat`: pass-through for arbitrary args (no longer capped at `%3 %4`) so `--refresh` / `--max-age <hours>` reach Rscript; cleaner open-html guard via `findstr` instead of inverted string-substitution.
- `scripts/migrate_scanner_results_table.R` (new): idempotent migration adding `schema_version` column and copying rows from `scanner_results_v5`. Legacy table retained for manual verification before DROP.
- `docs/TODO.md`: path reference updated (Option B sidecar CSV name).

### Validation
- Parse-checked all 14 touched files via `Rscript -e parse(...)`.
- Smoke-tested `swing_scanner/main.R` end-to-end: 196 today rows + 188 historical rows in `scanner_results`, `schema_version` column populated.
- Smoke-tested `/analyze AAPL long` with default 12h policy (uses cached scanner CSV) and with `--max-age 0.001` forcing the stale path through every phase (Phase A reports STALE, downstream cascade live-fetches).
- HTML output verified: 15 retrieval/tooltip elements rendered; vehicle banner shows `spread` with rule reasoning; spread enumeration in `<details open>`; Phase B breakdown shows BK1 PASS (RSI 61.8 + positive slope) + BK3 PASS (75% near high) + others FAIL.

### Net change
- 21 files changed in RStudies repo (commit `0dd9a55`), +2041 / -2392 lines (net code reduction).
- 5 files changed in RApplication repo (commit `22252d4`).

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
