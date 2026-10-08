# Changelog - RStudies

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [2026-10-08] - macro_context: macro fetch keeps universe symbols (FXC re-append, empty EUR.CHF)

### Fixed
- `fetch.R` now requests each universe symbol under its Yahoo name (Tickers.YahooName) and stores the rows under the universe symbol (`fetch_yahoo_as_universe()`). Tdata::getYahooData maps only 3-letter names and returns them under the Yahoo name, so FXC came back as FXC.SW: the cache check never found FXC and appended it again on every run (four copies of 64 rows on 10-08). EUR.CHF was sent to Yahoo unmapped and came back with no closes; it now reads EURCHF=X. Duplicate (ticker, date) rows are dropped when the cache is read. No report figure changes: nothing in macro_context reads either symbol by name.

## [2026-10-08] - macro_context: roll-free energy futures, empty Yahoo closes re-fetched

### Fixed
- Energy futures without roll gaps. Yahoo's CL=F, BZ=F, NG=F, HO=F and RB=F switch to the next contract at each expiry, so in backwardation each roll read as a fall and in contango as a rise. `roll_adjust()` (intermarket.R) rebuilds each series from daily returns: the listed front contract where Yahoo has it, the continuous series otherwise, a listed contract on older roll days (expired contracts are not served). Levels are rebuilt back from the front contract's last close, so Last is the real front price. The series feed the panels, the OIL scenario asset, the petrodollar chain and the sector-map drivers. Generic last-trading-day rules per root in `intermarket_config.R` (`FUT_LTD`, `fut_front_ym`, `fut_contract`), checked against Yahoo's September 2026 rolls (CL 09-23, NG 09-29, BZ/HO/RB 10-01). On 10-08 (10-07 data), 1M: WTI −4.5% → −0.8% (trend MIXED → UP), Brent +3.1% → +8.1%, natural gas +11.9% → +6.1%, ULSD +2.0% → +7.0%, RBOB −5.2% → +4.8%; Stagflation / oil supply shock state +30% → +38%, move +18% → +23%.
- Empty closes. At 09:00 on 10-08 Yahoo returned the 10-07 bar with no close for 35 symbols (European and Asian indices, Bund and gilt ETFs, European group members); `get_close()` dropped those rows, so the report showed Tuesday as the last session without a warning. `refetch_empty_closes()` re-fetches symbols whose latest weekday bar has no close and replaces their recent rows in the data and in today's cache (`cache_replace_recent()`, shared/cache.R). Symbols still empty are flagged in their panel row and in the last-session strip, and listed in an amber banner with the scenario inputs they affect. Evening re-run on 10-08: 32 of 35 filled (Euro Stoxx 50 −1.47% on 10-07 now shown); IS0L.DE, GLTL.L and NUCL.L still empty.
- Methodology: energy futures and missing-close paragraphs, two limitation notes.
- Reason: `NewTrading/Reports/macro_context_vs_ceresna_20261008.md`, gaps 1 and 2.

## [2026-10-08] - macro_context: WTI futures curve (+3 and +6 months)

### Added
- Oil panel (09): rows for the WTI contracts three and six delivery months after the front (Yahoo `CLG27.NYM`, `CLK27.NYM` on 10-08), read as fixed contracts so their changes contain no roll gaps. Front = first contract still trading on the report date (CME rule: 3 business days before the 25th of the month before delivery, 4 if the 25th is not a business day; holidays ignored). `crude_curve()` in `intermarket_config.R` picks the contracts at each run; they move up one month after each expiry.
- Relationships: front minus +3 months and front minus +6 months in \$/bbl (new panel kind `usd`: last and changes in dollars). The row text names the front's last trade date, because convergence distorts the front in its last two weeks.
- Global movie, oil paragraph: 1-month change and trend of both contracts, the front minus +6 months spread and whether the curve is flattening or steepening.
- Methodology 09 panels: how the contracts are chosen.
- Reason: the 10-07 Ceresna sessions rest on the curve (November flagging, March/April at higher highs) and the report had no curve view (`NewTrading/Reports/macro_context_vs_ceresna_20261007.md`, gap 2). First reading, 10-07 close: front −4.5%, Feb-27 +4.8%, May-27 +8.0% over 1 month; front minus +6 months \$5.25 of backwardation, −\$8.04 in a month.

## [2026-10-08] - BOT tiers: COUNTER-TREND removed, rows shown as WATCH

### Changed
- The COUNTER-TREND tier is gone from the bot_daily workbook: a row with daily trend_state 3/6 or less and asym at least 2 (not paused-trend BOT-) is WATCH. User: a falling name is not a BOT long without positive price action ("don't catch a falling knife"; SAF, MS, KO on 10-08). WATCH's legend rule says it is a level to watch, not an entry.
- bot_fwd keeps simulating those rows: tier WATCH, `tier_reason` counter_trend (`C.REASONS_SIMULATED`). The weekly report groups by tier/reason (`C.GROUPS`). 114 past COUNTER-TREND signals and 101 underlying rows relabelled. On the 72 first-day signals so far, five sessions after the signal the name was lower on average (-0.4 to -0.7 ATR, up 20 of 66), and a one-day rebound on the signal bar did not improve R.

## [2026-10-08] - BOT tiers: daily trend paused inside an intact trend is BOT-

### Changed
- A tradable row with asym at least 1 whose daily trend_state is 3/6 or less is BOT- (no longer COUNTER-TREND or WATCH) when w_trend_state is at least 4/6 and the close is above a rising daily EMA50 (short mirrored). Case: NET 10-08, a flag breakout drifting sideways above the broken level, read 2/6 daily. Same rule in `bot_daily_xlsx.R::bot_tier` and `bot_fwd/common.py::tier_reason`. On the 10-08 17:13 run it moves NET (COUNTER-TREND), BIIB and ARM (WATCH) to BOT-.
- bot_fwd: new column `bot_fwd_signal.tier_reason` (daily_trend / weekly_trend_hold / counter_trend), not shown in the workbook; backfilled from each signal's stored row. NET 10-08 reclassified BOT-. Runs before 10-08 lack w_trend_state, so the rule cannot fire on them.

## [2026-10-08] - bot_daily: workbook with legend instead of CSV

### Changed
- BOT_daily writes `NewTrading/Reports/bot_daily_<date>_<hhmm>.xlsx` instead of `bot_daily_<date>_<hhmm>.csv` and the `bot_daily_detail/` sidecar. Sheet Data: `tier` plus the daily columns, rows shaded by tier, filter on the header row. Sheet Detail: every field. Sheet Legend: tier rules with counts and a plain-words definition of every column. `reports/bot_daily/bot_daily_xlsx.R` holds the writer and `BOT_DAILY_LEGEND`; the run stops if a column has no definition. `--detail` now puts every field on Data.
- bot_fwd reads the Detail sheet (openpyxl read-only: openxlsx files link a drawing part they do not contain, which the full loader rejects); CSV runs before the switch are still read.
- `NewTrading/scripts/run_bot_daily.bat` no longer calls `bot_daily_to_xlsx.py` and opens the workbook from `Reports/`.

## [2026-10-08] - bot_fwd: stock risk floor

### Fixed
- Stock risk per share for sizing and 1R is at least 0.5 ATR (`STOCK_MIN_RISK_ATR`); the exit stop is unchanged. DUOL 09-28 had sized off 0.05 ATR (1R USD 88, +42 R).

## [2026-10-08] - macro_context: sector map workbook with legend

### Changed
- The daily BOT sector map is written to `NewTrading/Reports/intermarket_sectors_<date>.xlsx` (sheets Data and Legend) instead of `intermarket_sectors_<date>.csv`. No code read the CSV. `intermarket_xlsx.R` holds the writer and `SECTORS_LEGEND`, a self-contained definition of every column; the run stops if a column has no definition.
- `openxlsx` 4.2.9 added to the renv library and `renv.lock`.

## [2026-10-08] - macro_context: state and move scores

### Added
- State score per scenario: same weights applied to each asset's position in its 252-session range (`asset_states`, `state_score` in `archetypes.R`). State decides "in place" (`STATE_ACTIVE` 0.50); the 1-month move gives the direction (`SCEN_ACTIVE` 0.40). Statuses BUILDING, ESTABLISHED, MATURE, FADING, EMERGING, FADED, INACTIVE; scenarios ranked by state.
- `compare_state_move.py`: move vs state vs mix on the calibration episodes, timing, state threshold sweep.
- Column `state_score` in `macro_intermarket_scenarios` (added by `main.R` when missing).

### Changed
- Section 00 cards, table, sparklines (state solid, move dotted) and movie text show both scores; Methodology tab M1 rewritten.
- `HISTORY_DAYS` 450 -> 520 so the 60-day history has full 252-session ranges.

## [2026-10-08] - macro_context: credit-event scenario, credit stress on HYG/IEF

### Added
- Scenario **Credit event / recession bear market** (`archetypes.R`); checked on five episodes 2007-2023 in `calibrate_scenarios.py` (held-out separation 0.86, kept as written).

### Changed
- Regime signal `credit_stress` (and the credit part of `sentiment`) reads the 20-day return of HYG/IEF on adjusted closes instead of HYG's price, which also fell with Treasury yields; sigmoid recalibrated on 2016-2026 (center -0.52, scale 2.34). `calibrate_from_history.R` aligned.
- IEF added to the macro tickers; `fetch.R` appends tickers missing from the day's cache instead of returning the cache as is.

## [2026-10-08] - macro_context: new scenarios, absolute breadth, carry alert, weight calibration

### Added
- Scenarios **Leadership unwind / rotation out of the leaders** and **Policy pivot / easing rally** (`archetypes.R`).
- Fingerprint assets: Russell 2000 vs S&P 500 (`IWM/^GSPC`), 3-month bill yield (`^IRX`), and absolute breadth (`ABS_BREADTH`, share of S&P 500 stocks above their 50-day average, entered as (level - 50) / 15; history from `macro_context_results.s5fi`, each morning reading assigned to the previous session).
- Yen carry-unwind alert on 3-session z-scores (FIRING / WATCH / QUIET), shown in section 00 under the chains; 2019-2026 check: 19 firing sessions in 6 yen-up risk-off episodes.
- `calibrate_scenarios.py`: rebuilds daily scenario scores from 2004, measures separation of each scenario's dated episodes, threshold table and constrained leave-one-episode-out re-weighting. Results: `NewTrading/Reports/scenario_weight_calibration_20261008.md`.
- Methodology tab (`methodology.R`): every computation in the report, generated from the live config.

### Changed
- Weights from the calibration: policy pivot (10-year -2, 3-month bill -1, VIX -2, curve dropped), stagflation (10-year +1.5; gold, curve, breadth, VIX, EM FX, credit dropped), goldilocks (equal weight vs S&P dropped), wrecking ball (oil, VIX dropped), bond rout (credit dropped), debasement (10-year, curve dropped). In-place threshold 40% kept.
- Absolute breadth added to dash for cash, bond rout, stagflation, reflation, goldilocks, growth scare and narrow leadership; Russell vs S&P to reflation and growth scare.
- Regime inertia and softmax temperature are named constants (`REGIME_INERTIA`, `REGIME_TEMPERATURE`).

### Removed
- Yen carry-trade unwind as a 21-day scenario (replaced by the short-window alert).

## [2026-10-08] - bot_fwd: expiry, bid-ask and exit decisions

### Changed
- Monthly-only chains: nearest monthly up to 49 DTE when no expiry falls in 28-42.
- Bid-ask availability limit 20% of mid (was 15%).
- P0 no longer exits when asymmetry is gone (`no_asym`); P7 = P0 + that rule. Policy version v2-2026-10-08.
- Strike selection: when no strike has delta 0.25-0.35, the one closest to 0.30 within 0.20-0.40 (coarse grids, MET 10-07).
- Spreads priced long leg at ask (entry) / bid (exit), short leg at mid both ways, instead of the combo natural.
- Earnings date at entry from Yahoo's earnings history (past and upcoming), so backfilled entries see reports inside their hold (MU 09-30, NKE 10-01).
- Report column "R now" renamed "R at last close".
- Entry asymmetry and extension recorded per position (`px_v`, `entry_asym`, `entry_ext_atr`; report section "Entry extension and asymmetry"); filled for earlier positions on the next run. Index proxies now scaled from the proxy's last close before the session instead of the entry price, so their extension is measurable (targets and stops of the 10 proxy positions re-scaled).
- bot_daily files without an `atr` column (09-25 .. 09-29 09:21): ATR14 computed at entry from IBKR daily bars, and in the shadow from Yahoo bars, instead of skipping the signal (57 signals recovered in the backfill from 09-21).

## [2026-10-08] - bot_fwd: BOT forward test

### Added
- **`reports/bot_fwd/`** (Python): simulates every BOT / BOT- / COUNTER-TREND signal of `bot_daily` with a 30-delta call and a bull call spread, 28-42 DTE, 1 lot, at IBKR ask/bid with fees calibrated from `Trades`; long stock when neither option vehicle passes the bid-ask test. Exits replayed from daily marks under eight policies (P0 = trading plan rules, P1-P7 variants). Design: `NewTrading/Strategies/Breakouts/bot_forward_test_proposal_20261007.md` (§9 = implementation notes).
- Tables in `mydb.db`: `bot_fwd_signal`, `bot_fwd_position`, `bot_fwd_contract`, `bot_fwd_mark`, `bot_fwd_result`, `bot_fwd_underlying`.
- Entries are live quotes when the run is fresh and in US hours, else rebuilt from IBKR 5-minute `BID_ASK` bars at the entry time; marks from daily (stock) and 1-hour (option) bars, since IBKR serves no daily option bars. At most 5 history requests in flight, 90 s timeout; a timeout is retried on the next run, never read as an empty quote.
- Signals already through their stop at entry are skipped (`spot_through_stop`).
- Weekly report `NewTrading/Reports/bot_fwd_<Monday>.md`; `test_policy.py` checks the exit engine on synthetic marks.

### Changed
- **`bot_daily/main.R`**: a default run also writes every field to `bot_daily_detail/<same file name>`, read by the forward test at entry.

## [2026-10-06] - macro_context: reading principles aligned on Ceresna's five tenets

### Changed
- **Reading principles** (`template.html`): five principles instead of four, following Ceresna's Macro Outlook 2026-10-05 (order: present, value, liquidity, consensus, actors). The former principle 2 merged his long-term mean reversion and short-term liquidity tenets and narrowed liquidity to "liquidity stress"; they are now separate.
- Principle 2 (reversion to fundamental value) is a warning for BOT's holding period, not a signal: valuation is no reason to fade a breakout or exit early.
- Principle 3 (liquidity) reads both directions: tightening hits small caps, equal weight and breadth first and crowds flows into a few leaders; loosening broadens participation.
- Consensus principle restores "almost always"; actors principle restores currencies and commodities.

## [2026-10-05] - macro_context: BOT sector map per correlation group

### Changed
- **Section 10 BOT sector map** (`intermarket.R::group_map()`, `analyze_sectors()`): rows are the correlation groups (`ScannerUniverse.Cluster`, cluster review 2026-10-02) that hold at least one `Tickers.BOT_Eligible` name — 55 groups on 2026-10-05 — instead of the 33 hand-listed ETFs of `SECTOR_GROUPS`, which were keyed on the `bench` column of `tradable_universe_20260827.csv`.
- Why: the two keys did not line up. One ETF row covered several groups that do not move together (SMH = semis hardware + semis equipment + networking; IGV = cloud + speculative growth + e-commerce; SLX = steel + aerospace + copper), and one group was spread over several rows (speculative growth & crypto over IAI / IGV / CARZ / QQQ / IBIT; nuclear & critical minerals over URA / LIT / REMX / XLB). BOT's S3 already reads the groups (`shared/bot_read.R`); the map now reads the same ones.
- **Every group is its equal-weight index** (`ew_close()`: mean daily dividend-adjusted return of all members, compounded; a day counts when at least half the members trade; daily returns beyond ±50% dropped as bad prints), never its anchor ETF. An anchor outside the group is the nearest universe ETF, which serves two groups (ITA: defence primes and commercial aerospace read identically) or another industry (ITB for machinery). Group members are added to the intermarket fetch (287 symbols).
- **S3 aligned** (`shared/bot_read.R`): `.bot_group_bench()` gives every grouped name the peer median of its other members (`rs_bench = peers:<group>`); `bot_group_rotation()` (`grp_rs` / `grp_rank`) uses the members' median for every group. Outside anchors (SOXX, IAK, XLF, ITA…) are no longer read. Rotation rank rerun with members only (`NewTrading/Strategies/Breakouts/group_rotation_test.py`, old rule behind `--anchor-rule`): top 3 vs rest +0.32 ATR over 10 sessions (t 3.4) against +0.28 (t 3.1) with anchors, same 245 dates.
- **Drivers** move to `GROUP_DRIVERS` (`intermarket_config.R`), keyed by group name; the map stops with an error naming any BOT group without an entry, so a renamed or new group after a cluster review has to be given drivers.
- **/analyze Phase B aligned** (`shared/live_sources.R::compute_sector_rs_context()`): stock vs group (20d / 60d), group vs SPY and the group rank use the median return of the members (the name itself left out of its own benchmark; a member beyond ±50% in 20 sessions dropped), not the anchor. `resolve_sector_etf()` and `.ret20_batch()` removed; `.ret_batch()` returns ret20 and ret60 for every grouped name from one Yahoo call (~2 min for 340 names, against one call on ~45 anchors before). Context fields renamed `etf_sym` / `etf_ret20` / `etf_ret60` -> `bench` / `grp_ret20` / `grp_ret60` (+ `n_peers`); Phase B `sector_etf` -> `sector_bench`. Report labels: "Stock vs group", "Group vs SPY", "benchmark: median of N other members".
- The table shows each group's members and BOT-eligible / total names; BOT names in no group are listed under it with their `BOT_Bench`.
- Scenario cards (`verdict_tag()`): a scenario ETF shows the verdict of every group tagged with it (the anchor, or the `BOT_Bench` most members carry), one badge per group with its name on hover.
- **New panel "US sectors (SPDR)"**: the 11 sector SPDRs and each one / S&P 500, replacing the parent-sector rows (XLK, XLB, XLY, XLV) that held no BOT group. FXI and EWL move to World markets.
- `macro_intermarket_sectors` keeps its columns (`bench` = `EW`); basis, members and tags go to the daily CSV only.

## [2026-10-05] - macro_context: headline bias from the regime model

### Changed
- **Bias** (`synthesize(scenario_scores)`, now in `scenarios.R`; the points version in `analyze.R` is removed): the dominant regime sets the bias when it is >= 40% and >= 15 points above the next regime. Liquidity Stress -> `DEFENSIVE` (RED), Directional Flow -> `LONG BIAS` (GREEN), otherwise `NEUTRAL`. The explanation lists the dominant regime's signals by pull, weight * (signal - 0.5): "For" and "Against". Section 06 Synthesis shows the three probabilities, the lead and each signal's pull.
- Why: the points (VIX level, VIX9D/VIX3M, 10Y level, breadth band, mismatches) sat at exactly 3/3 on 13 of 43 runs, including every run 2026-09-24..10-05, while the regime panel on the same page had Liquidity Stress dominant at 46-49%. Calm vol was counted twice (3 long points), and breadth 20-35% scored short under an "oversold — reversal watch" label.
- `macro_context_results.long_pts` / `short_pts` are written as NA; `bias` and `bias_zone` keep their meaning for DEFENSIVE / NEUTRAL / LONG BIAS.

### Known limitation
- On the 41 stored regime days Directional Flow never exceeded 33.6% (mean 25.2%), so `LONG BIAS` cannot occur with the current `REGIME_WEIGHTS`; the April-September days the points called LONG BIAS have Neutral dominant. Rates (`rates_press`) are not among the Liquidity Stress inputs, so the 10Y never appears as a driver. Both belong to the regime-weight review (TODO 105).
- Replayed on history: DEFENSIVE on 17 days (2026-03-26..04-07, 2026-09-26 onward), NEUTRAL on 24.

## [2026-10-05] - macro_context: curve row says bear or bull steepening

### Changed
- **3M/10Y spread row** (`analyze_rates()`, `render_html.R`; was labelled 2Y/10Y, but `^IRX` is the 13-week bill): the level labels no longer name a cause ("Strong steepening — reflation" -> "Steep slope"). A new `sp_shape` reads the 20-day move: steepening or flattening (spread change >= 5bp), bear or bull (average of the 10Y and 3M changes rising or falling), with the 10Y and 3M changes shown. Colour: RED bear steepening (long end selling off), GREEN bull steepening, ORANGE flattening; level colour when the shape is stable.
- Over 2026-09-24..10-02 the row reads RED bear steepening (10Y +0.44 to +0.52 in 20 days, 3M +0.21 to +0.39), where it showed GREEN "Strong steepening — reflation"; BPT's macro outlook described the same days as a bond rout.

## [2026-10-05] - macro_context: VIX/VIX3M read the right way round

### Fixed
- **VIX/VIX3M row** (`analyze.R`, `render_html.R`): a low ratio (VIX below VIX3M) is contango, but it was labelled "Mild backwardation" (RED, < 0.85) and a high one "Healthy contango" (GREEN, > 1.10). Now GREEN < 0.90 contango, ORANGE 0.90-1.00 flat, RED >= 1.00 backwardation. 10-year distribution: median 0.879, p75 0.937, p95 1.023.
- **`backwardation` regime signal** (`scenarios.R`): `sig(1 - VIX/VIX3M, 0.12, 0.08)` rose in calm contango. Now `sig(VIX/VIX3M - 1, -0.12, 0.08)` (= 1 - the old value): 0.5 at the 10-year median, ~0.82 when VIX reaches VIX3M. `calibrate_from_history.R` re-run on the new measure: centre -0.121, scale 0.079, unchanged. Regime weights unchanged (+0.05 Liquidity Stress, -0.05 Directional Flow); on 2026-10-05 Liquidity Stress moves 48.0% -> 47.5%.
- `backtest_regimes.R` and `calibrate_from_history.R` use the same measure.

## [2026-10-05] - macro_context: VX1-VX2 futures spread row (TODO 56)

### Added
- **`fetch_vx_curve()`** (`fetch.R`): front and second monthly VIX futures from CBOE's daily settlement CSV (no TWS, no login). Only monthly contracts (`VX/<month><year>`) are read: the file lists the weeklies at the front monthly's price. Contracts expiring on the settlement date are skipped. Walks back up to 10 calendar days, so a weekend or holiday run reads the last settlement; 0 rows when CBOE is unreachable.
- **VX1-VX2 sub-row** under VIX Complex (`analyze_vix(raw, vx = NULL)`, `render_html.R`): spread in vol points, zone (GREEN < -0.5 healthy contango, ORANGE -0.5 to 0, RED 0 to +0.5, DARKRED > +0.5 severe backwardation), 1d change vs the previous settlement, contract months and settlement date. Shows `NO DATA` when the fetch fails.
- Informational only: the `backwardation` scenario signal and the bias points are unchanged.

### Note
- TODO 56 gave `VX1 - VX2` with contango positive; it is negative in contango (the second month trades above the front). The bands were mirrored accordingly.

## [2026-10-05] - Swing scanner removed

### Removed
- **`reports/swing_scanner/`** (front-run option flow scanner). The last commit with it is tagged `swing-scanner-final`. Why: across 116 closed BOT option trades, spot movement explains the P&L (R^2 0.89) and vega does not (0.03). The scanner's vol-cheapness thesis was never where the money came from. Its option history tables had stopped on 2026-04-27 (`daily_option_fetch.R` was never scheduled), so Phase D had no chain data. Its trend and footprint inputs duplicate BOT gates, and its target / R:R duplicates /analyze. The only signal with measured value, the group rotation rank, is now in BOT_daily (`grp_rank`).
- **/analyze no longer reads `swing_scanner_<date>.csv`.** Removed: the loader (`phases.R`), the Phase A last-resort fallback, the Phase D OI-cap fallback (`structures.R`), the staleness banner (`main.R`) and `scanner_csv_mtime()` (`freshness.R`). Every field comes from live IBKR / Tdata, with the DB tables as cache.
- NewTrading `scripts/run_flow_scanner.bat` replaced by `run_macro_context.bat` (same RunScanner task, 09:00); `scripts/daily_option_fetch.R` and `scripts/test_v5_modules.R` removed.

### Kept
- `shared/` modules used by /analyze (`setup_chain_rr.R`, `vehicle_rule.R`, `indicators.R`, `gates.R`, `universe.R`); DB tables `option_skew_history`, `option_chain_oi_history`, `scanner_rich_universe`, `scanner_results` (history).

## [2026-10-05] - BOT: group rotation rank; swing scanner stopped

### Added
- **`group`, `grp_rs`, `grp_rank`** detail columns in BOT_daily (`bot_group_rotation()` in `shared/bot_read.R`): the correlation group's 20-session return minus SPY's, ranked across groups. Taken over from the swing scanner's `sector_pts`. Measured with `NewTrading/Strategies/Breakouts/group_rotation_test.py`: top-3 groups +0.30 ATR over 10 sessions vs groups ranked below 6 (t 2.3, non-overlapping windows); hit rate of +1.5 ATR first only +3-5 points. Reported, not gated. /analyze leaves them NULL (ranking needs every group).

### Changed
- `bot_read_row()` takes an optional `rotation` table; the ScannerUniverse group query moved to `.bot_groups_table()`.

## [2026-10-05] - BOT: S3 benchmark from the correlation group

### Changed
- **`rs20` / S3 benchmark** (`shared/bot_read.R`, used by BOT_daily and /analyze): read from the name's correlation group instead of `Tickers.BOT_Bench`. Anchor outside the group (a universe ETF) -> the anchor. Anchor inside the group (member ETF such as SMH, or a central stock such as CF) -> median 20-session return of the other members, a member beyond +/-50% left out. Ungrouped names keep `Tickers.BOT_Bench`. Group fit on the 10-02 universe, median correlation over 60 sessions: anchor 0.70, peers 0.72, `BOT_Bench` 0.63.
- `bot_bench_ret20()` caches per symbol; new `bot_row_bench_ret20()`; `bot_read_ticker_rows()` adds `bench_peers`.
- /analyze BOT read labels S3 with the benchmark used.

### Added
- **`rs_bench`** detail column after `rs20` (anchor symbol, `peers:<group>`, or the `BOT_Bench` fallback).

### Fixed
- 47 BOT-eligible names with no `BOT_Bench` had S3 abstaining; they now have a benchmark (4 left: ESTX50, TGT, STG, HYG). Member anchors are no longer their own benchmark.

## [2026-10-02] - macro_context: COT positioning dashboard (section 11)

### Added
- **`refresh_cot.R` writes `NewTrading/Reports/cot_positioning_latest.csv`**: 38 markets (energy, metals, grains, softs, equity indices, VIX, Treasuries, currencies, bitcoin) x 5 trader groups. Each group has long / short / net, week-on-week changes and the COT index (Williams: 100 x (current - min) / (max - min)) of each leg over the last 52, 156 and 260 weekly reports. Groups: managed money, commercials, legacy large spec, other, retail (disaggregated report); leveraged funds, asset managers, dealers, other, retail (TFF). Same CFTC archives as before; the history window grows to 6 past years so the 5-year index has 260 rows in January.
- **Section 11 "COT positioning"** (`cot_render.R`): speculative group per market (managed money / leveraged funds), with the other side (commercials / asset managers) and the legacy large speculators in collapsible tables. Index cells at 90+ or 10- are shaded.
- Checked against cotsignal.com: the legacy large-spec rows match exactly on 14 markets (net, open interest, 1y/3y/5y index).
- positioning.R and the regime crowding score are unchanged (still 5 assets).

## [2026-10-02] - macro_context: reading principles, commodity/credit coverage, COT staleness fix

### Added
- **Reading principles** block under the daily bias (`template.html`): Ceresna's four tenets (present, liquidity stress, consensus, actors), each pointing to the sections that cover it.
- **Panels** (`intermarket_config.R`): VIX 3-month, palladium, heating oil / diesel, cocoa, coffee, sugar, lumber. Ratios: gold in EUR and in CHF, oil services / WTI, gas producers / natural gas, diesel / crude, copper miners / copper, agribusiness / ag futures, investment grade / Treasuries.
- **Sector map**: Integrated energy (XLE, benchmark of XOM/BP) and Rare earths (REMX, benchmark of MP).

### Fixed
- **COT staleness banner fired every Friday.** A file is loaded on Saturday (T+4) and replaced the next Saturday (T+11), so it is legitimately 4-10 days old; `scenarios.R` now counts missed releases from `age_days - 4` (was 3).
- **Distribution-paying bond/credit ETFs** (HYG, LQD, IEF, TIP, TLT) use dividend-adjusted closes (`ADJUSTED_SYMBOLS`), so ex-dates no longer read as credit stress. This also feeds the CREDIT fingerprint.
- **Ratios mixing an FX leg with another asset** use an as-of join in `get_close()`. Yahoo FX bars are dated a day early during UK summer time (Sunday rows, no Friday rows), so the inner join dropped about one day in five and stretched "1M" to six weeks (gold in EUR read -7.0% instead of -1.4%).

## [2026-10-02] - Names without a correlation group are "Ungrouped"

### Changed
- **`ScannerUniverse.Cluster = 'Ungrouped'`** replaces `'Unclassified'` for scanner names with no correlation group. These names keep a well-defined `Sector`; only the group is missing. `shared/universe.R`: `UNGROUPED` (was `UNCLASSIFIED_GROUP`), `get_ungrouped()` (was `get_unclassified()`); `swing_scanner/main.R` and `shared/live_sources.R` follow. The database label changed the same day (RApplication `logs/cluster_review_20261001.sql`, which also applies the hand review of the groups: 55 groups, 336 names).

## [2026-10-01] - BOT_monthly: ATM-only bid/ask probe; no probe for names without tracked options

### Fixed
- **`bot_monthly/main.R`: a missed spread dropped the whole row.** On a miss, `.miss()`/`.nodata()` return `value = NA` (atomic), and `v$atm_bid_ask_pct` threw "$ operator is invalid for atomic vectors". The guard now tests `is.list(v)`.
- **Names with `Tickers.IV = NO` were probed for options.** Bond ETFs, SIX-only lines such as AMRZ.SW and untracked chains produced errors, and a non-existent symbol cost a 60 s IBKR timeout. They are now skipped with the note "bid-ask: no listed options tracked (Tickers.IV = NO)", keep no previous spread, and get `BOT_VehicleHint = stock_only`.

### Changed
- **`shared/live_sources.R`: new `resolve_atm_spread()`**, used by BOT_monthly. It prices only the strike nearest spot (call and put, live quotes, force-refreshed). `resolve_option_spread()`, still used by /analyze, also prices the 30-delta wings, whose illiquid strikes held each snapshot open ~15 s. Measured: ~27 s per name instead of ~35 s. The remaining time is the `reqTickers()` snapshot wait; TODO 104 (RApplication) covers streaming quotes and a stable `AtmBidAskPct`.

## [2026-09-29] - Sector layer runs on correlation groups (TODO 71)

### Changed
- **Swing scanner sector layer and /analyze Phase B read `ScannerUniverse.Cluster` instead of `Sector`.** The hand-set sectors were a weak proxy for co-movement. Over 250 sessions a stock correlated 0.345 on average with its sector peers and 0.096 with the rest of the universe; the sector gate reads one ETF per sector, which assumes the members track it. `scripts/cluster_universe.py` (RApplication) groups the scanner stocks by return correlation: at most 10 names per group, each group anchored on an ETF or, when none tracks it, on its most central member. Groups raise the peer-minus-rest correlation from 0.249 to 0.471 over 56 groups. Technology splits into semiconductors, software, cloud and a speculative/crypto group; NVDA, AVGO and SMCI join uranium and nuclear power.
- `shared/universe.R`: new `get_groups()`, `get_group_anchors()`, `get_group_stocks()`, `get_group_sectors()`, `get_symbol_group()`, `get_unclassified()`. The `get_sector*()` functions are unchanged; macro_context still uses them.
- `swing_scanner/main.R`: gate, RS rank and `sector_pts` run per group. Names with no group (`Unclassified`) are scanned without a sector gate, and the run prints them as a warning.
- `swing_scanner/sector_gate.R`: `evaluate_sector_gates()` takes an optional `macro_keys` (group -> rule key). Each group uses its family's rules: the majority `Sector` of its members, mapped by the new `SECTOR_RULE_KEY`.
- `shared/live_sources.R`: "Sector" in Phase B is the ticker's group and its "ETF" the group anchor. The rank walks all group anchors from one Yahoo call (`.ret20_batch()`) instead of one fetch per ETF.
- `analyze/report.R`: tooltips describe the group and its anchor.

### Fixed
- **Macro tailwind/headwind rules and macro_context mismatches were inactive for 6 of 10 families.** `SECTOR_MACRO_RULES` and the mismatch table are keyed `PreciousMetals`, `Financials`, `Industrials`, `Materials`, `Agriculture`, `ConsumerStaples`; the scanner looked them up with the `Sector` labels `Precious Metals`, `Financial`, `Industrial`, `Basic Materials`, `Agricultural`, `Consumer non cyclical`, which never matched. `SECTOR_RULE_KEY` maps one to the other, so these families now receive their rules and mismatch boosts. Real Estate, Utilities, Consumer cyclical, China stocks and Communications have no rules, as before.

### Notes
- `sector_pts` still gives 3 points to ranks 1-3 and 2 points to ranks 4-6 among LONG-passing groups. With 56 groups instead of 16 sectors, those ranks are a smaller share of the universe.

## [2026-09-01] - BOT criteria reported as three clusters, not one flat count

### Changed
- **Phase B stage line and scanner flags**: `setup 5/6 - breakout 4/4` reads as ten independent confirmations. Measured across 205 scanner-universe names, the nine criteria carry **~4.9 effective dimensions** (participation ratio; only 3 eigenvalues > 1) in three clusters:
  - **trend / position** - S1, S2, S4, BK1, BK2, BK3 move together as one factor (lambda 3.27). `S1<->S2` phi **0.80**; underneath, `ma50_disp<->ma50_slope` **0.95**, `ma50_disp<->rsi14` **0.92**, `rsi14<->rng_pct` **0.92**.
  - **compression** - S5, orthogonal to everything else (-0.04..+0.10).
  - **supply / volume regime** - S6 and BK4, also orthogonal, and mildly *opposed* to each other (-0.12; they share `vol_ma20` on opposite sides of a fraction).

  So the score is nearer three statements than ten, weighted 6:1:2, and its loudest component is its least specific. Example: AAPL displayed "setup 5/6 - breakout 3/4" while **six of those eight points came from the single trend factor**.
- `shared/indicators.R` `compute_breakdown()` now also sets `trend_count` (of 6), `compression_count` (of 1) and `supply_count` (of 2) as attributes. **Additive** - `setup_count` / `breakout_count` are unchanged.
- `analyze/phases.R` carries the three counts into the Phase B result; `analyze/report.R` renders them via a new `.cluster_note()` ("trend 6/6 - compression 1/1 - supply 1/2") with the raw totals kept in a muted sub-span, falling back to the old string when the attributes are absent.
- `swing_scanner/flow_score.R` flags now lead with `T:6/6 C:1/1 V:1/2 RS:+` ahead of the unchanged `S:x/6 BK:x/4 | S1:+ ...`. Nothing parses this string (verified), so the extension is safe.

**Presentational only** - same gates, same thresholds, same scoring, same `Flow_Score`. No behaviour change; this stops the display overstating how much independent confirmation it has. Rationale and the measurements behind it: `docs/TODO.md` #82 (item I-1).

## [2026-08-26] - analyze: vol-of-vol reported as a percentile, not a level

### Changed
- **Volatility character section** (report.R `.render_vol_character`): the *Vol-of-vol (annualized)* row printed *"Very high - extreme vol instability, option prices will swing significantly"* on **every** report. It was not a per-ticker reading: the estimator returns ~1.94 even for constant volatility, so the "> 2.0" cutoff sat on its own noise floor and no ticker could ever score below it. Measured across the universe, KO scored *above* MT and SPY at 8.1% realized vol read as "extreme vol instability".
  - The row is now **Vol-of-vol (percentile)** against a stored reference basket, with the raw figure kept beneath it as provenance and labelled "uncalibrated - not comparable across tickers". The 30d row is labelled noise-dominated.
  - Requires Tdata >= 5.14.2 (`vov_percentile` field) and a populated `VolOfVolBreakpoints` table; without the table the row degrades to "n/a / percentile unavailable" rather than failing.
  - Sample of the new spread: TLT 1st pct, KO 28th, MT 37th, SPY 53rd, UNH 97th.

## [2026-06-09] - analyze: volatility-character section + option-fetch leaning

### Added
- **"Volatility character" section** (phases.R `.compute_vol_character`, report.R `.render_vol_character`, after Phase C): spot/vol correlation + vol-of-vol (cheap Yahoo history, always computed) plus an opt-in VIX put/call skew decomposition. Calls the Tdata-promoted helpers (`compute_spot_vol_correlation`, `compute_vol_of_vol`, `get_vix_skew`/`format_vix_results`; requires Tdata ≥ 5.10.21).
- **`--skew` flag** (main.R): the VIX put/call decomposition is an ~80-strike IBKR chain pull, so it is opt-in; default runs skip it and the row reports "not computed (run with --skew)".

### Changed — option-fetch leaning (large drop in live IBKR option requests per run)
- **shared/live_sources.R**
  - `resolve_rv30`: read `rvp` straight from the 252d historical-vol bars (`get_volatility_metrics`, hist-only) instead of `Tdata::getVolMetrics` — drops the 8 option-chain fetches (iv15/30/90/180 term structure) that were pure waste for a realized-vol percentile.
  - `resolve_option_spread` (Phase A) and `.live_25d_skew` (funnel): locate ATM / 30Δ / 25Δ strikes analytically (rough IV via new `.rough_iv30`, then BS), qualify a tight σ-sized band, and price only the few strikes needed — was ±35%/±25% priced wholesale (≈474 quotes → ≈6).
  - `.live_atm_iv`: ATM-search band ±10% → ±4%.
  - `resolve_chain_oi`: OI-wall band ±25% → ±12% (a far-OTM wall is not a relevant cap for a swing-horizon target).
- **analyze/structures.R**
  - Proposed spreads capped to the **top-10 by EV** (was every within-cap row, ~76).
  - Spread-enumeration band is now **width/price-aware** (`eff_moneyness = max(base, width/spot + room)`): stays tight on high-priced names (QQQ \$717: \$10 width ≈ 1.4%) and widens enough on cheap ones so spreads can form (MT \$67, F \$15 yielded ZERO width-10 spreads at a fixed 5%).
- **analyze/defaults.R**, **config.yml**: `moneyness_pct` 0.20 → 0.05 (base; structures.R scales it up per width as needed).

### Notes
- Validated across QQQ / MT / J / NVDA / F (\$15–\$717, \$1 and \$5 grids, long + short): per-leg quote fetches down from hundreds to 1–6 strikes; structures still produced on thin/cheap names; put path symmetric. Residual cost is inside `tdata_py.spread` (per-width force-refresh band pricing) — deferred Tdata-side optimization.

## [2026-06-08] - analyze: earnings-inside-the-hold check vs proposed expiries

The vol funnel labels earnings against a **fixed 14-day** macro lookahead and runs in Phase C, *before* the structure expiries are picked — so `/analyze C long` showed earnings "outside event window" (36 DTE) while the chosen **Jul 17** monthly vehicle (39 DTE) actually carries the **2026-07-14** print 3 days before expiry. A breakout spread held through earnings is a binary event trade, not a directional hold. (The prefer-monthly fix amplifies this: the liquid chain is the post-earnings monthly, while the only pre-earnings expiries are dead weeklies.)

### Added
- **analyze/structures.R — `.earnings_vs_expiries()`**: structure-relative earnings check. For each PROPOSED expiry (the ~30d and ~55d picks), flags earnings strictly between today and that expiry (`0 < earnings_dte <= expiry_dte`), reporting per-expiry DTE and days-pre-print, an `any_inside` boolean, and a ready-to-render message. `run_phase_d()` calls it (off `phase_c$funnel$earnings_dte`) and returns `earnings_expiry`.
- **analyze/report.R**: amber `.warn-box` banner under the *Recommended vehicle* header when a proposed expiry carries the print (e.g. "⚠ Earnings 2026-07-14 (36d) falls INSIDE the hold for 2026-07-17 (39d exp, 3d pre-print)…"); Data Summary *Earnings* row gets a "⚠ inside proposed expiry" tag.

### Verified
- Unit cases: C (earnings 36d, expiry Jul 17 / 39d) → inside, "3d pre-print"; earnings after expiry → clean; two expiries with earnings inside only the long leg → flags the long only; no earnings date → clean. Both changed files parse.

## [2026-06-08] - analyze: Phase A bid/ask liquidity probe + monthly-chain preference

`/analyze C long` recommended a **stock** vehicle off an ATM bid/ask of **25%** (30Δ call 45%) — but those quotes came from the near-dead **Jul 24 weekly** (OI ~1). The standard **Jul 17 monthly** was 6–7% wide with OI ~5k: a perfectly tradeable debit-vertical chain. Two defects: the new liquidity probe was reading whatever expiry sat closest to 45 DTE (a weekly), and that wide reading then mis-tripped the dormant `atm_bid_ask% > 8 → stock` vehicle rule.

### Added
- **shared/live_sources.R — `resolve_option_spread()`** (+ helpers `.pick_atm_row()`, `.pick_delta_row()`, `.norm_spread()`, `.spread_grab()`): live IBKR probe of one expiry's ATM and 30Δ call/put bid/ask, returning the normalized `(ask−bid)/mid` per leg plus an `atm_bid_ask_pct`. Surfaces appalling OTM spreads (e.g. REMX July 30Δ call near 100%) and feeds the `atm_bid_ask% > 8 → stock` vehicle rule.
- **analyze/phases.R — `run_phase_a()`**: now runs the spread probe (informational), threads `spread`/`spread_status`/`atm_bid_ask_pct` through every return path (live / DB / scanner-CSV / unavailable). New `.pluck_price()` + reworked `.live_price()` prefer the live IBKR price (`getStockPrice(close=FALSE)`) over Yahoo's adjusted, day-stale close.
- **analyze/main.R + report.R**: spread line in the run log; new **"Option liquidity (A)"** row in the Data Coverage provenance block.

### Fixed
- **shared/live_sources.R — `.pick_expiry_for_dte()`**: new `prefer_monthly = TRUE`. Detects the standard monthly (3rd Friday: `wday==5 & mday 15–21`), picks the monthly nearest `target_dte`, and falls back to nearest-any only when no monthly exists (indices / weekly-only names). One chokepoint, so it corrects the liquidity probe, the vehicle decision, the structure legs (`resolve_expiry` 30d/55d), and `.live_atm_iv` together.

### Verified
- Selector unit cases on C's Jul'26 expiries (Jul 2/10/17/24, Aug 21): targets 30/45/55 all resolve to **Jul 17** (was Jul 24 weekly at target 45); weekly-only input falls back to the weekly without error; `prefer_monthly=FALSE` reproduces the old nearest-DTE pick. All three changed R files parse.
- **Known follow-up:** when the 30d and 55d structure legs both snap to the same monthly (monthlies at e.g. 39 & 74 DTE), the two-expiry grid collapses to one — partially defeating the "enumerate ~30 and ~55 DTE" intent. Candidate fix: long leg takes the next *distinct* monthly. Deferred.

### Housekeeping
- **.gitignore**: ignore `quotes/` (tdata_py per-ticker quote cache written at run time).

## [2026-06-05] - analyze: move-maturity base sized to the breakout horizon

Follow-up to the move-maturity overlay (same day). The first cut anchored the base on the **120-day** (close) swing low — wrong for a 2-4 week breakout horizon (BOT plan: hold 8-14d options / 20-40d stock; base forms over the 20d/40d squeeze). On AAPL that anchored on the stale early-April \$246 low, so every name read "fully extended." The base is now the swing pivot of the **current leg** within a breakout-sized window.

### Changed
- **shared/setup_chain_rr.R — `.recent_swing_anchor()`** (new): ZigZag swing detection — a pivot is confirmed when price reverses by >= `th` (default 4%) from a running extreme. For a long it returns the swing low that launched the current up-leg (running min if mid-pullback, else the last confirmed swing low); mirror for shorts. In a stair-step trend this picks the most recent higher-low, not the stale window low; falls back to the windowed extreme when no >= th reversal exists inside the cap (one uninterrupted run — e.g. AAPL's current grind). `.move_extension()` default window 120 -> 40 and now calls the anchor instead of `min()/max()`.
- **Lookback is tunable**: `analyze.move_lookback_days` (default 40) in config.yml + defaults.R, threaded `compute_structural_target() -> .structural_target_long/short() -> .finalize_targets() -> .move_extension()`, and `run_phase_d() -> .live_targets()`. `compute_structural_target()` gains a defaulted `move_lookback = 40` arg (backward-compatible for the swing_scanner caller).
- **analyze/report.R**: "Move position" note now shows the active window — "base = swing low 258.59 (40d lookback)"; tooltip rewritten (most-recent-swing-pivot within the configurable breakout-horizon window, not the multi-month low).

### Verified
- ZigZag on live AAPL closes: one-leg grind (no >= 3% pullback in-window) so anchor = windowed min at every threshold — 20d \$287 / 40d \$258.59 / 60d \$246; 40d base -> ~89.8% of leg, still extended (correct: +20% in 40d, no pullback). All changed files parse; config.yml loads move_lookback_days = 40.

## [2026-06-05] - analyze: move-maturity overlay in the Fib/structural block

A `/analyze AAPL long` run did not surface that AAPL had run +23% off its early-April low and was sitting ~95% of the way to its structural wall — an extended move. The existing readings each under-signalled it: MA50 displacement was only +11% (the 50-day average had chased the rally), `ret20` showed +8.3% (most of the move sits behind the 20d window), and `fib_confirms` is a bare boolean. The swing base that answers "how extended" was already computed inside `.fib_overlay()` and then discarded. Rather than add parallel metrics (or disturb the flow-linked Phase B `stage` label / `headroom_band`), the move-maturity read is folded into the existing Fib/structural block.

### Added
- **shared/setup_chain_rr.R — `.move_extension()`**: expresses how far the move has travelled from its swing base (close-based, 120d; swing low for longs, swing high for shorts) toward the nearest structural target (`spot_target_low`) as (a) **% of the base→target leg** and (b) the **nearest Fibonacci rung**, plus the **next true extension rung (>1.0) priced out as a forward level**. Direction-symmetric; a ratio >1.0 renders as `N.NNN ext` (move has pushed through its wall). `.finalize_targets()` now calls it and returns `move_base` / `move_pct` / `move_fib` / `move_next_ext` (NA-filled on the no-candidates path).
- **analyze/structures.R — `.live_targets()`**: threads the four `move_*` fields through.
- **analyze/report.R**: new **"Move position"** row in *Structural target sources* (value = "95.1% of base→target leg · ~1.000 rung · next ext 1.272 = 330.59", note = swing base). Tooltip `move_position` added. Descriptive only — no scoring/flow side-effects, consistent with /analyze's data-only mandate.

### Verified
- Unit cases: AAPL-like long (base 253, spot 311, target 314) → 95.1% / ~1.000 rung / next ext 1.272 = 330.59; short (base 200, spot 165, target 150) → 70.0% / 0.618 rung / next ext 1.272 = 136.40; ratio >1.0 → `1.272 ext`. All three changed R files parse.

## [2026-06-03] - analyze: structural-target band — coincident-level + short-label fixes

REMX surfaced `spot_target_low == spot_target_high == 111.55` (a zero-width band) with `targets_agreeing = 2`. Root cause: the nearest unbroken swing high *is* the 52-week high (same bar), so two of the three structural sources return the identical value, and `.finalize_targets()` picked that zero-gap duplicate pair as the band. Also clarified the long/high labels, which were misleading for shorts.

### Fixed
- **shared/setup_chain_rr.R — `.finalize_targets()`**: clusters near-coincident candidate levels (±2%, matching the agreement definition) into distinct structural levels with support counts. A level corroborated by ≥2 sources is now a **single target** (`spot_target_low` set, `spot_target_high = NA`) with `targets_agreeing` = its support count, instead of a zero-width band. Genuinely distinct levels still form a low/high band exactly as before (verified: within-5% → agreeing 2; >5% → agreeing 1), so the swing_scanner `targets_agreeing >= 2` gate (classify.R) is preserved. Extracted the Fib overlay into `.fib_overlay()` which tolerates `high = NA`.

### Changed
- **analyze/report.R**: relabelled the structural-target rows **"Target — near" / "Target — far"** (was spot_target_low/high). The labels denote distance from spot, not price order — for a *short*, near = the higher price (first downside level), far = the lower price. `spot_target_low` still drives `effective_target` / R:R (internal logic unchanged). `Target — far` renders "—" with a note when it's a single corroborated target; structures-table header → "Target (near/far)". Tooltips updated to spell out the long/short price direction.

### Verified
- Unit cases: REMX long → near 111.55 / far — / agreeing 2; REMX short → near 90.54 / far — / agreeing 2; distinct-within-5% → 106/110 band agreeing 2; all-distinct-&gt;5% → 100/110 band agreeing 1. All R files parse.

## [2026-06-03] - analyze: Phase A option bid/ask-spread liquidity probe

Phase A reported only expiry *counts* — it said nothing about whether those options are tradeable. A name can have a dense expiry calendar yet quote unusable spreads (REMX July 30Δ call: bid 0.20 / ask 0.60 → ~100% of mid; even the 98 ATM call quotes 6.40 / 8.40 → 27%). Phase A now probes live bid/ask and reports the normalized spread = (ask − bid) / mid.

### Added
- **shared/live_sources.R — `resolve_option_spread()`** + helpers `.pick_atm_row()`, `.pick_delta_row()`, `.norm_spread()`, `.spread_grab()`. Picks the expiry nearest 45 DTE in the tradeable window, pulls ATM + ~30Δ call/put quotes via `getOptValue` (which already returns `bid`/`ask`/`spread`), and computes the normalized spread for each leg. Prefers the Python-side `spread` column (= 2·(ask−bid)/(ask+bid) ≡ (ask−bid)/mid); falls back to a local bid/ask recompute. Uniform `.ok/.miss/.nodata` provenance shape. **`force_refresh=TRUE`** (like `.live_atm_iv` / `.live_25d_skew`): the parquet quote cache can hold rows fetched by paths that left bid/ask NaN (e.g. chain-OI scans), which yielded a spurious "no bid/ask spread" NO DATA on the first live run — a spread probe must pull the live quote.
- **analyze/phases.R — `run_phase_a()`** now runs the spread probe (new `config` arg for TWS reachability) and threads `spread` / `spread_status` / `atm_bid_ask_pct` through every return path. New Phase E coverage row "Option liquidity (A)".
- **analyze/report.R — `.render_phase_a_spread()`**: per-leg sub-table (ATM call/put, 30Δ call/put) showing strike, Δ, bid, ask, and spread %. Neutral — bare numbers, no verdict.

### Changed
- **analyze/phases.R — `.live_price()`** now prefers the live IBKR quote (`getStockPrice(close=FALSE)` → `tdata_py$getValue` when TWS is up; DB last price otherwise) and falls back to Yahoo. Previously it used `getLastSymPrice` alone — Yahoo's *adjusted daily close*, a day stale and dividend-adjusted, so the report spot drifted from the live quote (REMX 2026-06-03: 102 vs IBKR 97.81). Fixes the displayed spot AND the ATM-strike selection in the spread probe for the whole /analyze report (Phase B header, Phase D targets, Phase A spread). Factored the column-pluck into `.pluck_price()`.
- **analyze/structures.R — `run_phase_d()`** takes `phase_a` and feeds its live `atm_bid_ask_pct` into `pick_vehicle_expiry()`, finally activating the dormant `atm_bid_ask% > 8 → stock` rule (previously always NA / never wired).
- **analyze/main.R**: passes `config` to Phase A and `phase_a` to Phase D; terminal log gains a per-leg spread line.

### Verified
- All five edited files parse. Offline helper test reproduces the math; **live `resolve_option_spread("REMX", …)` against TWS returns LIVE**: ATM 98C 25.9% (6.40/8.30), ATM 98P 36.1%, 30Δ call (114) 87.0%, 30Δ put (95) 48.4%, ATM bid/ask% 31 → trips the >8% → stock vehicle rule. First live run surfaced the cache/`force_refresh` bug above (NO DATA), now fixed.

## [2026-06-02] - scanner: Flow Phase-B scoring fixes + funnel diagnostics

Investigated a 0-candidate run (2026-06-02). Empirical funnel: 202 names → 71 dropped at A (no weekly), 130 at B, 1 at C (AAPL, rich IV). Root causes were two Phase-B issues, now fixed.

### Fixed
- **flow_score.R — dead `extended ≥ 8` escape clause**. Extended `stage_pts` was 1, so the max an extended name could reach was `1 + 3(sector) + 3(footprint) = 7 < 8` — the clause could *never* fire, categorically excluding every extended leader. Bumped extended `stage_pts` 1 → 2 so the ceiling is exactly 8; an extended name now escapes only with a top-3 sector AND full 3/3 footprint (the rare "leader still being accumulated"). Simulated on the 2026-06-02 run: Phase-B passers 1 → 8 (the 7 new are extended Tech leaders at flow 8).
- **flow_score.R — sector context was a hard cap, not a bonus**. Closed-gate (non-trending sector) scored `−2`, capping any non-top-sector stock at `stage(≤4) − 2 + footprint(≤3) ≤ 5` — below the 6 cutoff regardless of the name's own strength, so a genuine Stage-2 continuation in the #4+ sector could never surface. Changed closed-gate `−2 → 0` (neutral): top-sector membership is now upside, not a gate; strong continuation/early names pass on their own merit (`3 + 0 + 3 = 6`).

### Added
- **main.R — funnel breakdown log** after Phase E: Universe → Phase A pass → in-trending-sector → stage(early/continuation, with full stage histogram) → flow_score≥6 → Phase B pass → Phase C pass → dropped-at A/B/C counts. Makes every run self-diagnosing.

### Changed
- `SCANNER_SCHEMA_VERSION` 6 → 7 (marks the scoring change; persisted column schema unchanged).
- Note: Phase-B passers still face Phase C (cheap IV) and D (R:R); surfacing extended leaders routes them to the real economic filter rather than pre-killing them at B.

## [2026-06-02] - scanner: rename Pull_Score → Flow_Score (clarity)

### Changed
- **swing_scanner/pull_score.R → flow_score.R** (file renamed). `score_pull()` → `score_flow()`; returned `pull_score`/`pull_direction` → `flow_score`/`flow_direction`.
  - Rationale: the name "Pull" wrongly implied a raw technical-breakout score. Flow_Score is a higher-level flow/context composite (B.1 stage bucket + B.2 sector-rotation + B.3 footprint) that *consumes* the BOT setup/breakout counts only via the stage bucket. The raw technical synthesis stays in `score_breakout()` (`bot_setup`/`bot_breakout`). Added a header note documenting the distinction.
- **swing_scanner/main.R**: source path, `score_flow()` call, result list key `r$pull`→`r$flow`, df columns `pull_pass`/`pull_score`/`pull_direction` → `flow_pass`/`flow_score`/`flow_direction`, `keep_cols`, funnel label "Pull"→"Flow", Phase-B messages/comments. `SCANNER_SCHEMA_VERSION` 5 → 6.
- **swing_scanner/cheap_score.R**: `pull_direction` param → `flow_direction`.
- **swing_scanner/classify.R**: `df$pull_pass` → `df$flow_pass`.
- **swing_scanner/render_html.R**: tier1/tier2 column names + funnel/subtitle/filter text ("Pull"→"Flow").

### Migration
- `mydb.db`: `ALTER TABLE scanner_results RENAME COLUMN pull_score TO flow_score` + `pull_direction TO flow_direction` (SQLite 3.51.2). `data/mydb.sql` dump updated to match. The next scanner run writes `schema_version=6`.
- **VM not yet synced** — push `mydb.db` schema (or run the same ALTER on the VM DB) before the VM scanner runs, else its `append` will mismatch.

### Verified
- `score_flow()` returns `flow_score`/`flow_direction`; `score_cheap()` arg renamed; all scanner R files parse; live DB columns renamed (pull_score gone).

## [2026-05-12] - analyze: drop redundant columns, make structures collapsible

### Changed
- **reports/analyze/report.R**:
  - Outright table: removed `Max loss` column (always equals Entry premium for an outright long option).
  - Spreads table: removed `Max risk` column (always equals Debit for a DEBIT spread).
  - Both outright and spreads tables now wrapped in `<details open><summary>...</summary>...</details>` — default open, click summary to collapse. Matches Phase B per-indicator-breakdown UX.

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
