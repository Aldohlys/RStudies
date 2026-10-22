# ATR, daily-range distribution and overnight-gap volatility — Research Findings

**Window**: AAPL, 501 sessions, 2024-10-22 to 2026-10-21 (daily OHLC, Yahoo via `Tdata::getYahooData`).
**Script**: `atr-gap-volatility/atr_gap_study.R` (`Rscript atr-gap-volatility/atr_gap_study.R [TICKER] [YEARS]`).
**Outputs**: `output/atr_gap/AAPL/` — `summary.txt`, `exceedance.csv`, `tr_vs_atr.png`, `gap_vs_session.png`.

**Questions**:
1. How often does a day's true range exceed ATR(14), and how are true range, high − low and |close-to-close| distributed in ATR multiples?
2. How much of daily variance comes from the overnight gap (prev close → open) versus the regular session (open → close)?

Convention: each day is measured against the Wilder ATR(14) known at the **previous** close (no look-ahead). ATR on 2026-10-21: 6.47 (1.88 % of 343.99).

## 1. Daily moves in ATR multiples

| Share of days with move > k × ATR | median | > 0.5× | > 0.75× | > 1× | > 1.25× | > 1.5× | > 2× | > 2.5× | > 3× |
|---|---|---|---|---|---|---|---|---|---|
| True range | 0.90× | 92.2 % | 69.3 % | **39.1 %** | 20.2 % | 13.0 % | 6.0 % | 2.4 % | 0.8 % |
| High − low | 0.87× | 91.2 % | 63.7 % | **33.3 %** | 15.2 % | 9.4 % | 3.4 % | 1.4 % | 0.4 % |
| \|Close-to-close\| | 0.36× | 38.3 % | 22.6 % | **12.8 %** | 8.2 % | 5.4 % | 2.6 % | 0.8 % | 0.4 % |

- TR > ATR on ~39 % of days, ~5.5 days per 14-session window (range 1–11; every window had at least one). The median day is 0.90× ATR: the mean is pulled up by a right tail.
- Mild clustering: P(TR > ATR | previous day > ATR) = 41.8 % vs 37.5 % otherwise; dispersion of the 14-day count is above binomial (sd 2.08 vs 1.83).
- The close typically lands ~0.36 ATR from the previous close. ATR-based stops/targets evaluated on closes should use the close-to-close row, intraday touches the high − low row.
- High − low and TR coincide on 73 % of days; they diverge in the tail (> 2×: 3.4 % vs 6.0 %). On average TR = 92 % high − low + 8 % gap extension; on top-decile TR days gap extension is 17 % of TR.

## 2. Overnight gap vs regular session

| Estimator (annualized) | Vol |
|---|---|
| Close-to-close | 28.6 % |
| Overnight gap (log prev close → open) | 18.0 % |
| Session (log open → close) | 24.4 % |
| Rogers-Satchell (session, OHLC) | 23.4 % |
| Parkinson (high-low) | 23.8 % |
| Yang-Zhang (combined) | 29.7 % |

- **Overnight share of daily variance: 35 %** (overnight / (overnight + session)); 37 % of Yang-Zhang. Gap and session returns are mildly negatively correlated (ρ = −0.12), removing ~13 % of variance from close-to-close.
- Per hour, the session carries ~5× the variance of the overnight period (6.5 h vs 17.5 h).
- **Gap variance is jump risk, concentrated in few events**: the 10 largest gaps (2 % of days) are 56 % of all overnight variance, top 20 → 68 %. Excess kurtosis: overnight 19.9, session 17.0, close-to-close 10.5. Ex top-10 gaps, overnight vol is 12.1 % and the gap share drops to ~20 %.
- Largest gaps: 2026-07-31 −9.0 %, 2025-04-03 −8.6 %, 2025-04-14 +6.5 %, 2025-04-07 −6.1 %, 2025-05-12 +6.1 % (tariff episode plus post-earnings sessions). Largest single move in the sample was a session move: 2025-04-09 +14.5 % (tariff pause).
- Gaps are slightly more often faded than extended: the session continues the gap direction on 42.7 % of days, 40.0 % for the largest 20 % of gaps.
- Rolling 63-session overnight share ranged 13 %–64 %; latest 56 % (overnight 21.4 %, session 18.8 %, close-to-close 25.4 %), driven by the 2026-07-31 gap still inside the window. Expected to revert toward 20–35 % once it rolls out, absent a new large gap (next earnings).

## Caveats

- Two-year sample includes the April 2025 tariff crash, which inflates the > 2× tail and the gap concentration.
- Unadjusted Yahoo OHLC (no splits in the window; ex-dividend days shift the overnight return by ~0.1 %, negligible). Closes match IBKR daily bars for the sessions checked; IBKR gives 499 vs 501 sessions over the same span, so figures differ from an IBKR-based run in the first decimal.
- Historical frequencies, not forecasts.
