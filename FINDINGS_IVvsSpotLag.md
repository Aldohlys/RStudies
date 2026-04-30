# IV-vs-Spot Lag — Research Findings

**Window**: 2026-04-28 session.  
**Original question**: After strong upside spot momentum, does retail call buying lift IV30 / call skew / 25Δ risk reversal with a 3–7 day lag?

## Verdict

**Original thesis falsified at EOD.** The retail-call-lag effect is not detectable in five years of daily IVolatility surface data across nine retail-traded names. Any effect that exists is too small to overcome bid-ask + commission costs at retail trade size.

**Pivot to FOMC vol-surface dynamics**: pipeline confirmed working (positive control). Effect is real, ~−1.2 to −1.5 vol points post-FOMC over d+1 to d+10, but is a **known and published anomaly** — extending Nikkinen-Sahlström 2004, Johannes-Kaeck-Seeger 2023, Zhang-Kappou-Urquhart 2025. Not novel research and unlikely to be tradable retail-net-of-costs.

## Data acquired (IVolatility Backtest API Plus, 7-day trial)

For 17 tickers — 9 single-names + 8 index/sector ETFs — pulled 2021-01-04 to 2026-04-27:

- `stock_prices.parquet` — daily OHLC, ~1334 rows
- `ivx.parquet` — ATM IV by tenor (7d, 14d, ..., 1080d), call/put/mean
- `ivs_full.parquet` — full delta-and-moneyness surface, ~450k rows per ticker
- `ivs_25delta_30d_wide.parquet` — extracted 30d 25Δ call/put IV, daily

**Stored under**: `ivol_probe_out/{TICKER}/`

Tickers: AAPL, DOW, GOLD, JNJ, NFLX, OXY, TGT, WMT, XOM, SPY, QQQ, IWM, XLK, XLE, XLF, XLV, XLU.

Trial expired ~ early May 2026.

## Studies run

| # | Study | Spec | Best p-value | Verdict |
|---|---|---|---|---|
| 1 | Per-ticker mom10 momentum | 4 tickers (AAPL/DOW/TGT/WMT) | WMT 0.003 | Specification-fragile; killed by mom12 |
| 2 | WMT robustness sweep | 7 knob variants | LB12 → 0.31 | Confirms (1) is artifact |
| 3 | Per-ticker catalyst, +5% gap | 9 tickers | GOLD 0.16 | Underpowered, n too small |
| 4 | Per-ticker catalyst, +3% gap | 9 tickers | OXY 0.36 | None significant |
| 5 | Pooled catalyst, +3% gap up | 9 tickers, 51 events | **0.19** | Hint, not signal |
| 6 | Pooled catalyst, −3% gap down | 9 tickers, 52 events | 0.85 | No effect |
| 7 | FOMC ATM IV crush | 8 ETFs, 42 events | SPY 0.030, IWM 0.033, XLE 0.043 | **Real but already published** |

## Key literature

- **Lucca-Moench 2015 JF**, "Pre-FOMC Announcement Drift" — equity drift; effect attenuated post-2015 (Kurov 2021).
- **Nikkinen-Sahlström 2004 IRFA** — first formal post-FOMC IV crush documentation.
- **Johannes-Kaeck-Seeger** SSRN 4484011 — structural model of FOMC event-risk volatility risk premium.
- **Hu-Pan-Wang-Zhu 2022 JFE** — uncertainty-resolution mechanism; predicts post-event IV decay.
- **Zhang-Kappou-Urquhart** SSRN 5464630 (Sep 2025) — d+1 SPX delta-neutral straddle returns +1.47% (long-vol *expands* on day after FOMC; bleeds down later).
- **Garleanu-Pedersen-Poteshman 2009 RFS** — demand-based option pricing; foundational for dealer-flow mechanism.

## Why the original thesis failed

1. **EOD data is wrong granularity.** Retail flow concentrates 11:00–14:30 ET; dealers re-hedge intraday. Any lag effect lives within the session, not across days.
2. **2021-2026 sample is post-Robinhood-mania.** The retail-flow regime that motivated the thesis was 2020–early 2021, mostly outside our window.
3. **AAPL is the wrong name.** Most efficient single-name option market in the world; if a lag existed it would be priced out fastest. Mid-cap retail darlings (PLTR, SOFI, COIN) might fare differently but were not pulled.

## Trade economics check on the +1.49 vp pooled catalyst hint

For a long-c25 / short-p25 RR25 spread held d+1 to d+9:

| Ticker | Vega/contract | Gross at +1.49vp | Round-trip slippage | **Net per lot** |
|---|---|---|---|---|
| AAPL | $25 | +$37 | $15 | +$22 |
| NFLX | $64 | +$95 | $64 | +$31 |
| WMT, TGT, OXY | $5–12 | +$7–18 | $7–22 | ≈ $0 or negative |
| GOLD | $2 | +$3 | $4 | −$1 |

Per-trade Sharpe ≈ 0.19. Annualized at ~10 events/yr: **0.6**. Below most factor-fund hurdles. **Not tradable.**

## Files saved

- `IVvsSpotLag.R` — multi-ticker mom-momentum study, parameterised
- `robustness_WMT.R` — knob-sweep harness
- `catalyst_study.R` — per-ticker post-earnings catalyst study
- `catalyst_pooled.R` — pooled catalyst, both directions
- `fomc_study.R` — FOMC event study with pre/post window
- `ivolatility_probe.py` — IVolatility data fetcher (CLI: `--symbol XXX`)
- `ivol_probe_out/study_summary.csv` — momentum-study summary
- `ivol_probe_out/wmt_robustness.csv` — WMT robustness table
- `ivol_probe_out/catalyst_summary.csv` — per-ticker catalyst at +3% gap
- `ivol_probe_out/pooled_catalyst_{up,down}.csv` — pooled catalyst paths
- `ivol_probe_out/fomc_summary.csv` — FOMC ATM-IV pre/post stats
- All `*_results.rds` for re-loading in R

## Suggested next directions (if returning to this)

1. **Idiosyncratic-event clustering**: bank-earnings-week XLF dynamics, tech-earnings-week XLK dynamics. Less academically mined.
2. **Cross-asset lead-lag around CPI/NFP**: equity IV vs FX IV vs Treasury IV repricing the same release.
3. **0DTE-driven SPX regime detection**: post-2022 retail/0DTE has changed the surface in ways the literature is still catching up to.

All three need different data than the IVolatility EOD surface.

## Lessons for the pipeline

- Pre-paying for data without a probe script wastes quota — always probe first.
- IVolatility's async-job pattern (`urlForDetails` → `urlForDownload`) is poorly documented; helpers in `ivolatility_probe.py` handle it correctly now.
- The R event-study harness (`build_paths` + bootstrap) is generic; works for any anchor + horizon definition. Reusable.
- Per-ticker n is the binding constraint with EOD data. Pooling across tickers gains the most power; specification searches do not.
