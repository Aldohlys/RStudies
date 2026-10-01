"""bot_reach_time_trial.py - measures the "winning interval" proposal for BOT_daily.

Proposal: for a target d ATR above px, the sessions a good (p75) / winning (p90)
trade needs to TOUCH it, sess_q = (d / C_q)^2, where C_q is the q-quantile of
the maximum favourable excursion over N sessions standardised by ATR * sqrt(N):
    MFE_N = max(High[t+1 .. t+N]) - Close[t],   z = MFE_N / (ATR_t * sqrt(N))
Checks:
  1. C_q by horizon N: is it constant (the sqrt-time premise)?
  2. formula vs observed first-passage time: for d in ATR, the sessions until
     the High first reaches Close_t + d * ATR_t; its 10th / 25th percentile is
     what "a winner / a good trade touches it after N sessions" means.
  3. close-based C (today's em10 construction) vs touch-based, for scale.
Long side only. Dividend-adjusted OHLC, Wilder ATR(14).
"""
import sqlite3, time, datetime as dt
import numpy as np, pandas as pd, yfinance as yf

DB = r"C:/Users/aldoh/Documents/RApplication/data/mydb.db"
HORIZONS = [5, 10, 15, 21, 30, 42]
DISTS = [1.0, 1.5, 2.0, 3.0, 4.0, 5.0]
MAXN = 120

c = sqlite3.connect(f"file:{DB}?mode=ro", uri=True)
tk = pd.read_sql("SELECT Name, COALESCE(NULLIF(YahooName,''), Name) AS yh FROM Tickers "
                 "WHERE BOT_Eligible = 1", c)
syms = sorted(tk.yh.unique())
print(f"{len(syms)} BOT-eligible names")

def fetch(syms, tries=4):
    out, miss = {}, list(syms)
    for k in range(tries):
        if k: time.sleep(20)
        d = yf.download(miss, period="8y", auto_adjust=True, progress=False, threads=True, group_by="ticker")
        for s in list(miss):
            try:
                x = d[s][["High", "Low", "Close"]].dropna() if len(miss) > 1 else d[["High", "Low", "Close"]].dropna()
            except KeyError:
                continue
            if len(x) > 750:
                out[s] = x[x.index.date < dt.date.today()]
                miss.remove(s)
        if not miss: break
    return out, miss

data, miss = fetch(syms)
print(f"prices for {len(data)}, missing {len(miss)}")

def atr14(x):
    pc = x.Close.shift(1)
    tr = pd.concat([x.High - x.Low, (x.High - pc).abs(), (x.Low - pc).abs()], axis=1).max(axis=1)
    return tr.ewm(alpha=1 / 14, adjust=False).mean()

zs = {N: [] for N in HORIZONS}          # touch-based standardised MFE
zc = {N: [] for N in HORIZONS}          # close-based standardised move
fpt = {d: [] for d in DISTS}            # first-passage times (sessions), inf if never within MAXN
for s, x in data.items():
    a = atr14(x).values; cl = x.Close.values; hi = x.High.values
    n = len(cl)
    for N in HORIZONS:
        # running max of High over the next N sessions
        mx = pd.Series(hi[::-1]).rolling(N, min_periods=N).max().values[::-1]   # max over [t, t+N-1]
        mfe = np.full(n, np.nan); mfe[:-1] = mx[1:] - cl[:-1]                  # max over [t+1, t+N]
        mv = np.full(n, np.nan); mv[:-N] = cl[N:] - cl[:-N]
        ok = np.arange(n) >= 20
        zs[N].append((mfe / (a * np.sqrt(N)))[ok])
        zc[N].append((mv / (a * np.sqrt(N)))[ok])
    step = 5  # every 5th start day keeps the first-passage scan fast
    for t in range(20, n - MAXN, step):
        rise = (hi[t + 1:t + 1 + MAXN] - cl[t]) / a[t]
        for d in DISTS:
            hit = np.nonzero(rise >= d)[0]
            fpt[d].append(hit[0] + 1 if len(hit) else np.inf)

print("\n1. coefficient by horizon (quantiles of z; touch = max High, close = close at N)")
rows = []
for N in HORIZONS:
    z = np.concatenate(zs[N]); z = z[np.isfinite(z)]
    w = np.concatenate(zc[N]); w = w[np.isfinite(w)]
    rows.append([N, *np.round(np.quantile(z, [.5, .75, .9]), 3), *np.round(np.quantile(w, [.75, .9]), 3)])
print(pd.DataFrame(rows, columns=["N", "touch p50", "touch p75", "touch p90", "close p75", "close p90"]).to_string(index=False))

C75 = np.quantile(np.concatenate(zs[10])[np.isfinite(np.concatenate(zs[10]))], .75)
C90 = np.quantile(np.concatenate(zs[10])[np.isfinite(np.concatenate(zs[10]))], .90)
print(f"\n2. formula (C75={C75:.3f}, C90={C90:.3f} at N=10) vs observed first-passage time")
rows = []
for d in DISTS:
    f = np.array(fpt[d])
    q10, q25, q50 = np.quantile(f, [.10, .25, .50])
    rows.append([d, round((d / C90) ** 2, 1), q10, round((d / C75) ** 2, 1), q25, q50,
                 round(100 * np.mean(f <= 21), 1), round(100 * np.mean(np.isinf(f)), 1)])
print(pd.DataFrame(rows, columns=["d (ATR)", "formula p90", "observed 10th pct", "formula p75",
                                  "observed 25th pct", "observed median", "% touched <= 21 sess",
                                  f"% never in {MAXN}"]).to_string(index=False))
print(f"\nstarts per distance: {len(fpt[DISTS[0]])}")
