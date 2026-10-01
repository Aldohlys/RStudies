"""bot_stop_floor_trial.py - stop floor 1.0 ATR vs 2.5 ATR when no usable level
exists, under BOT's exit rule (exit if the target is not touched by
sess_target_p75).

Per start day (every 5th session, 8 years, BOT-eligible names), long:
    target = px + C_close90 * ATR * sqrt(10)        (em10_cap, close-based)
    stop   = px - k * ATR,  k in {1.0, 2.5}
    exit   = target touched -> +reward; stop touched -> -1 R (both on one
             session -> stop); else at the close of session T, T = the
             winning-interval exit, round((d / C_touch75)^2) with d the target
             distance in ATR.
Reported per stop: asym, outcome shares, EV in R, and EV in money for a fixed
risk budget (same CHF risked per trade, so EV in R is EV per CHF risked).
"""
import sqlite3, time, datetime as dt
import numpy as np, pandas as pd, yfinance as yf

DB = r"C:/Users/aldoh/Documents/RApplication/data/mydb.db"
c = sqlite3.connect(f"file:{DB}?mode=ro", uri=True)
syms = sorted(pd.read_sql("SELECT COALESCE(NULLIF(YahooName,''), Name) AS yh FROM Tickers "
                          "WHERE BOT_Eligible = 1", c).yh.unique())

def fetch(syms, tries=4):
    out, miss = {}, list(syms)
    for k in range(tries):
        if k: time.sleep(20)
        d = yf.download(miss, period="8y", auto_adjust=True, progress=False, threads=True, group_by="ticker")
        for s in list(miss):
            try:
                x = d[s][["High", "Low", "Close"]].dropna()
            except KeyError:
                continue
            if len(x) > 750:
                out[s] = x[x.index.date < dt.date.today()]; miss.remove(s)
        if not miss: break
    return out, miss

data, miss = fetch(syms)
print(f"{len(data)} names with prices, {len(miss)} missing")

def atr14(x):
    pc = x.Close.shift(1)
    tr = pd.concat([x.High - x.Low, (x.High - pc).abs(), (x.Low - pc).abs()], axis=1).max(axis=1)
    return tr.ewm(alpha=1 / 14, adjust=False).mean().values

N = 10
recs = []
for s, x in data.items():
    hi, lo, cl = x.High.values, x.Low.values, x.Close.values
    a = atr14(x); n = len(cl); sc = a * np.sqrt(N)
    mv = np.full(n, np.nan); mv[:-N] = cl[N:] - cl[:-N]
    mx = pd.Series(hi[::-1]).rolling(N).max().values[::-1]
    up = np.full(n, np.nan); up[:-1] = mx[1:] - cl[:-1]
    ok = (np.arange(n) >= 20) & np.isfinite(sc) & (sc > 0)
    c_close = np.nanquantile((mv / sc)[ok], 0.90)
    c75 = np.nanquantile((up / sc)[ok], 0.75)
    d_atr = c_close * np.sqrt(N)                      # target distance in ATR at the cap
    T = max(1, int(round((d_atr / c75) ** 2)))         # winning-interval exit (sess_target_p75)
    for t in range(60, n - T - 1, 5):
        px, at = cl[t], a[t]
        if not (np.isfinite(at) and at > 0): continue
        tgt = px + d_atr * at
        h, l = hi[t + 1:t + 1 + T], lo[t + 1:t + 1 + T]
        ft = np.nonzero(h >= tgt)[0]; ft = ft[0] if len(ft) else 10**9
        for k in (1.0, 2.5):
            stp = px - k * at
            fs = np.nonzero(l <= stp)[0]; fs = fs[0] if len(fs) else 10**9
            reward = (tgt - px) / (px - stp)
            if ft < fs:      out, r, day = "target", reward, ft + 1
            elif fs < 10**9: out, r, day = "stop", -1.0, fs + 1
            else:            out, r, day = "time", (cl[t + T] - px) / (px - stp), T
            recs.append((s, k, T, out, r, reward, day))

df = pd.DataFrame(recs, columns=["sym", "k", "T", "out", "R", "asym", "day"])
print(f"winning-interval exit T: median {df['T'].median():.0f} sessions (range {df['T'].min()}-{df['T'].max()})")

def summ(g):
    return pd.Series({"asym": g.asym.median(),
                      "target %": (g.out == "target").mean() * 100,
                      "stop %": (g.out == "stop").mean() * 100,
                      "time exit %": (g.out == "time").mean() * 100,
                      "EV (R)": g.R.mean(), "EV se": g.R.std() / np.sqrt(len(g)),
                      "avg loss on stop/time-loss (R)": g.R[g.R < 0].mean(),
                      "days held (mean)": g.day.mean()})

print("\n== 271 names, exit at target / stop / sess_target_p75")
print(df.groupby("k").apply(summ).round(3).to_string())
u = df[df.sym == "UBSG.SW"]
if len(u):
    print(f"\n== UBSG only (T = {u['T'].iloc[0]} sessions, {len(u)//2} starts)")
    print(u.groupby("k").apply(summ).round(3).to_string())
d = df.pivot_table(index=["sym", df.groupby(['sym','k']).cumcount()], columns="k", values="R")
diff = (d[2.5] - d[1.0]).dropna()
print(f"\nEV difference 2.5 - 1.0 ATR: {diff.mean():+.3f} R (se {diff.std()/np.sqrt(len(diff)):.3f}, n {len(diff)})")
