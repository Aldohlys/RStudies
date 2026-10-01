"""bot_cap_touch_trial.py - close-based vs touch-based em10_cap: does the higher
cap pay, or does it only raise asym on paper?

em10_cap places the target, when the level found lies further, at
    px + C * ATR * sqrt(10)
with C the name's 90th percentile of the 10-session CLOSE move (today) or of
the 10-session TOUCH (max High) excursion (proposal). Same stop either way.
For every start day (every 5th session, 8 years, BOT-eligible names), long:
    target_close = px + C_close * ATR * sqrt(10)
    target_touch = px + C_touch * ATR * sqrt(10)
    stop         = px - s * ATR,  s in {1.0, 1.88 (median BOT_daily stop), 2.5}
walk H sessions: target first -> + reward (R), stop first -> -1 R (a session
touching both counts as stop: conservative), neither -> mark to market at
the close of session H. C per name, measured on the name's whole history
(in-sample, like BOT_monthly). Two populations: every day, and trend days
(close > EMA50 and EMA50 rising over 5 sessions: S1 + S2, present on most of
the book's real entries).
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
STOPS = [1.0, 1.88, 2.5]
HORIZONS = [10, 21]
recs = []
coefs = []
for s, x in data.items():
    hi, lo, cl = x.High.values, x.Low.values, x.Close.values
    a = atr14(x); n = len(cl); sc = a * np.sqrt(N)
    mv = np.full(n, np.nan); mv[:-N] = cl[N:] - cl[:-N]
    mx = pd.Series(hi[::-1]).rolling(N).max().values[::-1]
    up = np.full(n, np.nan); up[:-1] = mx[1:] - cl[:-1]
    ok = (np.arange(n) >= 20) & np.isfinite(sc) & (sc > 0)
    c_close = np.nanquantile((mv / sc)[ok], 0.90)
    c_touch = np.nanquantile((up / sc)[ok], 0.90)
    coefs.append((c_close, c_touch))
    ema = pd.Series(cl).ewm(span=50, adjust=False).mean().values
    trend = (cl > ema) & (ema > np.roll(ema, 5))
    for t in range(60, n - max(HORIZONS) - 1, 5):
        px, at = cl[t], a[t]
        if not (np.isfinite(at) and at > 0): continue
        for name, C in (("close", c_close), ("touch", c_touch)):
            tgt = px + C * at * np.sqrt(N)
            for sm in STOPS:
                stp = px - sm * at
                for H in HORIZONS:
                    h, l = hi[t + 1:t + 1 + H], lo[t + 1:t + 1 + H]
                    hit_s = np.nonzero(l <= stp)[0]; hit_t = np.nonzero(h >= tgt)[0]
                    fs = hit_s[0] if len(hit_s) else 10**9; ft = hit_t[0] if len(hit_t) else 10**9
                    reward = (tgt - px) / (px - stp)
                    if ft < fs:   out, r, days = "target", reward, ft + 1
                    elif fs < 10**9: out, r, days = "stop", -1.0, fs + 1
                    else:         out, r, days = "open", (cl[t + H] - px) / (px - stp), np.nan
                    recs.append((s, bool(trend[t]), name, sm, H, out, r, reward, days))

df = pd.DataFrame(recs, columns=["sym", "trend", "cap", "stop_atr", "H", "out", "R", "asym", "days"])
cc = np.array(coefs)
print(f"per-name C90: close median {np.median(cc[:,0]):.3f}, touch median {np.median(cc[:,1]):.3f}, "
      f"touch/close median {np.median(cc[:,1]/cc[:,0]):.2f}")
print(f"starts: {df.groupby(['cap','stop_atr','H']).size().iloc[0]} per cell "
      f"({df[df.trend].groupby(['cap','stop_atr','H']).size().iloc[0]} on trend days)\n")

def summ(g):
    return pd.Series({"asym": g.asym.median(), "P(target)": (g.out == "target").mean() * 100,
                      "P(stop)": (g.out == "stop").mean() * 100, "P(open)": (g.out == "open").mean() * 100,
                      "EV (R)": g.R.mean(), "EV se": g.R.std() / np.sqrt(len(g)),
                      "days to target (median)": g.days.median()})

for label, sub in (("ALL DAYS", df), ("TREND DAYS (close > rising EMA50)", df[df.trend])):
    print(f"== {label}")
    t = sub.groupby(["H", "stop_atr", "cap"]).apply(summ).round(3)
    print(t.to_string()); print()
    # paired difference touch - close, per start
    w = sub.pivot_table(index=["sym", "stop_atr", "H"], columns="cap", values="R", aggfunc="mean")
print("EV difference touch - close (R), per cell, all days / trend days:")
for H in HORIZONS:
    for sm in STOPS:
        a_ = df[(df.H == H) & (df.stop_atr == sm)]
        b_ = a_[a_.trend]
        d1 = a_[a_.cap == "touch"].R.values - a_[a_.cap == "close"].R.values
        d2 = b_[b_.cap == "touch"].R.values - b_[b_.cap == "close"].R.values
        print(f"  H={H:2d} stop={sm:.2f} ATR: all {d1.mean():+.3f} (se {d1.std()/np.sqrt(len(d1)):.3f}) | "
              f"trend {d2.mean():+.3f} (se {d2.std()/np.sqrt(len(d2)):.3f})")
