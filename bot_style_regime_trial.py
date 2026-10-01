"""MTUM/VLUE style regime vs BOT outcomes. Read-only study (TODO 91, closed: rejected 2026-10-01).

Regime (date t, using data up to t):
  R_ratio = MTUM / VLUE (adjusted closes)
  ON  if R_ratio > its 50-session SMA            (momentum beating value lately)
  also continuous: 63-session log change of the ratio
Comparator: SPY above its 200-session SMA (market trend) - is the regime more than that?

Synthetic entries: BOT-eligible names, every 5th session, long, 8 years.
  outcome A: signed 12-session return / ATR14          (direction, validated by S-1)
  outcome B: winning-interval result in R: target = px + C_close90*ATR*sqrt(10),
             stop = px - 1 ATR, exit at target / stop / close of session T=(d/C_touch75)^2
Inference: per-date means, then mean and SE across dates (dates are the clusters:
every name on a date shares the same regime value).
Real trades: Trades WHERE Strategy='BOT': entry = first TradeDate, outcome = stored
Return (return on initial risk), regime at entry.
"""
import sqlite3, time, datetime as dt
import numpy as np, pandas as pd, yfinance as yf

DB = r"C:/Users/aldoh/Documents/RApplication/data/mydb.db"
c = sqlite3.connect(f"file:{DB}?mode=ro", uri=True)
syms = sorted(pd.read_sql("SELECT COALESCE(NULLIF(YahooName,''), Name) AS yh FROM Tickers "
                          "WHERE BOT_Eligible = 1", c).yh.unique())

def fetch(tickers, tries=4, period="10y"):
    out, miss = {}, list(tickers)
    for k in range(tries):
        if k: time.sleep(20)
        d = yf.download(miss, period=period, auto_adjust=True, progress=False, threads=True, group_by="ticker")
        for s in list(miss):
            try:
                x = d[s][["High", "Low", "Close"]].dropna()
            except KeyError:
                try:
                    x = d[["High", "Low", "Close"]].dropna()
                    if isinstance(x.columns, pd.MultiIndex): x.columns = x.columns.get_level_values(0)
                except KeyError:
                    continue
            if len(x) > 500:
                out[s] = x[x.index.date < dt.date.today()]; miss.remove(s)
        if not miss: break
    return out, miss

ref, m = fetch(["MTUM", "VLUE", "SPY"])
assert not m, m
mt, vl, spy = ref["MTUM"].Close, ref["VLUE"].Close, ref["SPY"].Close
ratio = (mt / vl).dropna()
reg = pd.DataFrame({
    "on": (ratio > ratio.rolling(50).mean()).astype(float),
    "chg63": np.log(ratio / ratio.shift(63)),
    "spy_up": (spy > spy.rolling(200).mean()).astype(float),
    "spy_up50": (spy > spy.rolling(50).mean()).astype(float),
    "spy_ret20": np.log(spy / spy.shift(20)),
    "on_lag5": (ratio > ratio.rolling(50).mean()).astype(float).shift(5),
}).dropna()
reg.index = pd.to_datetime(reg.index).normalize()
print(f"regime series {reg.index[0].date()} .. {reg.index[-1].date()}, {len(reg)} sessions")
print(f"share ON: {reg.on.mean()*100:.1f}%   SPY-up: {reg.spy_up.mean()*100:.1f}%")
spells = (reg.on != reg.on.shift()).cumsum()
lens = reg.groupby(spells).size()
print(f"ON/OFF spell length: median {lens.median():.0f} sessions, mean {lens.mean():.1f}, n spells {len(lens)}")
print(f"agreement regime ON vs SPY-up (phi): {reg[['on','spy_up']].corr().iloc[0,1]:+.3f}")
print(f"regime today ({reg.index[-1].date()}): {'ON' if reg.on.iloc[-1] else 'OFF'}, 63d ratio change {reg.chg63.iloc[-1]*100:+.1f}%")

data, miss = fetch(syms, period="8y")
print(f"\n{len(data)} names, {len(miss)} missing")

def atr14(x):
    pc = x.Close.shift(1)
    tr = pd.concat([x.High - x.Low, (x.High - pc).abs(), (x.Low - pc).abs()], axis=1).max(axis=1)
    return tr.ewm(alpha=1 / 14, adjust=False).mean().values

N = 10
rows = []
for s, x in data.items():
    hi, lo, cl = x.High.values, x.Low.values, x.Close.values
    idx = pd.to_datetime(x.index).normalize()
    a = atr14(x); n = len(cl); sc = a * np.sqrt(N)
    mv = np.full(n, np.nan); mv[:-N] = cl[N:] - cl[:-N]
    mx = pd.Series(hi[::-1]).rolling(N).max().values[::-1]
    up = np.full(n, np.nan); up[:-1] = mx[1:] - cl[:-1]
    ok = (np.arange(n) >= 20) & np.isfinite(sc) & (sc > 0)
    c90 = np.nanquantile((mv / sc)[ok], 0.90); c75 = np.nanquantile((up / sc)[ok], 0.75)
    d_atr = c90 * np.sqrt(N); T = max(1, int(round((d_atr / c75) ** 2)))
    ema = pd.Series(cl).ewm(span=50, adjust=False).mean().values
    for t in range(60, n - max(T, 12) - 1, 5):
        px, at = cl[t], a[t]
        if not (np.isfinite(at) and at > 0): continue
        rA = (cl[t + 12] - px) / at
        tgt, stp = px + d_atr * at, px - at
        h, l = hi[t + 1:t + 1 + T], lo[t + 1:t + 1 + T]
        ft = np.nonzero(h >= tgt)[0]; fs = np.nonzero(l <= stp)[0]
        ft = ft[0] if len(ft) else 10**9; fs = fs[0] if len(fs) else 10**9
        if ft < fs: rB, hit = d_atr, 1
        elif fs < 10**9: rB, hit = -1.0, 0
        else: rB, hit = (cl[t + T] - px) / at, 0
        rows.append((idx[t], s, rA, rB, hit, cl[t] > ema[t] and ema[t] > ema[t - 5]))
ev = pd.DataFrame(rows, columns=["date", "sym", "rA", "rB", "hit", "trend"])
ev = ev.merge(reg, left_on="date", right_index=True, how="inner")
print(f"events with a regime value: {len(ev)} on {ev.date.nunique()} dates")

def clustered(sub, col):
    g = sub.groupby("date")[col].mean()
    return g.mean(), g.std() / np.sqrt(len(g)), len(g)

def report(sub, label):
    print(f"\n== {label}")
    print(f"{'split':<26}{'n dates':>8}{'A: 12d ret/ATR':>18}{'B: R (win.int.)':>18}{'P(target first)':>17}")
    for name, mask in (("regime ON", sub.on == 1), ("regime OFF", sub.on == 0),
                       ("SPY up", sub.spy_up == 1), ("SPY down", sub.spy_up == 0),
                       ("SPY up & ON", (sub.spy_up == 1) & (sub.on == 1)),
                       ("SPY up & OFF", (sub.spy_up == 1) & (sub.on == 0))):
        q = sub[mask]
        if len(q) < 50: continue
        mA, sA, nd = clustered(q, "rA"); mB, sB, _ = clustered(q, "rB"); mH, _, _ = clustered(q, "hit")
        print(f"{name:<26}{nd:>8}{mA:>+11.3f} ±{sA:.3f}{mB:>+11.3f} ±{sB:.3f}{mH*100:>15.1f}%")
    on, off = sub[sub.on == 1], sub[sub.on == 0]
    for col in ("rA", "rB"):
        a1, s1, _ = clustered(on, col); a0, s0, _ = clustered(off, col)
        print(f"  ON - OFF on {col}: {a1-a0:+.3f} (se {np.hypot(s1, s0):.3f})")
    # within SPY-up only: does the regime add anything?
    u = sub[sub.spy_up == 1]
    a1, s1, _ = clustered(u[u.on == 1], "rB"); a0, s0, _ = clustered(u[u.on == 0], "rB")
    print(f"  within SPY-up, ON - OFF on rB: {a1-a0:+.3f} (se {np.hypot(s1, s0):.3f})")
    # continuous: per-date mean outcome vs 63d ratio change (rank correlation across dates)
    g = sub.groupby("date").agg(rB=("rB", "mean"), chg=("chg63", "first"))
    print(f"  Spearman(63d ratio change, date-mean rB): {g.rB.corr(g.chg, method='spearman'):+.3f} over {len(g)} dates")

report(ev, "ALL ENTRIES")
report(ev[ev.trend], "TREND ENTRIES (close > rising EMA50, like most BOT entries)")

ev.to_parquet("mtum_vlue_events.parquet")
ev["month"] = ev.date.dt.to_period("M")
def mclust(sub, col):
    g = sub.groupby("month")[col].mean(); return g.mean(), g.std() / np.sqrt(len(g)), len(g)
def diff(sub, col, mask_on, mask_off, f=mclust):
    a1, s1, n1 = f(sub[mask_on], col); a0, s0, n0 = f(sub[mask_off], col)
    return a1 - a0, np.hypot(s1, s0), n1, n0
print("== ROBUSTNESS (rB, winning-interval R; clusters = calendar months)")
for label, sub in (("all entries", ev), ("trend entries", ev[ev.trend])):
    d, se, n1, n0 = diff(sub, "rB", sub.on == 1, sub.on == 0)
    print(f"{label}: ON-OFF {d:+.3f} (se {se:.3f}, months {n1}/{n0})")
    u = sub[sub.spy_up50 == 1]; d, se, *_ = diff(u, "rB", u.on == 1, u.on == 0)
    print(f"   within SPY > SMA50: {d:+.3f} (se {se:.3f})")
    w = sub[sub.spy_up50 == 0]; d, se, *_ = diff(w, "rB", w.on == 1, w.on == 0)
    print(f"   within SPY < SMA50: {d:+.3f} (se {se:.3f})")
    d, se, *_ = diff(sub, "rB", sub.on_lag5 == 1, sub.on_lag5 == 0)
    print(f"   regime lagged 5 sessions: {d:+.3f} (se {se:.3f})")
    d, se, *_ = diff(sub, "rB", sub.spy_up50 == 1, sub.spy_up50 == 0)
    print(f"   [comparator] SPY > SMA50 vs below: {d:+.3f} (se {se:.3f})")
    # regression on date means: rB ~ on + spy_ret20 (does the regime survive the market's own 20d move?)
    g = sub.groupby("date").agg(rB=("rB", "mean"), on=("on", "first"), r20=("spy_ret20", "first")).dropna()
    X = np.column_stack([np.ones(len(g)), g.on, g.r20]); beta = np.linalg.lstsq(X, g.rB, rcond=None)[0]
    print(f"   date-level OLS rB ~ on + SPY 20d return: on {beta[1]:+.3f}, spy_ret20 {beta[2]:+.3f}")
    by_year = sub.assign(y=sub.date.dt.year).groupby("y").apply(
        lambda q: q[q.on == 1].rB.mean() - q[q.on == 0].rB.mean())
    print("   ON-OFF by year: " + ", ".join(f"{y}: {v:+.2f}" for y, v in by_year.items()))

# real BOT trades
tr = pd.read_sql("SELECT TradeNr, TradeDate, EventType, Return FROM Trades WHERE Strategy = 'BOT'", c)
ent = tr.groupby("TradeNr").TradeDate.min()
ret = tr.dropna(subset=["Return"]).groupby("TradeNr").Return.sum()
closed = tr[tr.EventType == "Close"].groupby("TradeNr").TradeDate.max()
real = pd.DataFrame({"entry": ent, "ror": ret, "closed": closed}).dropna()
real["entry"] = pd.to_datetime(real.entry.astype(int).astype(str))
real = real.merge(reg, left_on="entry", right_index=True, how="left").dropna(subset=["on"])
print(f"\n== REAL BOT TRADES with a Return and a regime at entry: {len(real)} "
      f"(entries {real.entry.min().date()} .. {real.entry.max().date()}, last close {int(real.closed.max())})")
for name, q in (("regime ON", real[real.on == 1]), ("regime OFF", real[real.on == 0])):
    print(f"  {name:<11} n {len(q):3d}  mean ror {q.ror.mean():+.3f}  median {q.ror.median():+.3f}  "
          f"win {(q.ror > 0).mean()*100:4.1f}%  P(>2) {(q.ror > 2).mean()*100:4.1f}%")
d = real[real.on == 1].ror.mean() - real[real.on == 0].ror.mean()
se = np.hypot(real[real.on == 1].ror.std() / np.sqrt((real.on == 1).sum()),
              real[real.on == 0].ror.std() / np.sqrt((real.on == 0).sum()))
print(f"  ON - OFF mean ror: {d:+.3f} (se {se:.3f}, naive: trades in the same weeks share the regime)")
legs = pd.read_sql("SELECT TradeNr, Right, Pos, Risk, EventType FROM Trades WHERE Strategy = 'BOT' AND EventType = 'Open'", c)
def side(g):
    # long if the net open position is long calls / short puts / long stock
    sc = 0.0
    for r in g.itertuples():
        sgn = 1 if r.Pos > 0 else -1
        sc += sgn * (-1 if r.Right == "P" else 1) * abs(r.Pos)
    return "long" if sc > 0 else ("short" if sc < 0 else "neutral")
real["side"] = legs.groupby("TradeNr").apply(side).reindex(real.index)
print("  sides:", real.side.value_counts().to_dict())
lg = real[real.side == "long"]
for name, q in (("LONG, regime ON", lg[lg.on == 1]), ("LONG, regime OFF", lg[lg.on == 0])):
    print(f"  {name:<17} n {len(q):3d}  mean ror {q.ror.mean():+.3f}  median {q.ror.median():+.3f}  win {(q.ror > 0).mean()*100:4.1f}%")
months = real.entry.dt.to_period("M")
print(f"  distinct entry months: {months.nunique()} (effective sample for a book-level variable)")
print(f"  Spearman(63d ratio change at entry, ror): {real.chg63.corr(real.ror, method='spearman'):+.3f}")
