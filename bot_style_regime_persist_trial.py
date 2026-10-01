"""MTUM/VLUE regime with persistence (no whipsaw on bull / bear traps).
Reuses the synthetic events written by bot_style_regime_trial.py (mtum_vlue_events.parquet, CWD). TODO 91, closed.
Variants, each a state that changes only on a confirmed move:
  raw     ratio > SMA50                                   (previous run, reference)
  band1   ON when ratio > SMA50*1.01, OFF when < SMA50*0.99, else keep state
  band2   same with 2%
  conf5   switch only after 5 consecutive closes on the other side of SMA50
  slope   SMA50 rising over 10 sessions
"""
import sqlite3, time, datetime as dt
import numpy as np, pandas as pd, yfinance as yf

def get(sym):
    for k in range(4):
        x = yf.download(sym, period="10y", auto_adjust=True, progress=False)
        if len(x):
            if isinstance(x.columns, pd.MultiIndex): x.columns = x.columns.get_level_values(0)
            return x.Close
        time.sleep(15)
    raise SystemExit(f"no data for {sym}")

ratio = (get("MTUM") / get("VLUE")).dropna()
ratio = ratio[ratio.index.date < dt.date.today()]
sma = ratio.rolling(50).mean()

def hysteresis(b):
    st, out = np.nan, []
    for r, m in zip(ratio.values, sma.values):
        if np.isfinite(m):
            if r > m * (1 + b): st = 1.0
            elif r < m * (1 - b): st = 0.0
            elif np.isnan(st): st = float(r > m)
        out.append(st)
    return pd.Series(out, index=ratio.index)

def confirm(k):
    above = (ratio > sma).astype(float).where(sma.notna())
    st, run, prev, out = np.nan, 0, None, []
    for a in above.values:
        if np.isnan(a): out.append(np.nan); continue
        run = run + 1 if a == prev else 1; prev = a
        if np.isnan(st): st = a
        elif a != st and run >= k: st = a
        out.append(st)
    return pd.Series(out, index=ratio.index)

reg = pd.DataFrame({
    "raw": (ratio > sma).astype(float).where(sma.notna()),
    "band1": hysteresis(0.01), "band2": hysteresis(0.02),
    "conf5": confirm(5),
    "slope": (sma > sma.shift(10)).astype(float).where(sma.shift(10).notna()),
}).dropna()
reg.index = pd.to_datetime(reg.index).normalize()

print(f"{'variant':<8}{'ON %':>6}{'spells':>8}{'median len':>12}{'mean len':>10}{'today':>7}")
for v in reg.columns:
    sp = reg.groupby((reg[v] != reg[v].shift()).cumsum()).size()
    print(f"{v:<8}{reg[v].mean()*100:>6.1f}{len(sp):>8}{sp.median():>12.0f}{sp.mean():>10.1f}{'ON' if reg[v].iloc[-1] else 'OFF':>7}")

ev = pd.read_parquet("mtum_vlue_events.parquet")
ev = ev[[c for c in ev.columns if c not in reg.columns and c != "on"]].merge(reg, left_on="date", right_index=True)
ev["month"] = ev.date.dt.to_period("M")

def mclust(q):
    g = q.groupby("month").rB.mean(); return g.mean(), g.std() / np.sqrt(len(g))

print("\nSynthetic, ON - OFF in R (winning-interval exit), month-clustered se")
print(f"{'variant':<8}{'all entries':>20}{'trend entries':>20}{'within SPY>SMA50':>20}{'years ON>OFF':>14}")
for v in reg.columns:
    cells = []
    for sub in (ev, ev[ev.trend], ev[ev.spy_up50 == 1]):
        a1, s1 = mclust(sub[sub[v] == 1]); a0, s0 = mclust(sub[sub[v] == 0])
        cells.append(f"{a1-a0:+.3f} ({np.hypot(s1, s0):.3f})")
    yrs = ev.assign(y=ev.date.dt.year).groupby("y").apply(lambda q: q[q[v] == 1].rB.mean() - q[q[v] == 0].rB.mean()).dropna()
    print(f"{v:<8}{cells[0]:>20}{cells[1]:>20}{cells[2]:>20}{f'{(yrs > 0).sum()}/{len(yrs)}':>14}")

# real BOT trades, long side
c = sqlite3.connect("file:C:/Users/aldoh/Documents/RApplication/data/mydb.db?mode=ro", uri=True)
tr = pd.read_sql("SELECT TradeNr, TradeDate, EventType, Return, Right, Pos FROM Trades WHERE Strategy = 'BOT'", c)
ent = tr.groupby("TradeNr").TradeDate.min()
ret = tr.dropna(subset=["Return"]).groupby("TradeNr").Return.sum()
op = tr[tr.EventType == "Open"]
side = op.assign(sc=np.sign(op.Pos) * np.where(op.Right == "P", -1, 1) * op.Pos.abs()).groupby("TradeNr").sc.sum()
real = pd.DataFrame({"entry": ent, "ror": ret, "side": np.sign(side)}).dropna()
real["entry"] = pd.to_datetime(real.entry.astype(int).astype(str))
real = real.merge(reg, left_on="entry", right_index=True)
lg = real[real.side > 0]
print(f"\nReal BOT long trades with a regime at entry: {len(lg)}")
print(f"{'variant':<8}{'ON: n / mean ror / win':>28}{'OFF: n / mean ror / win':>28}{'ON-OFF':>9}")
for v in reg.columns:
    on, off = lg[lg[v] == 1], lg[lg[v] == 0]
    print(f"{v:<8}{len(on):>10} / {on.ror.mean():+.2f} / {(on.ror>0).mean()*100:3.0f}%"
          f"{len(off):>12} / {off.ror.mean():+.2f} / {(off.ror>0).mean()*100:3.0f}%{on.ror.mean()-off.ror.mean():>+9.2f}")
