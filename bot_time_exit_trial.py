"""S-8 (TODO 82): winning-interval exit vs actual, on the real BOT trades.

Run (all three write / read <dir>):
    python bot_time_exit_trial_trades.py <dir>
    Rscript bot_time_exit_trial_targets.R <dir>   (from RStudies: writes <dir>/s8_targets.csv)
    python bot_time_exit_trial.py <dir>

Equity curve of a trade on snapshot date D (last snapshot of the day):
    P&L(D) = sum(Total of its Trades rows with TradeDate <= D) + sum(mktValue of its open legs on D)
Legs on D: snapshot rows with the trade's TradeNr, or with TradeNr NULL and an
Instrument the trade traded (snapshots written before the leg was booked).
Rule (sess_target_p75): if the underlying has not touched the entry-day target by
the p75 session and the trade is still open then, exit at that day's mark
(last snapshot on or before the p75 date). Otherwise the trade is unchanged.
Also the p90 variant (stricter: exit if not touched by the p90 session).
R = |realised P&L / stored Return| (the trade's initial risk).
Marks are snapshot mid/marks: an exit at the bid would do slightly worse.
"""
import sqlite3, numpy as np, pandas as pd
import sys
S = sys.argv[1] if len(sys.argv) > 1 else "."
c = sqlite3.connect("file:C:/Users/aldoh/Documents/RApplication/data/mydb.db?mode=ro", uri=True)
tr = pd.read_csv(f"{S}/s8_trades.csv").set_index("TradeNr")
tg = pd.read_csv(f"{S}/s8_targets.csv").set_index("TradeNr")
tg = tg[tg.note.isna() | (tg.note == "")]
df = tr.join(tg[["target", "target_source", "dist_atr", "sess_p90", "sess_p75", "touch_session",
                 "touch_date", "exit_date_p75"]], how="inner")
legs = pd.read_sql("SELECT TradeNr, TradeDate, Instrument, Total FROM Trades WHERE Strategy='BOT' AND Account='U1804173'", c)
snap = pd.read_sql("SELECT TradeNr, date, heure, Instrument, mktValue FROM U1804173", c)
last = snap.groupby("date").heure.max().rename("hmax")
snap = snap.merge(last, left_on="date", right_index=True)
snap = snap[snap.heure == snap.hmax]
sdates = np.array(sorted(snap.date.unique()))

def pnl_on(nr, D):
    lg = legs[legs.TradeNr == nr]
    cash = lg[lg.TradeDate <= D].Total.sum()
    inst = set(lg.Instrument)
    rows = snap[(snap.date == D) & ((snap.TradeNr == nr) | (snap.TradeNr.isna() & snap.Instrument.isin(inst)))]
    return cash + rows.mktValue.sum()

def mark_at(nr, day, entry, close):
    """P&L at the last snapshot on or before `day` (YYYYMMDD int), not before entry."""
    ok = sdates[(sdates <= day) & (sdates >= entry)]
    if not len(ok): return np.nan, None
    return pnl_on(nr, ok[-1]), ok[-1]

def to_int(s): return int(str(s).replace("-", "")) if isinstance(s, str) and s else None

rows = []
for nr, t in df.iterrows():
    entry, close = int(t.entry), int(t.close)
    actual = t.ret
    touch = to_int(t.touch_date)
    res = {"TradeNr": nr, "yahoo": t.yahoo, "dir": t.direction, "actual": actual,
           "dist_atr": t.dist_atr, "sess_p75": t.sess_p75, "sess_p90": t.sess_p90,
           "held_days": (pd.Timestamp(str(close)) - pd.Timestamp(str(entry))).days}
    for var, ex_col in (("p75", "exit_date_p75"),):
        ex = to_int(t[ex_col])
        if ex is None or close <= ex or (touch is not None and touch <= ex):
            res[f"rule_{var}"] = actual; res[f"acted_{var}"] = False
        else:
            p, d = mark_at(nr, ex, entry, close)
            res[f"rule_{var}"] = p / t.R if np.isfinite(p) else actual
            res[f"acted_{var}"] = bool(np.isfinite(p)); res["exit_snap"] = d
    rows.append(res)
r = pd.DataFrame(rows)
print(f"trades compared: {len(r)} ({(r.dir == 'long').sum()} long, {(r.dir == 'short').sum()} short)")
print(f"entry-day target distance (ATR): median {r.dist_atr.median():.2f}; sess_p75 median {r.sess_p75.median():.1f}, "
      f"p90 median {r.sess_p90.median():.1f}; actual holding median {r.held_days.median():.0f} calendar days")
acted = r[r.acted_p75]
print(f"rule acts (target not touched by the p75 session, trade still open): {len(acted)} of {len(r)}")

def stats(x):
    return (f"mean {x.mean():+.3f} R  median {x.median():+.3f}  win {(x > 0).mean()*100:4.1f}%  "
            f"P(>1) {(x > 1).mean()*100:4.1f}%  P(>2) {(x > 2).mean()*100:4.1f}%")
print("\nALL TRADES")
print("  actual          ", stats(r.actual))
print("  winning-int exit", stats(r.rule_p75))
d = r.rule_p75 - r.actual
print(f"  difference: mean {d.mean():+.3f} R (se {d.std()/np.sqrt(len(d)):.3f}); "
      f"rule better on {(d > 0).sum()}, worse on {(d < 0).sum()}, same on {(d == 0).sum()}")
print("\nTRADES WHERE THE RULE ACTS")
print("  actual          ", stats(acted.actual))
print("  winning-int exit", stats(acted.rule_p75))
cut_winners = acted[acted.actual > 1]
print(f"  of these, actual winners > 1 R cut by the rule: {len(cut_winners)} "
      f"(actual mean {cut_winners.actual.mean():+.2f} R -> rule {cut_winners.rule_p75.mean():+.2f} R)")
saved = acted[acted.actual < 0]
print(f"  actual losers exited earlier: {len(saved)} (actual mean {saved.actual.mean():+.2f} R -> rule {saved.rule_p75.mean():+.2f} R)")
r.to_csv(f"{S}/s8_result.csv", index=False)
pd.set_option("display.width", 200)
print("\nlargest changes:")
print(r.assign(diff=d).reindex(d.abs().sort_values(ascending=False).index).head(10)[
    ["TradeNr", "yahoo", "dir", "dist_atr", "sess_p75", "held_days", "actual", "rule_p75"]].round(2).to_string(index=False))
