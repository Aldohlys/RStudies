"""Underlying shadow (proposal §5.1): forward move of every tradable row of each
session's first bot_daily run, all tiers. Price data only (Yahoo daily bars), so
it covers non-US names and gives LOW / WATCH as the control group.

Measured from the row's px (the price the run read), in ATR of that row:
ret10_atr / ret20_atr  close 10 / 20 sessions later
hit_up_first           1 = +1.5 ATR touched before -1.5 ATR within 20 sessions,
                       0 = -1.5 ATR first (or both in the same session), NULL = neither
mfe_atr / mae_atr      best high / worst low over the 20 sessions
Signed by direction (a short gains when price falls).
"""
import datetime as dt

import common as C

HORIZON, TOUCH = 20, 1.5


def run(conn, log=print):
    rows = [dict(r) for r in conn.execute(
        "SELECT * FROM bot_fwd_underlying WHERE sessions_seen IS NULL OR sessions_seen < ?",
        (HORIZON,))]
    rows = [r for r in rows if r["px"] and r["atr"]]
    if not rows:
        log("Shadow: nothing pending")
        return
    start = min(r["session_date"] for r in rows)
    ys = sorted({r["yahoo"] for r in rows})
    try:
        import yfinance as yf
        data = yf.download(ys, start=start, interval="1d", auto_adjust=False,
                           progress=False, threads=True, group_by="ticker")
    except Exception as e:
        log(f"Shadow: Yahoo download failed ({e})")
        return
    n = 0
    for r in rows:
        try:
            df = data[r["yahoo"]] if len(ys) > 1 else data
        except KeyError:
            continue
        df = df.dropna(subset=["Close"])
        fwd = df[df.index.strftime("%Y-%m-%d") > r["session_date"]].head(HORIZON)
        if fwd.empty:
            continue
        sgn = -1 if r["direction"] == "short" else 1
        px, atr = r["px"], r["atr"]
        closes = list(fwd["Close"])
        hit = None
        for hi, lo in zip(fwd["High"], fwd["Low"]):
            up = (hi - px) * sgn >= TOUCH * atr if sgn == 1 else (px - lo) >= TOUCH * atr
            dn = (px - lo) >= TOUCH * atr if sgn == 1 else (hi - px) >= TOUCH * atr
            if dn:
                hit = 0
                break
            if up:
                hit = 1
                break
        best = fwd["High"].max() if sgn == 1 else fwd["Low"].min()
        worst = fwd["Low"].min() if sgn == 1 else fwd["High"].max()
        conn.execute(
            """UPDATE bot_fwd_underlying SET ret10_atr = ?, ret20_atr = ?, hit_up_first = ?,
               mfe_atr = ?, mae_atr = ?, sessions_seen = ? WHERE session_date = ? AND sym = ?""",
            (sgn * (closes[9] - px) / atr if len(closes) >= 10 else None,
             sgn * (closes[19] - px) / atr if len(closes) >= 20 else None,
             hit, sgn * (best - px) / atr, sgn * (px - worst) / atr,
             len(closes), r["session_date"], r["sym"]))
        n += 1
    conn.commit()
    log(f"Shadow: {n} of {len(rows)} pending rows updated")
