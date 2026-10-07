"""Daily marks per contract (proposal §4.1).

Stocks: IBKR daily TRADES / BID / ASK bars. Options: IBKR serves no daily bars
for options (error 162, "No data of type EODChart", checked 2026-10-07), so the
session is built from 1-hour BID and ASK bars: closing bid/ask = last bar's
close, bid high/low = extremes of the bars after the first (the opening bar's
quotes are noise: AAPL 355C on 10-07 showed a 1.52-4.35 bid range in the first
half hour). Missing sessions are back-filled while the contract lives; a session
with an underlying bar and no option bar gets a Black-Scholes mark from the
underlying close and the last IV (source = model).
"""
import datetime as dt

import common as C


def _complete_through():
    now = dt.datetime.now(C.ET)
    d = now.date() if now.time() >= dt.time(16, 15) else now.date() - dt.timedelta(days=1)
    while not C.is_weekday(d):
        d -= dt.timedelta(days=1)
    return d


def _bar_date(b):
    d = b.date
    return d if isinstance(d, dt.date) and not isinstance(d, dt.datetime) else d.astimezone(C.ET).date()


def _upsert(conn, row):
    keys = list(row)
    conn.execute(f"INSERT OR REPLACE INTO bot_fwd_mark ({','.join(keys)}) VALUES ({','.join('?' * len(keys))})",
                 [row[k] for k in keys])


def run(conn, ib, log=print):
    from ib_async import Contract
    through = _complete_through()
    todo = [dict(c) for c in conn.execute(
        """SELECT * FROM bot_fwd_contract
           WHERE first_date <= ? AND (last_mark IS NULL OR last_mark < MIN(mark_until, ?))
           ORDER BY kind DESC""", (C.ymd(through), C.ymd(through)))]  # STK first
    jobs = []
    for c in todo:
        start = dt.date.fromisoformat(c["first_date"])
        if c["last_mark"]:
            start = max(start, dt.date.fromisoformat(c["last_mark"]) + dt.timedelta(days=1))
        end = min(through, dt.date.fromisoformat(c["mark_until"]))
        if start <= end:
            c["start"], c["end"] = start, end
            c["days"] = min(max((dt.date.today() - start).days + 2, 2), 60)
            c["con"] = Contract(conId=c["conid"], exchange="SMART")
            jobs.append(c)
    if jobs:
        ib.qualifyContracts(*[c["con"] for c in jobs])
    n_rows = n_fail = 0
    for kind, whats, size in (("STK", ("TRADES", "BID", "ASK"), "1 day"),
                              ("OPT", ("BID", "ASK"), "1 hour")):
        group = [c for c in jobs if c["kind"] == kind]
        reqs = [(c["con"], "", f"{c['days']} D", size, w) for c in group for w in whats]
        res = C.hist_many(ib, reqs)
        for i, c in enumerate(group):
            got = dict(zip(whats, res[i * len(whats):(i + 1) * len(whats)]))
            if any(v is None for v in got.values()):
                n_fail += 1          # timed out: retried next run, last_mark unchanged
                continue
            rows = _stk_rows(c, got) if kind == "STK" else _opt_rows(c, got)
            for r in rows.values():
                _upsert(conn, r)
            if kind == "OPT":
                _greeks_and_model(conn, c, c["start"], c["end"])
            # An answered request with no bars (no quotes that day) still
            # advances last_mark; model marks cover the gap.
            conn.execute("UPDATE bot_fwd_contract SET last_mark = ? WHERE conid = ?",
                         (C.ymd(c["end"]), c["conid"]))
            conn.commit()
            n_rows += len(rows)
    log(f"Marks: {len(jobs)} contracts due, {n_rows} session rows written, "
        f"{n_fail} timed out (retried next run)")


def _stk_rows(c, got):
    rows = {}
    for what, bars in got.items():
        for b in bars:
            d = _bar_date(b)
            if not (c["start"] <= d <= c["end"]):
                continue
            r = rows.setdefault(d, {"conid": c["conid"], "date": C.ymd(d), "kind": "STK",
                                    "source": "bar"})
            if what == "TRADES":
                r.update(open=b.open, high=b.high, low=b.low, close=b.close)
            elif what == "BID":
                r["bid_close"] = b.close
            else:
                r["ask_close"] = b.close
    return rows


def _opt_rows(c, got):
    per_day = {}
    for what, bars in got.items():
        for b in bars:
            d = _bar_date(b)
            if c["start"] <= d <= c["end"]:
                per_day.setdefault(d, {"BID": [], "ASK": []})[what].append(b)
    rows = {}
    for d, bb in per_day.items():
        bid = sorted(bb["BID"], key=lambda b: b.date)
        ask = sorted(bb["ASK"], key=lambda b: b.date)
        body = bid[1:] or bid
        rows[d] = {"conid": c["conid"], "date": C.ymd(d), "kind": "OPT", "source": "bar",
                   "bid_close": bid[-1].close if bid else None,
                   "ask_close": ask[-1].close if ask else None,
                   "bid_high": max(b.high for b in body) if body else None,
                   "bid_low": min(b.low for b in body) if body else None}
    return rows


def _underlying_conid(conn, c):
    r = conn.execute(
        """SELECT stk_conid FROM bot_fwd_position WHERE long_conid = ? OR short_conid = ? LIMIT 1""",
        (c["conid"], c["conid"])).fetchone()
    return r["stk_conid"] if r else None


def _greeks_and_model(conn, c, start, end):
    """IV and delta per option mark from the mid and the underlying close; model
    marks for sessions the underlying traded and the option has no bar."""
    u = _underlying_conid(conn, c)
    if u is None:
        return
    spot = {r["date"]: r["close"] for r in conn.execute(
        "SELECT date, close FROM bot_fwd_mark WHERE conid = ? AND close IS NOT NULL", (u,))}
    marks = {r["date"]: dict(r) for r in conn.execute(
        "SELECT * FROM bot_fwd_mark WHERE conid = ? ORDER BY date", (c["conid"],))}
    pos_iv = conn.execute(
        "SELECT iv_long FROM bot_fwd_position WHERE long_conid = ? AND iv_long IS NOT NULL LIMIT 1",
        (c["conid"],)).fetchone()
    last_iv = pos_iv["iv_long"] if pos_iv else 0.3
    for d in sorted(spot):
        if d < C.ymd(dt.date.fromisoformat(c["first_date"])) or d > C.ymd(end):
            continue
        S = spot[d]
        T = C.year_frac(dt.date.fromisoformat(d), c["expiry"])
        m = marks.get(d)
        if m and m["bid_close"] is not None and m["ask_close"] is not None:
            mid = (m["bid_close"] + m["ask_close"]) / 2
            iv = C.implied_vol(mid, S, c["strike"], T, c["right"]) if mid > 0 else None
            iv = iv or last_iv
            last_iv = iv
            g = C.bs(S, c["strike"], T, iv, c["right"])
            conn.execute("UPDATE bot_fwd_mark SET iv = ?, delta = ? WHERE conid = ? AND date = ?",
                         (iv, g["delta"] if g else None, c["conid"], d))
        elif dt.date.fromisoformat(d) >= start:
            g = C.bs(S, c["strike"], T, last_iv, c["right"])
            if g:
                _upsert(conn, {"conid": c["conid"], "date": d, "kind": "OPT", "source": "model",
                               "bid_close": g["price"], "ask_close": g["price"],
                               "bid_high": g["price"], "bid_low": g["price"],
                               "iv": last_iv, "delta": g["delta"]})
