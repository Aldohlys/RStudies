"""Offline checks of the exit engine on synthetic marks (no TWS, no DB).

Run: python reports/bot_fwd/test_policy.py
"""
import datetime as dt
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import common as C  # noqa: E402
import policy as P  # noqa: E402

FEE = {"opt_leg": 0.91, "version": "test"}
D0 = dt.date(2026, 9, 1)


def days(n):
    out, d = [], D0
    while len(out) < n:
        if d.weekday() < 5:
            out.append(C.ymd(d))
        d += dt.timedelta(days=1)
    return out


def underlying(closes, highs=None, lows=None, opens=None):
    ds = days(len(closes))
    return {d: {"open": (opens or closes)[i], "high": (highs or closes)[i],
                "low": (lows or closes)[i], "close": closes[i],
                "bid_close": closes[i] - 0.01, "ask_close": closes[i] + 0.01}
            for i, d in enumerate(ds)}


def leg(bids, asks=None, bid_highs=None, deltas=None):
    ds = days(len(bids))
    return {d: {"bid_close": bids[i], "ask_close": (asks or [b + 0.2 for b in bids])[i],
                "bid_high": (bid_highs or bids)[i], "iv": 0.3,
                "delta": (deltas or [0.3] * len(bids))[i], "source": "bar"}
            for i, d in enumerate(ds)}


SIG = {"session_date": days(1)[0], "earnings_date": None, "group_name": None}


def outright(**kw):
    p = {"pos_id": "t|outright", "vehicle": "outright", "mult": 100, "shares": 1,
         "expiry": "20261009", "long_strike": 105.0, "iv_long": 0.3, "vol_bump": 0,
         "entry_price": 2.0, "cost_total": 200.91, "cost_total_mid": 190.91,
         "cost_R": 200.91, "spot_entry": 100.0, "target_v": 110.0, "stop_v": 95.0, "atr_v": 2.0}
    p.update(kw)
    return p


def check(name, cond):
    print(("ok   " if cond else "FAIL ") + name)
    return cond


def main():
    ok = True
    # Target: the option's bid high reaches the Black-Scholes value at target.
    U = underlying([100, 103, 106, 109, 110])
    L = leg([2.0, 2.8, 4.0, 5.5, 6.0], bid_highs=[2.0, 2.9, 4.2, 9.0, 9.5])
    r = P.simulate(outright(), SIG, U, L, {}, {}, FEE, P.POLICIES["P7"])
    ok &= check("outright target fills at the limit (P7)", r["exit_reason"] == "target" and r["R"] > 1)
    r = P.simulate(outright(), SIG, U, L, {}, {}, FEE, P.POLICIES["P0"])
    ok &= check("P0: no_asym exits at 2/3 of the way (106 of 95->110)",
                r["exit_reason"] == "no_asym" and r["hold_sessions"] == 2)

    # Stop: underlying closes below the stop -> exit at the closing bid.
    U = underlying([100, 98, 94.5, 96])
    L = leg([2.0, 1.4, 0.7, 0.9])
    r = P.simulate(outright(), SIG, U, L, {}, {}, FEE, P.POLICIES["P0"])
    ok &= check("outright stop on close", r["exit_reason"] == "stop" and r["exit_price"] == 0.7)

    # Momentum: no up-close in 5 sessions, price flat-to-down above the stop.
    U = underlying([100, 99.8, 99.6, 99.5, 99.4, 99.3, 99.2])
    L = leg([2.0, 1.9, 1.8, 1.7, 1.6, 1.5, 1.4])
    r = P.simulate(outright(), SIG, U, L, {}, {}, FEE, P.POLICIES["P0"])
    ok &= check("momentum exit at session 5", r["exit_reason"] == "momentum" and r["hold_sessions"] == 5)
    r1 = P.simulate(outright(), SIG, U, L, {}, {}, FEE, P.POLICIES["P1"])
    ok &= check("P1 ignores momentum (still open)", r1["status"] == "open")

    # Last week: DTE <= 7 closes the outright.
    U = underlying([100, 100.5, 101, 100.8, 101.2])
    L = leg([2.0, 2.0, 2.1, 2.0, 2.1])
    r = P.simulate(outright(expiry=C.ymd(dt.date.fromisoformat(days(5)[3]) + dt.timedelta(days=7)).replace("-", "")),
                   SIG, U, L, {}, {}, FEE, P.POLICIES["P5"])
    ok &= check("last week exit at DTE 7", r["exit_reason"] == "last_week" and r["hold_sessions"] == 3)

    # Stock: gap through the stop fills at the open, loss beyond 1R.
    stock = {"pos_id": "t|stock", "vehicle": "stock", "mult": 1, "shares": 60, "expiry": None,
             "entry_price": 100.0, "cost_total": 6001.0, "cost_total_mid": 6000.4,
             "cost_R": 301.0, "spot_entry": 100.0, "target_v": 110.0, "stop_v": 95.0, "atr_v": 2.0,
             "vol_bump": 0}
    U = underlying([100, 99, 92], lows=[100, 98, 91], opens=[100, 99.5, 93])
    r = P.simulate(stock, SIG, U, {}, {}, {}, FEE, P.POLICIES["P0"])
    ok &= check("stock gap stop fills at the open", r["exit_reason"] == "stop" and r["exit_price"] == 93
                and r["R"] < -1)

    # Spread: combo closing bid reaches 80% of width.
    spread = {"pos_id": "t|spread", "vehicle": "spread", "mult": 100, "shares": 1,
              "expiry": "20261009", "long_strike": 105.0, "short_strike": 115.0, "width": 10.0,
              "iv_long": 0.3, "vol_bump": 0, "entry_price": 2.5, "cost_total": 251.82,
              "cost_total_mid": 231.82, "cost_R": 251.82, "spot_entry": 100.0,
              "target_v": 120.0, "stop_v": 95.0, "atr_v": 2.0}
    U = underlying([100, 105, 112, 118])
    L = leg([3.0, 5.0, 8.5, 13.5])
    S = leg([0.5, 1.0, 2.0, 4.5], asks=[0.6, 1.1, 2.1, 4.6])
    r = P.simulate(spread, SIG, U, L, S, {}, FEE, P.POLICIES["P7"])
    ok &= check("spread target at 80% of width", r["exit_reason"] == "target" and r["exit_price"] == 8.0)

    print("ALL OK" if ok else "SOME FAILED")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
