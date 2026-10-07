"""Exit engine (proposal §4.2-4.4).

Exits are computed from the stored marks, never stored with the entry, so every
policy below is replayed over every position on each run. A threshold change is
a new POLICY_VERSION and a full replay, never an edit of past rows.

Fill rules on daily data:
- resting limits: outright fills at the limit when the session's bid high (1-hour
  bars after the opening one) reaches it; spread fills at the limit when the
  combo's closing bid reaches it (no intraday combo series); stock fills at
  max(open, target) when the high reaches the target.
- stock stop order: min(open, stop) when the low reaches the stop.
- every other exit: the session's closing bid (closing mid under P6).
- spreads: long leg at its ask on entry and its bid on exit, short leg at its
  mid both ways (decided 2026-10-08).
"""
import datetime as dt
import math

import common as C

POLICY_VERSION = "v2-2026-10-08"  # v2: no_asym out of P0

# no_asym (rule 10) left P0 on 2026-10-08: with a fixed stop it fires once price
# is two-thirds of the way from the stop to the target, before the target limit
# can fill. P7 keeps it, to measure what it would have done.
BASE = {"rules": {"last_week", "spread_expiry", "target", "stop", "dead_delta", "earnings",
                  "momentum", "sector", "time_stop", "max_hold"},
        "time_stop_session": 10, "target_outright": "bs", "target_spread": 0.80,
        "fill": "natural"}

POLICIES = {
    "P0": dict(BASE),
    "P1": dict(BASE, rules={"last_week", "spread_expiry", "stop", "max_hold"}),
    "P2a": dict(BASE, target_outright=2.0, target_spread=0.70),
    "P2b": dict(BASE, target_outright=3.0, target_spread=0.90),
    "P3": dict(BASE, rules=BASE["rules"] - {"time_stop"}),
    "P4": dict(BASE, time_stop_session=7),
    "P5": dict(BASE, rules={"last_week", "spread_expiry", "max_hold"}),
    "P6": dict(BASE, fill="mid"),
    "P7": dict(BASE, rules=BASE["rules"] | {"no_asym"}),   # P0 + exit when asymmetry is gone
}

MOMENTUM_MIN_DAYS = 5           # bot_momentum_monitor.py MIN_DAYS
DEAD_DELTA = 0.10
EARNINGS_SESSIONS = 2
NO_ASYM_RATIO = 0.5
STOCK_MAX_HOLD = 40


# ── Group index for the sector rule ──────────────────────────────────────────
def group_indices(conn, groups):
    """Equal-weight index of each correlation group's members (daily returns
    averaged, Yahoo closes) with its EMA20. {group: {date: (close, ema20)}}."""
    groups = {g for g in groups if g and g != "Ungrouped"}
    if not groups:
        return {}
    members = {}
    for r in conn.execute(
            """SELECT u.Cluster g, COALESCE(NULLIF(t.YahooName, ''), u.Symbol) y
               FROM ScannerUniverse u LEFT JOIN Tickers t ON t.Name = u.Symbol
               WHERE u.Cluster IS NOT NULL"""):
        if r["g"] in groups:
            members.setdefault(r["g"], []).append(r["y"])
    allsyms = sorted({y for v in members.values() for y in v})
    if not allsyms:
        return {}
    try:
        import yfinance as yf
        px = yf.download(allsyms, period="1y", interval="1d", auto_adjust=True,
                         progress=False, threads=True)["Close"]
    except Exception:
        return {}
    if hasattr(px, "to_frame") and not hasattr(px, "columns"):
        px = px.to_frame(allsyms[0])
    rets = px.pct_change()
    out = {}
    for g, ys in members.items():
        cols = [y for y in ys if y in rets.columns]
        if not cols:
            continue
        idx = (1 + rets[cols].mean(axis=1, skipna=True).fillna(0)).cumprod()
        ema = idx.ewm(span=20, adjust=False).mean()
        out[g] = {d.strftime("%Y-%m-%d"): (float(c), float(e))
                  for d, c, e in zip(idx.index, idx.values, ema.values)}
    return out


# ── Replay ───────────────────────────────────────────────────────────────────
def _weekdays_between(d0, d1):
    """Weekdays after d0 up to and including d1."""
    n, d = 0, d0
    while d < d1:
        d += dt.timedelta(days=1)
        if d.weekday() < 5:
            n += 1
    return n


def _exit_fee(pos, fee):
    if pos["vehicle"] == "stock":
        return C.stock_fee(pos["shares"])
    return fee["opt_leg"] * (2 if pos["vehicle"] == "spread" else 1)


def simulate(pos, sig, U, L, S, gidx, fee, pol):
    """One position under one policy. U/L/S: {date: mark} for the underlying and
    the long/short legs. Returns the bot_fwd_result row."""
    veh = pos["vehicle"]
    mult = pos["mult"] or 1
    qty = pos["shares"] or 1
    mid_fill = pol["fill"] == "mid"
    cost = pos["cost_total_mid"] if mid_fill else pos["cost_total"]
    cost_R = pos["cost_R"]
    fee_x = _exit_fee(pos, fee)
    entry_d = dt.date.fromisoformat(sig["session_date"])
    expiry = dt.datetime.strptime(pos["expiry"], "%Y%m%d").date() if pos["expiry"] else None
    spot0, tgt, stp, atr = pos["spot_entry"], pos["target_v"], pos["stop_v"], pos["atr_v"]
    earn = dt.date.fromisoformat(sig["earnings_date"]) if sig["earnings_date"] else None
    grp = gidx.get(sig["group_name"]) if gidx else None
    g0 = grp.get(sig["session_date"]) if grp else None

    days = sorted(d for d in U if d >= sig["session_date"])
    if veh != "stock":
        days = [d for d in days if d in L and (veh == "outright" or d in S)]

    def values(d):
        """(bid value, mid value, bid high) per unit of the structure."""
        if veh == "stock":
            u = U[d]
            b = u["bid_close"] if u["bid_close"] else u["close"]
            a = u["ask_close"] if u["ask_close"] else u["close"]
            return b, (b + a) / 2, u["high"]
        l = L[d]
        if l["bid_close"] is None or l["ask_close"] is None:
            return None, None, None
        if veh == "outright":
            return l["bid_close"], (l["bid_close"] + l["ask_close"]) / 2, l["bid_high"]
        s = S[d]
        if s["bid_close"] is None or s["ask_close"] is None:
            return None, None, None
        # Exit mirrors the entry: long leg at its bid, short leg at its mid.
        b = max(l["bid_close"] - (s["bid_close"] + s["ask_close"]) / 2, 0.0)
        m = ((l["bid_close"] + l["ask_close"]) - (s["bid_close"] + s["ask_close"])) / 2
        return b, max(m, 0.0), None

    def pnl_at(price):
        return price * mult * qty - fee_x - cost

    path, exit_ = [], None
    up_days, hi_max, lo_min = 0, -math.inf, math.inf
    prev_close = U[days[0]]["close"] if days else None
    model_n = 0
    for i, d in enumerate(days):
        if i == 0:
            continue
        dd = dt.date.fromisoformat(d)
        u = U[d]
        close = u["close"]
        if close is None:
            continue
        b, m, bh = values(d)
        if b is None or m is None:
            continue
        if veh != "stock" and (L[d]["source"] == "model" or (veh == "spread" and S[d]["source"] == "model")):
            model_n += 1
        up_days += int(prev_close is not None and close > prev_close)
        prev_close = close
        hi_max = max(hi_max, u["high"] if u["high"] is not None else close)
        lo_min = min(lo_min, u["low"] if u["low"] is not None else close)
        exit_px = m if mid_fill else b
        path.append((pnl_at(exit_px)) / cost_R)
        dte = (expiry - dd).days if expiry else None
        R = pol["rules"]

        def out(reason, price):
            return {"date": d, "reason": reason, "price": price, "mid": m, "i": i}

        if "last_week" in R and veh == "outright" and dte is not None and dte <= 7:
            exit_ = out("last_week", exit_px); break
        if "spread_expiry" in R and veh == "spread" and dte is not None and dte <= 5:
            exit_ = out("spread_expiry", exit_px); break
        if "target" in R:
            if veh == "stock":
                if u["high"] is not None and u["high"] >= tgt:
                    exit_ = out("target", max(tgt, u["open"] or tgt)); break
            elif veh == "outright":
                t_o = pol["target_outright"]
                if t_o == "bs":
                    T = C.year_frac(dd, pos["expiry"])
                    iv = (pos["iv_long"] or 0.3) + (0.03 if pos["vol_bump"] else 0.0)
                    lim = C.bs(tgt, pos["long_strike"], T, iv)["price"]
                else:
                    lim = t_o * pos["entry_price"]
                if lim > pos["entry_price"] and bh is not None and bh >= lim:
                    exit_ = out("target", lim); break
            else:
                lim = pol["target_spread"] * pos["width"]
                if b >= lim:
                    exit_ = out("target", lim); break
        if "stop" in R:
            if veh == "stock":
                if u["low"] is not None and u["low"] <= stp:
                    exit_ = out("stop", min(stp, u["open"] or stp)); break
            elif close < stp:
                exit_ = out("stop", exit_px); break
        if "dead_delta" in R and veh == "outright":
            dl = L[d]["delta"]
            if dl is not None and dl < DEAD_DELTA:
                exit_ = out("dead_delta", exit_px); break
        if "earnings" in R and earn is not None and earn >= dd \
                and _weekdays_between(dd, earn) <= EARNINGS_SESSIONS:
            exit_ = out("earnings", exit_px); break
        if "momentum" in R and i >= MOMENTUM_MIN_DAYS:
            if up_days / i < 0.5 or hi_max <= spot0:
                exit_ = out("momentum", exit_px); break
        if "sector" in R and grp and g0 and g0[0] >= g0[1]:
            gd = grp.get(d)
            if gd and gd[0] < gd[1]:
                exit_ = out("sector", exit_px); break
        if "time_stop" in R and veh != "stock" and i == pol["time_stop_session"]:
            if b * mult < pos["cost_total"] and close <= spot0:
                exit_ = out("time_stop", exit_px); break
        if "no_asym" in R and (tgt - close) < NO_ASYM_RATIO * (close - stp):
            exit_ = out("no_asym", exit_px); break
        if "max_hold" in R and veh == "stock" and i >= STOCK_MAX_HOLD:
            exit_ = out("max_hold", exit_px); break

    # Window end without an exit: the marks stop at DTE 5 / 30 sessions / 40
    # sessions (stock); then the position is closed at the last mark.
    status = "closed" if exit_ else "open"
    if not exit_ and days and len(days) > 1:
        last = days[-1]
        window_over = (veh == "stock" and len(days) - 1 >= STOCK_MAX_HOLD) or \
                      (veh != "stock" and (len(days) - 1 >= C.OPT_MARK_SESSIONS or
                                           (expiry and (expiry - dt.date.fromisoformat(last)).days <= 5)))
        if window_over:
            b, m, _ = values(last)
            exit_ = {"date": last, "reason": "data_end", "price": m if mid_fill else b,
                     "mid": m, "i": len(days) - 1}
            status = "closed"

    res = {"pos_id": pos["pos_id"], "status": status, "policy_version": POLICY_VERSION,
           "computed_at": C.now_iso(), "model_marks": model_n,
           "mfe_R": max(path) if path else None, "mae_R": min(path) if path else None,
           "spot_mfe_atr": (hi_max - spot0) / atr if atr and hi_max > -math.inf else None,
           "spot_mae_atr": (spot0 - lo_min) / atr if atr and lo_min < math.inf else None}
    if exit_:
        pnl = pnl_at(exit_["price"])
        res.update(exit_date=exit_["date"], exit_reason=exit_["reason"], exit_price=exit_["price"],
                   exit_mid=exit_["mid"], fee_exit=fee_x, pnl_usd=pnl, R=pnl / cost_R,
                   hold_sessions=exit_["i"])
    elif len(days) > 1:
        b, m, _ = values(days[-1])
        pnl = pnl_at(m if mid_fill else b)
        res.update(pnl_usd=pnl, R=pnl / cost_R, hold_sessions=len(days) - 1)
    else:
        res.update(hold_sessions=0)

    if veh != "stock" and len(days) > 1:
        end_i = exit_["i"] if exit_ else len(days) - 1
        res.update(_greeks_split(pos, U, L, S, days[:end_i + 1], mult))
    return res


def _greeks_split(pos, U, L, S, days, mult):
    """Delta / vega / theta P&L summed day by day from the previous close's
    Black-Scholes greeks; 'other' is the rest of the mid-to-mid change."""
    legs = [(pos["long_strike"], L, 1)]
    if pos["vehicle"] == "spread":
        legs.append((pos["short_strike"], S, -1))
    dp = vp = tp = total = 0.0
    for a, b in zip(days[:-1], days[1:]):
        Sa, Sb = U[a]["close"], U[b]["close"]
        da, db = dt.date.fromisoformat(a), dt.date.fromisoformat(b)
        for K, M, sgn in legs:
            ma, mb = M[a], M[b]
            if None in (Sa, Sb, ma["iv"], mb["iv"], ma["bid_close"], mb["bid_close"]):
                continue
            g = C.bs(Sa, K, C.year_frac(da, pos["expiry"]), ma["iv"])
            dp += sgn * g["delta"] * (Sb - Sa) * mult
            vp += sgn * g["vega"] * (mb["iv"] - ma["iv"]) * mult
            tp += sgn * g["theta"] * (db - da).days * mult
            total += sgn * (((mb["bid_close"] + mb["ask_close"]) - (ma["bid_close"] + ma["ask_close"])) / 2) * mult
    return {"delta_pnl": dp, "vega_pnl": vp, "theta_pnl": tp, "other_pnl": total - dp - vp - tp}


def run(conn, log=print):
    fee = C.fee_model(conn)
    pos_rows = [dict(r) for r in conn.execute("SELECT * FROM bot_fwd_position")]
    sigs = {r["signal_id"]: dict(r) for r in conn.execute("SELECT * FROM bot_fwd_signal")}
    marks = {}
    for r in conn.execute("SELECT * FROM bot_fwd_mark"):
        marks.setdefault(r["conid"], {})[r["date"]] = dict(r)
    gidx = group_indices(conn, {sigs[p["signal_id"]]["group_name"] for p in pos_rows
                                if p["signal_id"] in sigs})
    cols = [r[1] for r in conn.execute("PRAGMA table_info(bot_fwd_result)")]
    n = 0
    for p in pos_rows:
        sig = sigs.get(p["signal_id"])
        U = marks.get(p["stk_conid"], {})
        if not sig or not U:
            continue
        L = marks.get(p["long_conid"], {}) if p["long_conid"] else {}
        S = marks.get(p["short_conid"], {}) if p["short_conid"] else {}
        for name, pol in POLICIES.items():
            if p["vehicle"] == "stock" and name in ("P2a", "P2b", "P3", "P4"):
                continue  # the variants change option-only rules
            res = simulate(p, sig, U, L, S, gidx, fee, pol)
            res["policy"] = name
            keys = [k for k in res if k in cols]
            conn.execute(f"INSERT OR REPLACE INTO bot_fwd_result ({','.join(keys)}) "
                         f"VALUES ({','.join('?' * len(keys))})", [res[k] for k in keys])
            n += 1
    conn.commit()
    log(f"Policies: {n} position x policy rows replayed ({POLICY_VERSION})")
