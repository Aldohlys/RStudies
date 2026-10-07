"""Signal intake and entry pricing (proposal §2, §3).

Every bot_daily file of the last CATCHUP_DAYS days is re-read on each run. A name
in BOT / BOT- / COUNTER-TREND opens one position per vehicle per session, from
the first run of the session that shows it. Entries are priced at the run's
timestamp: live quotes when the run is fresh, otherwise rebuilt from IBKR 5-minute
BID/ASK bars (the contracts are still live, so IBKR serves their history).
"""
import csv
import datetime as dt
import glob
import json
import math
import os
import re

import common as C

FILE_RE = re.compile(r"bot_daily_(\d{8})(?:_(\d{4}))?\.csv$")


# ── bot_daily files ──────────────────────────────────────────────────────────
def run_files(since):
    out = []
    for f in glob.glob(os.path.join(C.CSV_DIR, "bot_daily_*.csv")):
        m = FILE_RE.search(os.path.basename(f))
        if not m:
            continue
        d = dt.datetime.strptime(m.group(1) + (m.group(2) or "1712"), "%Y%m%d%H%M")
        ts = d.replace(tzinfo=C.LOCAL)
        if ts.date() >= since:
            out.append((ts, f))
    return sorted(out)


def read_rows(path):
    """The detail sidecar (bot_daily_detail/<same name>) when it exists, else
    the daily file. Files of 2026-09-24..28 have no tradable column: skipped."""
    side = os.path.join(C.DETAIL_DIR, os.path.basename(path))
    src = side if os.path.exists(side) else path
    with open(src, newline="", encoding="utf-8-sig") as fh:
        rows = list(csv.DictReader(fh, delimiter=";"))
    if not rows or "tradable" not in rows[0]:
        return [], src
    return rows, src


# ── Reference data ───────────────────────────────────────────────────────────
class Ref:
    def __init__(self, conn):
        self.conn = conn
        self.tickers = {r["Name"]: dict(r) for r in conn.execute("SELECT * FROM Tickers")}
        self.univ = {r["Symbol"]: dict(r) for r in conn.execute("SELECT * FROM ScannerUniverse")}
        self._earn = {}

    def vol(self, sym, session_date):
        r = self.conn.execute(
            """SELECT iv30, ivp, rv30, vrp FROM Prices WHERE sym = ? AND iv30 IS NOT NULL
               AND substr(datetime, 1, 8) <= ? ORDER BY datetime DESC LIMIT 1""",
            (sym, session_date.replace("-", ""))).fetchone()
        return dict(r) if r else {}

    def regime(self, session_date):
        r = self.conn.execute(
            """SELECT bias, bias_zone, vix FROM macro_context_results
               WHERE cache_date <= ? ORDER BY cache_date DESC LIMIT 1""",
            (session_date,)).fetchone()
        return (f"{r['bias']}/{r['bias_zone']}", r["vix"]) if r else (None, None)

    def earnings(self, sym, session_date):
        """Next earnings on or after the session, from Yahoo (Tickers.NextEarnings
        is not maintained: AAPL still read 2026-04-30 on 2026-10-07)."""
        if sym not in self._earn:
            info = self.tickers.get(sym) or {}
            if info.get("Type") == "IND":
                self._earn[sym] = []
                return None
            y = info.get("YahooName") or sym
            val = None
            try:
                import yfinance as yf
                cal = yf.Ticker(y).calendar
                ds = cal.get("Earnings Date") if isinstance(cal, dict) else None
                if ds:
                    val = sorted(str(d)[:10] for d in ds)
            except Exception:
                val = None
            self._earn[sym] = val or []
        fut = [d for d in self._earn[sym] if d >= session_date]
        return fut[0] if fut else None


# ── IBKR helpers ─────────────────────────────────────────────────────────────
def hist(ib, contract, end, duration, barsize, what):
    r = C.hist_many(ib, [(contract, end, duration, barsize, what)])
    return (r[0] if r else None) or []


_holiday_cache = {}


def session_open_at(ib, ts):
    """True when SPY traded in the 30 minutes before ts (a holiday has no bars)."""
    key = ts.astimezone(C.ET).date()
    if key not in _holiday_cache:
        from ib_async import Stock
        spy = Stock("SPY", "SMART", "USD")
        ib.qualifyContracts(spy)
        _holiday_cache[key] = bool(hist(ib, spy, ts, "1800 S", "5 mins", "TRADES"))
    return _holiday_cache[key]


def quotes_rebuild(ib, contracts, ts):
    """bid/ask at ts from the last 5-minute BID_ASK bar ending at or before ts
    (open = time-averaged bid, close = time-averaged ask over the 5 minutes)."""
    res = C.hist_many(ib, [(c, ts, "1800 S", "5 mins", "BID_ASK") for c in contracts])
    out = {}
    for c, bars in zip(contracts, res):
        if bars is None:
            out[c.conId] = {"bid": None, "ask": None, "iv": None, "delta": None, "failed": True}
            continue
        bars = [b for b in bars if b.date <= ts - dt.timedelta(minutes=5)] or bars
        b = bars[-1] if bars else None
        bid = b.open if b and b.open > 0 else None
        ask = b.close if b and b.close > 0 else None
        out[c.conId] = {"bid": bid, "ask": ask, "iv": None, "delta": None}
    return out


def quotes(ib, contracts, ts, live):
    return C.quotes_stream(ib, contracts) if live else quotes_rebuild(ib, contracts, ts)


def is_monthly(expiry):
    d = dt.datetime.strptime(expiry, "%Y%m%d").date()
    return int(d.weekday() == 4 and 15 <= d.day <= 21)


# ── Entry pricing ────────────────────────────────────────────────────────────
def price_entry(ib, vsym, info, sig, ts, live, conn):
    """Positions (outright, spread, or stock fallback) for one signal.
    Returns (positions, contracts, note); positions is None when the quote could
    not be taken at all (retry on a later run)."""
    from ib_async import Option, Stock

    session = sig["session_date"]
    sd = dt.date.fromisoformat(session)
    option_shadow = []
    stk = Stock(vsym, "SMART", "USD")
    if not ib.qualifyContracts(stk) or not stk.conId:
        return [], [], "stock_not_qualified"

    # Spot, and the stock's own bid/ask for the fallback.
    q = quotes(ib, [stk], ts, live).get(stk.conId, {})
    sbid, sask = q.get("bid"), q.get("ask")
    spot = (sbid + sask) / 2 if sbid and sask else None
    if q.get("failed"):
        return None, [], "stock_history_timeout"
    if not spot:
        return None, [], "no_spot"

    # Target and stop in the vehicle's price: unchanged for a name traded
    # itself; for an index proxy, the same percentage distance from spot.
    if vsym != sig["sym"]:
        k = spot / sig["px"]
        target_v, stop_v, atr_v = sig["target"] * k, sig["stop"] * k, sig["atr"] * k
    else:
        target_v, stop_v, atr_v = sig["target"], sig["stop"], sig["atr"]

    # bot_daily reads the last completed session, so an intraday run can see a
    # name that has already traded through the row's stop (CAT 10-07: row px
    # 863.44 = 10-06 close, stop 838.89, spot at the 11:13 ET entry 810.27).
    # The signal is void at entry; no vehicle is opened.
    if spot <= stop_v:
        return [], [], f"spot_through_stop {spot:.2f} <= {stop_v:.2f}"

    fee = C.fee_model(conn)
    base = {"signal_id": sig["signal_id"], "sym": vsym, "stk_conid": stk.conId,
            "spot_entry": spot, "target_v": target_v, "stop_v": stop_v, "atr_v": atr_v,
            "source": "live" if live else "hist_rebuild",
            "fee_model_version": fee["version"], "vol_bump": sig.get("vol_bump", 0)}
    contracts = [{"conid": stk.conId, "kind": "STK", "sym": vsym, "mult": 1,
                  "trading_class": None, "expiry": None, "strike": None, "right": None}]
    reasons = {}

    tclass = (info.get("TradingClass") or vsym) if vsym == sig["sym"] else vsym
    mult = int(info.get("Multiplier") or 100) if vsym == sig["sym"] else 100
    try:
        params = ib.reqSecDefOptParams(vsym, "", "STK", stk.conId)
    except Exception:
        params = []
    chain = [p for p in params if p.exchange == "SMART" and p.tradingClass == tclass]
    if not chain and params:
        chain = [p for p in params if p.exchange == "SMART"][:1]
    if not chain:
        reasons = {"outright": "no_chain", "spread": "no_chain"}
    else:
        ch = chain[0]
        tclass = ch.tradingClass
        dte_of = lambda e: (dt.datetime.strptime(e, "%Y%m%d").date() - sd).days
        exps = [e for e in sorted(ch.expirations) if C.DTE_MIN <= dte_of(e) <= C.DTE_MAX]
        if not exps:
            # Monthly-only chains (IR, AME, FTV, LIN, MET: 16 Oct then 20 Nov on
            # 10-07) miss a 15-day window on about half the sessions: take the
            # nearest monthly up to DTE_MAX_MONTHLY instead (decided 2026-10-08).
            monthly = [e for e in sorted(ch.expirations)
                       if is_monthly(e) and C.DTE_MIN <= dte_of(e) <= C.DTE_MAX_MONTHLY]
            exps = monthly[:1]
        if not exps:
            reasons = {"outright": "no_expiry", "spread": "no_expiry"}
        else:
            sig_est = (sig.get("iv30") or 0.35)
            desired_w = 10.0 if 0.03 <= 10.0 / spot <= 0.12 else 0.05 * spot
            strikes = sorted(float(s) for s in ch.strikes)
            # Candidates: long strikes around 30 delta on the estimated vol (a wide
            # band, the quote decides), and for each the short strike nearest
            # long + width. Fewer contracts = fewer ~5 s history requests.
            cands = {}
            for e in exps:
                T = C.year_frac(sd, e)
                longs = [k for k in strikes if k > spot * 0.97
                         and 0.18 <= C.bs(spot, k, T, sig_est, "C")["delta"] <= 0.42]
                for k in longs:
                    above = [x for x in strikes if x > k]
                    picks = [k] + ([min(above, key=lambda x: abs(x - (k + desired_w)))] if above else [])
                    for kk in picks:
                        cands[(e, kk)] = Option(vsym, e, kk, "C", "SMART", currency="USD",
                                                tradingClass=tclass)
            cands = list(cands.values())
            q_ok = [c for c in ib.qualifyContracts(*cands) if c and c.conId] if cands else []
            qts = quotes(ib, q_ok, ts, live)
            n_fail = sum(1 for c in q_ok if qts.get(c.conId, {}).get("failed"))
            if q_ok and n_fail > 0.2 * len(q_ok):
                return None, [], f"option_history_timeout {n_fail}/{len(q_ok)}"
            legs = {}
            for c in q_ok:
                q = qts.get(c.conId, {})
                bid, ask = q.get("bid"), q.get("ask")
                mid = (bid + ask) / 2 if bid and ask else (ask / 2 if ask else None)
                T = C.year_frac(sd, c.lastTradeDateOrContractMonth)
                iv = q.get("iv") or (C.implied_vol(mid, spot, c.strike, T) if mid else None)
                delta = q.get("delta")
                if delta is None and iv:
                    delta = C.bs(spot, c.strike, T, iv)["delta"]
                legs[(c.lastTradeDateOrContractMonth, c.strike)] = {
                    "c": c, "bid": bid, "ask": ask, "mid": mid, "iv": iv, "delta": delta, "T": T}
            out_pos, r_out = _pick_outright(legs, base, mult, target_v, fee, sd)
            spr_pos, r_spr = _pick_spread(legs, base, mult, target_v, desired_w, fee, sd)
            reasons = {"outright": r_out, "spread": r_spr}
            positions = [p for p in (out_pos, spr_pos) if p]
            if positions:
                for p in positions:
                    if not p["available"]:
                        p["fallback_reason"] = "bid_ask"
                    for key in ("long", "short"):
                        leg = p.pop("_" + key, None)
                        if leg:
                            c = leg["c"]
                            contracts.append({"conid": c.conId, "kind": "OPT", "sym": vsym,
                                              "mult": mult, "trading_class": tclass,
                                              "expiry": c.lastTradeDateOrContractMonth,
                                              "strike": c.strike, "right": "C"})
                if any(p["available"] for p in positions):
                    return positions, contracts, None
            option_shadow = positions

    # Stock fallback (proposal §3.4). Option vehicles that failed only on the
    # bid-ask test are kept beside it as available = 0, so the threshold can be
    # judged on their outcomes later.
    fb = f"outright:{reasons.get('outright')};spread:{reasons.get('spread')}"
    if not sask or not sbid:
        return option_shadow, contracts, "stock_no_quote|" + fb
    risk_ps = sask - stop_v
    if risk_ps <= 0:
        return option_shadow, contracts, "stop_above_price|" + fb
    shares = max(1, int(C.RISK_CAP // risk_ps))
    capped = 0
    if shares * sask > C.STOCK_NOTIONAL_CAP:
        shares, capped = max(1, int(C.STOCK_NOTIONAL_CAP // sask)), 1
    f_in = C.stock_fee(shares)
    pos = dict(base, vehicle="stock", fallback_reason=fb, shares=shares, mult=1,
               entry_long_bid=sbid, entry_long_ask=sask, entry_price=sask,
               entry_mid=(sbid + sask) / 2, fee_entry=f_in,
               cost_total=shares * sask + f_in, cost_R=risk_ps * shares + f_in,
               cost_total_mid=shares * (sbid + sask) / 2 + f_in,
               payoff_ratio=(target_v - sask) / risk_ps,
               bid_ask_pct=(sask - sbid) / ((sask + sbid) / 2) * 100,
               accept="ACCEPT" if (target_v - sask) / risk_ps > 2 else "REJECT",
               reject_reason="" if (target_v - sask) / risk_ps > 2 else "stop_too_wide",
               over_budget=int(risk_ps > C.RISK_CAP), notional=shares * sask,
               notional_capped=capped, available=1)
    return option_shadow + [pos], contracts, None


def _delta_band(ls):
    """Legs with delta 0.25-0.35, else the one closest to 0.30 within 0.20-0.40."""
    ls = [l for l in ls if l["delta"] is not None]
    band = [l for l in ls if C.DELTA_MIN <= l["delta"] <= C.DELTA_MAX]
    if band:
        return band
    wide = [l for l in ls if C.DELTA_MIN_WIDE <= l["delta"] <= C.DELTA_MAX_WIDE]
    return [min(wide, key=lambda l: abs(l["delta"] - C.DELTA_TARGET))] if wide else []


def _pick_outright(legs, base, mult, target_v, fee, sd):
    band = [l for l in _delta_band(legs.values()) if l["ask"]]
    if not band:
        return None, "no_strike"
    scored = []
    for l in band:
        T_fwd = max(l["T"] - 5 / 365, 1 / 365)
        fwd = C.bs(target_v, l["c"].strike, T_fwd, (l["iv"] or 0.3) + 0.02)["price"]
        pr = (fwd - l["ask"]) / l["ask"]
        ba = (l["ask"] - l["bid"]) / l["mid"] * 100 if l["bid"] else None
        reason = "no_bid" if not l["bid"] else ("bid_ask" if ba > C.MAX_BA_PCT else None)
        scored.append((reason is None, pr, -(ba or 999), l, ba, reason))
    ok = [s for s in scored if s[0]]
    available, why = 1, None
    if not ok:
        wide = [s for s in scored if s[5] == "bid_ask"]
        if not wide:
            return None, max(scored, key=lambda s: s[1])[5]
        ok, available, why = wide, 0, "bid_ask"
    _, pr, _, l, ba, _ = max(ok, key=lambda s: (s[1], s[2]))
    prem = l["ask"] * mult
    reject = "premium_over_budget" if prem > C.RISK_CAP else ("payoff_below_target" if pr <= 2 else "")
    f_in = fee["opt_leg"]
    e = l["c"].lastTradeDateOrContractMonth
    pos = dict(base, vehicle="outright", expiry=e,
               dte=(dt.datetime.strptime(e, "%Y%m%d").date() - sd).days,
               monthly=is_monthly(e), long_conid=l["c"].conId, long_strike=l["c"].strike,
               mult=mult, shares=1, delta_long=l["delta"], iv_long=l["iv"],
               entry_long_bid=l["bid"], entry_long_ask=l["ask"], entry_price=l["ask"],
               entry_mid=l["mid"], fee_entry=f_in, cost_total=prem + f_in,
               cost_R=prem + f_in, cost_total_mid=l["mid"] * mult + f_in,
               payoff_ratio=pr, bid_ask_pct=ba,
               accept="REJECT" if reject else "ACCEPT", reject_reason=reject,
               over_budget=int(prem > C.RISK_CAP), notional=prem, notional_capped=0,
               available=available, _long=l)
    return pos, why


def _pick_spread(legs, base, mult, target_v, desired_w, fee, sd):
    by_exp = {}
    for (e, k), l in legs.items():
        by_exp.setdefault(e, {})[k] = l
    scored, any_long = [], False
    for e, row in by_exp.items():
        band = _delta_band(row.values())
        if not band:
            continue
        any_long = True
        lg = min(band, key=lambda l: abs(l["delta"] - C.DELTA_TARGET))
        K = lg["c"].strike
        shorts = [k for k in row if k > K]
        if not shorts:
            scored.append((False, None, lg, None, "no_strike"))
            continue
        sk = min(shorts, key=lambda k: abs(k - (K + desired_w)))
        sh = row[sk]
        if not (lg["bid"] and lg["ask"] and sh["bid"] and sh["ask"]):
            scored.append((False, None, lg, sh, "no_bid"))
            continue
        # Long leg at its ask, short leg at its mid (decided 2026-10-08): the
        # combo natural (long ask - short bid) stacks two half-spreads on a
        # small debit and failed 7 of 11 test spreads at 20%.
        nat = lg["ask"] - sh["mid"]
        mid = lg["mid"] - sh["mid"]
        width = sk - K
        if nat <= 0 or mid <= 0 or nat >= width:
            scored.append((False, None, lg, sh, "bid_ask"))
            continue
        ba = (nat - mid) / mid * 100
        if ba > C.MAX_BA_PCT:
            scored.append((False, (width - nat) / nat, lg, sh, "bid_ask"))
            continue
        scored.append((True, (width - nat) / nat, lg, sh, None))
    if not any_long:
        return None, "no_strike"
    ok = [s for s in scored if s[0]]
    available, why = 1, None
    if not ok:
        wide = [s for s in scored if s[4] == "bid_ask" and s[1] is not None]
        if not wide:
            return None, scored[0][4] if scored else "no_strike"
        ok, available, why = wide, 0, "bid_ask"
    _, rr, lg, sh, _ = max(ok, key=lambda s: s[1])
    K, sk = lg["c"].strike, sh["c"].strike
    width = sk - K
    nat, mid = lg["ask"] - sh["mid"], lg["mid"] - sh["mid"]
    f_in = 2 * fee["opt_leg"]
    dpw = nat / width * 100
    be = K + nat
    reject = "debit_pct_width" if dpw > 33 else ("breakeven_beyond_target" if be > target_v else "")
    e = lg["c"].lastTradeDateOrContractMonth
    pos = dict(base, vehicle="spread", expiry=e,
               dte=(dt.datetime.strptime(e, "%Y%m%d").date() - sd).days,
               monthly=is_monthly(e), long_conid=lg["c"].conId, short_conid=sh["c"].conId,
               long_strike=K, short_strike=sk, width=width, mult=mult, shares=1,
               delta_long=lg["delta"], iv_long=lg["iv"],
               entry_long_bid=lg["bid"], entry_long_ask=lg["ask"],
               entry_short_bid=sh["bid"], entry_short_ask=sh["ask"],
               entry_price=nat, entry_mid=mid, fee_entry=f_in,
               cost_total=nat * mult + f_in, cost_R=nat * mult + f_in,
               cost_total_mid=mid * mult + f_in, payoff_ratio=rr, debit_pct_width=dpw,
               bid_ask_pct=(nat - mid) / mid * 100,
               accept="REJECT" if reject else "ACCEPT", reject_reason=reject,
               over_budget=int(nat * mult > C.RISK_CAP), notional=nat * mult,
               notional_capped=0, available=available, _long=lg, _short=sh)
    return pos, why


# ── Main intake loop ─────────────────────────────────────────────────────────
def _insert(conn, table, row, cols=None):
    cols = cols or [r[1] for r in conn.execute(f"PRAGMA table_info({table})")]
    keys = [k for k in row if k in cols]
    conn.execute(f"INSERT OR IGNORE INTO {table} ({','.join(keys)}) VALUES ({','.join('?' * len(keys))})",
                 [row[k] for k in keys])


def register_contracts(conn, contracts, session_date, vehicle_until):
    for c in contracts:
        if c["kind"] == "OPT":
            e = dt.datetime.strptime(c["expiry"], "%Y%m%d").date()
            until = min(e - dt.timedelta(days=5),
                        dt.date.fromisoformat(session_date) + dt.timedelta(days=45))
        else:
            until = vehicle_until
        until = C.ymd(until)
        old = conn.execute("SELECT mark_until FROM bot_fwd_contract WHERE conid = ?",
                           (c["conid"],)).fetchone()
        if old:
            if until > (old["mark_until"] or ""):
                conn.execute("UPDATE bot_fwd_contract SET mark_until = ? WHERE conid = ?",
                             (until, c["conid"]))
        else:
            conn.execute(
                """INSERT INTO bot_fwd_contract (conid, kind, sym, expiry, strike, right, mult,
                   trading_class, first_date, mark_until) VALUES (?,?,?,?,?,?,?,?,?,?)""",
                (c["conid"], c["kind"], c["sym"], c["expiry"], c["strike"], c["right"],
                 c["mult"], c["trading_class"], session_date, until))


def run(conn, ib, log=print):
    ref = Ref(conn)
    now = dt.datetime.now(C.LOCAL)
    since = (now - dt.timedelta(days=int(os.environ.get("BOT_FWD_CATCHUP", C.CATCHUP_DAYS)))).date()
    only = set(filter(None, os.environ.get("BOT_FWD_ONLY", "").split(",")))  # tests
    n_new = n_pos = n_pending = 0
    for run_ts, path in run_files(since):
        rows, src = read_rows(path)
        if not rows:
            continue
        entry_ts = C.entry_time_for(run_ts)
        if entry_ts > now:
            n_pending += 1
            continue
        # A holiday has no session: move to the next open.
        if ib is not None:
            for _ in range(4):
                if session_open_at(ib, entry_ts + dt.timedelta(minutes=5)):
                    break
                entry_ts = C.next_weekday_open(entry_ts)
            if entry_ts > now:
                continue
        session = C.ymd(entry_ts.astimezone(C.ET).date())
        live = (ib is not None and (now - run_ts) <= dt.timedelta(minutes=C.LIVE_WINDOW_MIN)
                and entry_ts == run_ts.astimezone(C.ET))

        # Underlying shadow: every tradable row of the session's first run.
        for r in rows:
            t = C.tier_of(r)
            if t == "VETO":
                continue
            _insert(conn, "bot_fwd_underlying", {
                "session_date": session, "sym": r["name"], "tier": t,
                "run_file": os.path.basename(path), "direction": r.get("direction"),
                "yahoo": r.get("yahoo") or (ref.tickers.get(r["name"]) or {}).get("YahooName") or r["name"],
                "px": C.fnum(r.get("px")), "atr": C.fnum(r.get("atr")),
                "asym": C.fnum(r.get("asym")), "trend_state": r.get("trend_state")})
        conn.commit()

        prev = conn.execute("SELECT MAX(session_date) d FROM bot_fwd_signal WHERE session_date < ?",
                            (session,)).fetchone()["d"]
        for r in rows:
            tier = C.tier_of(r)
            if tier not in C.TIERS_SIMULATED:
                continue
            sym = r["name"]
            if only and sym not in only:
                continue
            sid = f"{session}|{sym}"
            if conn.execute("SELECT 1 FROM bot_fwd_signal WHERE signal_id = ?", (sid,)).fetchone():
                continue
            if ib is None:
                n_pending += 1
                continue
            info = ref.tickers.get(sym) or {}
            vol = ref.vol(sym, session)
            regime, vix = ref.regime(session)
            pr = conn.execute("SELECT run_day FROM bot_fwd_signal WHERE signal_id = ?",
                              (f"{prev}|{sym}",)).fetchone() if prev else None
            open_same = conn.execute(
                """SELECT COUNT(DISTINCT s.signal_id) n FROM bot_fwd_signal s
                   JOIN bot_fwd_position p ON p.signal_id = s.signal_id
                   LEFT JOIN bot_fwd_result r ON r.pos_id = p.pos_id AND r.policy = 'P0'
                   WHERE s.sym = ? AND s.session_date < ?
                     AND (r.status IS NULL OR r.status = 'open' OR r.exit_date >= ?)""",
                (sym, session, session)).fetchone()["n"]
            u = ref.univ.get(sym) or {}
            sig = {
                "signal_id": sid, "session_date": session, "sym": sym,
                "vehicle_sym": C.INDEX_PROXY.get(sym, sym), "run_file": os.path.basename(src),
                "run_ts": run_ts.isoformat(), "entry_ts": entry_ts.isoformat(), "tier": tier,
                "run_day": (pr["run_day"] + 1) if pr else 1, "open_same_sym": open_same,
                "direction": r.get("direction"), "px": C.fnum(r.get("px")),
                "atr": C.fnum(r.get("atr")), "asym": C.fnum(r.get("asym")),
                "target": C.fnum(r.get("target")), "target_source": r.get("target_source"),
                "stop": C.fnum(r.get("stop")),
                "sess_target_p75": C.fnum(r.get("sess_target_p75")),
                "sess_target_p90": C.fnum(r.get("sess_target_p90")),
                "trend_state": r.get("trend_state"), "zone_state": r.get("zone_state"),
                "atr_pctile": C.fnum(r.get("atr_pctile")), "ret20": C.fnum(r.get("ret20")),
                "rs20": C.fnum(r.get("rs20")), "grp_rank": C.fnum(r.get("grp_rank")),
                "grp_rs": C.fnum(r.get("grp_rs")),
                "group_name": r.get("group") or u.get("Cluster"),
                "sector": u.get("Sector") or info.get("Sector"),
                "atr_band": r.get("atr_band") or info.get("ATR_Band"),
                "ivp": vol.get("ivp"), "iv30": vol.get("iv30"), "rv30": vol.get("rv30"),
                "vrp": vol.get("vrp"), "earnings_date": ref.earnings(sym, session),
                "regime": regime, "vix": vix, "row_json": json.dumps(r),
                "created_at": C.now_iso()}
            sig["vol_bump"] = int((sig["ivp"] is not None and sig["ivp"] <= 30)
                                  and (sig["iv30"] or 9) < (sig["rv30"] or 0))

            vsym = sig["vehicle_sym"]
            vinfo = ref.tickers.get(vsym) or {}
            if vsym == sym and ((info.get("Currency") or "USD") != "USD" or info.get("Type") == "IND"):
                sig.update(status="skipped", note="non_us_phase4")
                _insert(conn, "bot_fwd_signal", sig)
                conn.commit()
                continue
            if None in (sig["px"], sig["target"], sig["stop"], sig["atr"]):
                sig.update(status="skipped", note="missing_target_or_stop")
                _insert(conn, "bot_fwd_signal", sig)
                conn.commit()
                continue
            try:
                positions, contracts, note = price_entry(ib, vsym, vinfo or info, sig,
                                                         entry_ts, live, conn)
            except Exception as e:
                log(f"  {sym}: pricing failed ({e}) - retried next run")
                continue
            if positions is None:
                log(f"  {sym}: {note} - retried next run")
                continue
            sig.update(status="entered" if positions else "no_vehicle", note=note)
            _insert(conn, "bot_fwd_signal", sig)
            stk_until = dt.date.fromisoformat(session) + dt.timedelta(days=60)
            opt_until = dt.date.fromisoformat(session) + dt.timedelta(days=45)
            for p in positions:
                p["pos_id"] = f"{sid}|{p['vehicle']}"
                p["created_at"] = C.now_iso()
                _insert(conn, "bot_fwd_position", p)
            has_stock = any(p["vehicle"] == "stock" for p in positions)
            register_contracts(conn, contracts, session, stk_until if has_stock else opt_until)
            conn.commit()
            n_new += 1
            n_pos += len(positions)
            log(f"  {session} {sym:7s} {tier:13s} -> "
                + (", ".join(f"{p['vehicle']} {p.get('long_strike') or ''}"
                             f"{('/' + str(p['short_strike'])) if p.get('short_strike') else ''}"
                             f" {p.get('expiry') or ''} @ {p['entry_price']:.2f}"
                             for p in positions) or f"none ({note})")
                + f" [{'live' if live else 'rebuild'}]")
    log(f"Intake: {n_new} new signals, {n_pos} positions; {n_pending} pending")
