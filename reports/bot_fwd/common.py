"""Shared pieces of the BOT forward test: paths, schema, calendar, Black-Scholes,
fees, IBKR connection.

Design: NewTrading/Strategies/Breakouts/bot_forward_test_proposal_20261007.md
"""
import datetime as dt
import math
import os
import re
import sqlite3
from zoneinfo import ZoneInfo

# BOT_FWD_DB points the whole run at a copy (tests); default is the live DB.
DB_PATH = os.environ.get("BOT_FWD_DB", r"C:\Users\aldoh\Documents\RApplication\data\mydb.db")
CSV_DIR = r"C:\Users\aldoh\Documents\NewTrading\Reports"
DETAIL_DIR = CSV_DIR + r"\bot_daily_detail"
REPORT_DIR = os.environ.get("BOT_FWD_REPORT_DIR", r"C:\Users\aldoh\Documents\NewTrading\Reports")

ET = ZoneInfo("America/New_York")
LOCAL = ZoneInfo("Europe/Zurich")

IB_HOST, IB_PORT, IB_CLIENT_ID = "127.0.0.1", 7496, 4790

# ── Parameters (proposal §3, §4) ─────────────────────────────────────────────
DTE_MIN, DTE_MAX = 28, 42
DTE_MAX_MONTHLY = 49          # fallback: nearest monthly up to 49 DTE when none in 28-42
DELTA_MIN, DELTA_MAX, DELTA_TARGET = 0.25, 0.35, 0.30
# Coarse strike grids can skip the band (MET 10-07, 44 DTE: 100C 0.36, 105C 0.19):
# then the strike closest to 30 delta, if within this wider band.
DELTA_MIN_WIDE, DELTA_MAX_WIDE = 0.20, 0.40
MAX_BA_PCT = 20.0             # availability test, % of mid (15 until 2026-10-08)
RISK_CAP = 300.0              # USD, per trade
STOCK_NOTIONAL_CAP = 30000.0
RATE = 0.04                   # flat risk-free rate for Black-Scholes
ENTRY_DELAY_MIN = 30          # entry 30 min after the open when the run is outside RTH
                              # (at +15 the 5-min bar is 09:40-09:45: AMGN 30d call 45% wide)
LIVE_WINDOW_MIN = 20          # a run younger than this is entered on live quotes
CATCHUP_DAYS = 10             # bot_daily files re-read on every run
OPT_MARK_SESSIONS = 30
STK_MARK_SESSIONS = 40

TIERS_SIMULATED = ("BOT", "BOT-", "COUNTER-TREND")
INDEX_PROXY = {"SPX": "SPY", "NDX": "QQQ", "RUT": "IWM"}

# Tiers: same rule as NewTrading/scripts/bot_daily_to_xlsx.py::tier_of
TREND_MIN, ASYM_BOT, ASYM_BOT_MINUS, ASYM_COUNTER = 4, 1.5, 1.0, 2.0


def trend_count(s):
    m = re.match(r"^\s*(\d+)\s*/\s*6", str(s))
    return int(m.group(1)) if m else None


def fnum(x):
    try:
        v = float(x)
        return v if math.isfinite(v) else None
    except (TypeError, ValueError):
        return None


def tier_of(row):
    if str(row.get("tradable", "")).strip() not in ("1", "1.0"):
        return "VETO"
    n = trend_count(row.get("trend_state"))
    a = fnum(row.get("asym_em", row.get("asym")))
    if n is not None and n >= TREND_MIN and a is not None and a >= ASYM_BOT:
        return "BOT"
    if n is not None and n >= TREND_MIN and a is not None and a >= ASYM_BOT_MINUS:
        return "BOT-"
    if n is not None and n < TREND_MIN and a is not None and a >= ASYM_COUNTER:
        return "COUNTER-TREND"
    if a is not None and a >= ASYM_BOT_MINUS:
        return "WATCH"
    return "LOW"


# ── Database ─────────────────────────────────────────────────────────────────
SCHEMA = """
CREATE TABLE IF NOT EXISTS bot_fwd_signal (
  signal_id TEXT PRIMARY KEY, session_date TEXT, sym TEXT, vehicle_sym TEXT,
  run_file TEXT, run_ts TEXT, entry_ts TEXT, tier TEXT, run_day INTEGER,
  open_same_sym INTEGER, direction TEXT, px REAL, atr REAL, asym REAL,
  target REAL, target_source TEXT, stop REAL, sess_target_p75 REAL,
  sess_target_p90 REAL, trend_state TEXT, zone_state TEXT, atr_pctile REAL,
  ret20 REAL, rs20 REAL, grp_rank REAL, grp_rs REAL, group_name TEXT,
  sector TEXT, atr_band TEXT, ivp REAL, iv30 REAL, rv30 REAL, vrp REAL,
  earnings_date TEXT, regime TEXT, vix REAL, row_json TEXT, status TEXT,
  note TEXT, created_at TEXT);
CREATE TABLE IF NOT EXISTS bot_fwd_position (
  pos_id TEXT PRIMARY KEY, signal_id TEXT, vehicle TEXT, sym TEXT,
  fallback_reason TEXT, expiry TEXT, dte INTEGER, monthly INTEGER,
  long_conid INTEGER, short_conid INTEGER, stk_conid INTEGER,
  long_strike REAL, short_strike REAL, width REAL, shares INTEGER, mult INTEGER,
  delta_long REAL, iv_long REAL, spot_entry REAL, target_v REAL, stop_v REAL,
  atr_v REAL, entry_long_bid REAL, entry_long_ask REAL, entry_short_bid REAL,
  entry_short_ask REAL, entry_price REAL, entry_mid REAL, fee_entry REAL,
  cost_total REAL, cost_R REAL, cost_total_mid REAL, payoff_ratio REAL,
  debit_pct_width REAL, bid_ask_pct REAL, accept TEXT, reject_reason TEXT,
  over_budget INTEGER, notional REAL, notional_capped INTEGER,
  vol_bump INTEGER, available INTEGER, source TEXT, fee_model_version TEXT,
  created_at TEXT);
CREATE TABLE IF NOT EXISTS bot_fwd_contract (
  conid INTEGER PRIMARY KEY, kind TEXT, sym TEXT, expiry TEXT, strike REAL,
  right TEXT, mult INTEGER, trading_class TEXT, first_date TEXT,
  mark_until TEXT, last_mark TEXT);
CREATE TABLE IF NOT EXISTS bot_fwd_mark (
  conid INTEGER, date TEXT, kind TEXT, open REAL, high REAL, low REAL,
  close REAL, bid_close REAL, ask_close REAL, bid_high REAL, bid_low REAL,
  iv REAL, delta REAL, source TEXT, PRIMARY KEY (conid, date));
CREATE TABLE IF NOT EXISTS bot_fwd_result (
  pos_id TEXT, policy TEXT, status TEXT, exit_date TEXT, exit_reason TEXT,
  exit_price REAL, exit_mid REAL, fee_exit REAL, pnl_usd REAL, R REAL,
  hold_sessions INTEGER, mfe_R REAL, mae_R REAL, spot_mfe_atr REAL,
  spot_mae_atr REAL, delta_pnl REAL, vega_pnl REAL, theta_pnl REAL,
  other_pnl REAL, model_marks INTEGER, policy_version TEXT, computed_at TEXT,
  PRIMARY KEY (pos_id, policy));
CREATE TABLE IF NOT EXISTS bot_fwd_underlying (
  session_date TEXT, sym TEXT, tier TEXT, run_file TEXT, direction TEXT,
  yahoo TEXT, px REAL, atr REAL, asym REAL, trend_state TEXT,
  ret10_atr REAL, ret20_atr REAL, hit_up_first INTEGER, mfe_atr REAL,
  mae_atr REAL, sessions_seen INTEGER, PRIMARY KEY (session_date, sym));
"""


def db():
    conn = sqlite3.connect(DB_PATH, timeout=60)
    conn.row_factory = sqlite3.Row
    return conn


# Columns added after the tables first went live; ensure_schema() adds any
# that an existing table lacks.
ADDED_COLUMNS = {
    "bot_fwd_position": [
        ("px_v", "REAL"),            # signal price in the traded symbol's terms
        ("entry_asym", "REAL"),      # (target - entry spot) / (entry spot - stop)
        ("entry_ext_atr", "REAL"),   # (entry spot - signal price) / ATR
    ],
}


def ensure_schema(conn):
    conn.executescript(SCHEMA)
    for table, cols in ADDED_COLUMNS.items():
        have = {r[1] for r in conn.execute(f"PRAGMA table_info({table})")}
        for name, typ in cols:
            if name not in have:
                conn.execute(f"ALTER TABLE {table} ADD COLUMN {name} {typ}")
    conn.commit()


def entry_metrics(spot, px_v, target_v, stop_v, atr_v):
    """Asymmetry left at the entry price, and how far the entry price has moved
    from the signal's price (bot_daily reads the last completed session), in ATR.
    Trading plan 7.1: do not chase a bar already +1.5 ATR through the level."""
    asym = ((target_v - spot) / (spot - stop_v)
            if None not in (spot, target_v, stop_v) and spot > stop_v else None)
    ext = ((spot - px_v) / atr_v if None not in (spot, px_v, atr_v) and atr_v else None)
    return asym, ext


def now_iso():
    return dt.datetime.now(LOCAL).isoformat(timespec="seconds")


# ── US session calendar ──────────────────────────────────────────────────────
def is_weekday(d):
    return d.weekday() < 5


def rth_bounds(d):
    """09:30 and 16:00 ET on date d, as aware datetimes."""
    o = dt.datetime.combine(d, dt.time(9, 30), ET)
    return o, dt.datetime.combine(d, dt.time(16, 0), ET)


def entry_time_for(run_ts):
    """Entry timestamp for a bot_daily run: the run itself during RTH, else the
    next session's open + ENTRY_DELAY_MIN. Holidays are resolved later (no bars
    -> the next weekday is tried)."""
    t = run_ts.astimezone(ET)
    d = t.date()
    o, c = rth_bounds(d)
    if is_weekday(d) and o <= t < c:
        return t
    if not (is_weekday(d) and t < o):
        d = d + dt.timedelta(days=1)
    while not is_weekday(d):
        d += dt.timedelta(days=1)
    return rth_bounds(d)[0] + dt.timedelta(minutes=ENTRY_DELAY_MIN)


def next_weekday_open(t):
    d = t.astimezone(ET).date() + dt.timedelta(days=1)
    while not is_weekday(d):
        d += dt.timedelta(days=1)
    return rth_bounds(d)[0] + dt.timedelta(minutes=ENTRY_DELAY_MIN)


def ymd(d):
    return d.strftime("%Y-%m-%d")


# ── Black-Scholes (no dividends) ─────────────────────────────────────────────
def _ncdf(x):
    return 0.5 * (1.0 + math.erf(x / math.sqrt(2.0)))


def _npdf(x):
    return math.exp(-0.5 * x * x) / math.sqrt(2.0 * math.pi)


def bs(S, K, T, sig, right="C", r=RATE):
    """Price, delta, vega (per 1.00 vol), theta (per calendar day)."""
    if S is None or K is None or S <= 0 or K <= 0:
        return None
    if T <= 0 or sig is None or sig <= 0:
        intrinsic = max(S - K, 0.0) if right == "C" else max(K - S, 0.0)
        d = (1.0 if S > K else 0.0) if right == "C" else (-1.0 if S < K else 0.0)
        return {"price": intrinsic, "delta": d, "vega": 0.0, "theta": 0.0}
    sq = sig * math.sqrt(T)
    d1 = (math.log(S / K) + (r + 0.5 * sig * sig) * T) / sq
    d2 = d1 - sq
    disc = math.exp(-r * T)
    if right == "C":
        price = S * _ncdf(d1) - K * disc * _ncdf(d2)
        delta = _ncdf(d1)
        theta = -S * _npdf(d1) * sig / (2 * math.sqrt(T)) - r * K * disc * _ncdf(d2)
    else:
        price = K * disc * _ncdf(-d2) - S * _ncdf(-d1)
        delta = _ncdf(d1) - 1.0
        theta = -S * _npdf(d1) * sig / (2 * math.sqrt(T)) + r * K * disc * _ncdf(-d2)
    return {"price": price, "delta": delta, "vega": S * _npdf(d1) * math.sqrt(T),
            "theta": theta / 365.0}


def implied_vol(price, S, K, T, right="C", r=RATE):
    """Bisection; None when the price is outside the no-arbitrage band."""
    if price is None or S is None or price <= 0 or T <= 0:
        return None
    lo, hi = 0.01, 5.0
    plo, phi = bs(S, K, T, lo, right, r)["price"], bs(S, K, T, hi, right, r)["price"]
    if not (plo <= price <= phi):
        return None
    for _ in range(80):
        mid = 0.5 * (lo + hi)
        if bs(S, K, T, mid, right, r)["price"] < price:
            lo = mid
        else:
            hi = mid
    return 0.5 * (lo + hi)


def year_frac(d_from, expiry):
    """Calendar-day year fraction to the 16:00 ET expiry close."""
    e = dt.datetime.strptime(expiry, "%Y%m%d").date()
    return max((e - d_from).days, 0) / 365.0


# ── Fees (proposal §3.7) ─────────────────────────────────────────────────────
_FEE = None


def fee_model(conn):
    """Per-leg option fee for a 1-lot order, calibrated from the account's own
    US option fills since 2025-01-01; stock: max(1.00, 0.0075 x shares)."""
    global _FEE
    if _FEE is None:
        row = conn.execute(
            """SELECT AVG(Commission) a, COUNT(*) n FROM Trades
               WHERE Currency = 'USD' AND ABS(Pos) = 1 AND Commission > 0
                 AND (Instrument LIKE '% C' OR Instrument LIKE '% P')
                 AND TradeDate >= 20250101""").fetchone()
        opt = round(row["a"], 2) if row and row["n"] and row["n"] >= 30 else 0.91
        _FEE = {"opt_leg": opt, "version": f"opt{opt:.2f}_stk0.0075min1"}
    return _FEE


def stock_fee(shares):
    return max(1.0, 0.0075 * abs(shares))


# ── IBKR ─────────────────────────────────────────────────────────────────────
def ib_connect():
    """Connected IB instance, or None. A contract lookup proves TWS answers
    (a connected TWS can still time out every request - TODO, 2026-09-25)."""
    try:
        from ib_async import IB, Stock
        ib = IB()
        ib.RequestTimeout = 60
        ib.connect(IB_HOST, IB_PORT, clientId=IB_CLIENT_ID, timeout=10, readonly=True)
        if not ib.qualifyContracts(Stock("SPY", "SMART", "USD")):
            ib.disconnect()
            return None
        return ib
    except Exception as e:
        print(f"IBKR not available: {e}")
        return None


def hist_many(ib, reqs, timeout=90, chunk=5):
    """Historical bars for many requests. reqs: list of (contract, end, duration,
    barsize, what). Returns, in order, a bar list, or None when the request
    failed or timed out (not the same as no bars).

    Measured 2026-10-07/08: the first request on an option contract takes 6-17 s
    at IBKR, repeats 0.1 s; with 20-40 in flight most first requests ran past a
    60 s timeout and came back empty. Hence few in flight and a long timeout."""
    import asyncio
    from ib_async import util

    async def one(c, end, dur, size, what):
        try:
            return await asyncio.wait_for(
                ib.reqHistoricalDataAsync(c, end, dur, size, what, True, 1), timeout) or []
        except Exception:
            return None

    out = []
    for i in range(0, len(reqs), chunk):
        part = reqs[i:i + chunk]
        res = util.run(asyncio.gather(*(one(*r) for r in part)))
        out.extend(res if isinstance(res, list) else [res])
    return out


def quotes_stream(ib, contracts, wait=3.0, chunk=90):
    """bid, ask, iv, delta per conid from streaming quotes (a snapshot request
    waits ~11 s for completion; streaming lines fill in 1-3 s). Market data
    lines are limited (100), hence the chunks."""
    out = {}
    ib.reqMarketDataType(1)
    for i in range(0, len(contracts), chunk):
        part = contracts[i:i + chunk]
        tks = [ib.reqMktData(c, "", False, False) for c in part]
        ib.sleep(wait)
        for t in tks:
            g = t.modelGreeks
            ok = lambda v: v is not None and not math.isnan(v) and v > 0
            out[t.contract.conId] = {
                "bid": t.bid if ok(t.bid) else None, "ask": t.ask if ok(t.ask) else None,
                "iv": g.impliedVol if g and g.impliedVol and not math.isnan(g.impliedVol) else None,
                "delta": g.delta if g and g.delta is not None and not math.isnan(g.delta) else None}
        for c in part:
            ib.cancelMktData(c)
    return out


def quiet_ib_errors(ib):
    """Unknown-strike lookups (Error 200) and 'no data' replies are expected
    while probing a chain; keep them out of the log."""
    import logging
    logging.getLogger("ib_async.wrapper").setLevel(logging.CRITICAL)
    logging.getLogger("ib_async.ib").setLevel(logging.CRITICAL)
    logging.getLogger("ib_async.client").setLevel(logging.ERROR)
    logging.getLogger("yfinance").setLevel(logging.CRITICAL)
