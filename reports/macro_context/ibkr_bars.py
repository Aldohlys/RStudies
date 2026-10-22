"""Daily bars from IBKR for the macro report's empty-close fallbacks (intermarket.R,
fill_futures_closes() and fill_stock_closes()).

Futures are chosen by delivery month (ContractDetails.contractMonth), never by
lastTradeDateOrContractMonth='YYYYMM': IBKR matches that against the last trade
date, which for WTI falls in the month before delivery.

Stocks and ETFs are read over all sessions (useRTH=False). The regular-hours bar
of a European listing ends before the closing auction (Nestle 20 Oct 2026: 83.43
against the exchange close 83.11); the all-session bar matched the exchange close
within 0.05% on 23 of 24 names. A primary exchange instead of SMART returned no
bars for the continental listings.
"""
from ib_async import IB, Future, Stock


def _connect(client_id, host, port, timeout):
    ib = IB()
    ib.RequestTimeout = timeout
    try:
        ib.connect(host, port, clientId=client_id, readonly=True, timeout=5)
    except Exception:
        return None
    return ib


def _rows(bars, **keys):
    return [dict(keys, date=str(b.date), open=b.open, high=b.high, low=b.low,
                 close=b.close, volume=b.volume) for b in bars or []]


def fut_daily_bars(reqs, host="127.0.0.1", port=7496, client_id=4791, duration="10 D", timeout=20):
    """reqs: list of dicts {root, exchange, month ('YYYYMM' delivery month)}.

    Returns a list of dicts {root, month, local, date, open, high, low, close, volume},
    or None when TWS does not answer. A contract that cannot be resolved is left out.
    """
    ib = _connect(client_id, host, port, timeout)
    if ib is None:
        return None
    out = []
    try:
        for r in reqs:
            try:
                cds = ib.reqContractDetails(Future(symbol=r["root"], exchange=r["exchange"], currency="USD"))
                cd = next((c for c in cds if c.contractMonth == r["month"]), None)
                if cd is None:
                    continue
                bars = ib.reqHistoricalData(cd.contract, "", duration, "1 day", "TRADES", True, 1,
                                            timeout=timeout)
            except Exception:
                continue
            out += _rows(bars, root=r["root"], month=r["month"], local=cd.contract.localSymbol)
    finally:
        ib.disconnect()
    return out


def stk_daily_bars(reqs, host="127.0.0.1", port=7496, client_id=4792, duration="10 D", timeout=20):
    """reqs: list of dicts {key, symbol, currency, conid and exchange (optional)}, SMART-routed.

    Returns a list of dicts {key, local, date, open, high, low, close, volume}, or None
    when TWS does not answer. A contract that cannot be resolved is left out.
    """
    ib = _connect(client_id, host, port, timeout)
    if ib is None:
        return None
    out = []
    try:
        for r in reqs:
            try:
                # A listing exchange (Tickers.Exchange other than SMART) pins the
                # listing: NUCL in USD on SMART alone resolved to another stock
                c = (Stock(conId=int(r["conid"]), exchange="SMART") if r.get("conid")
                     else Stock(r["symbol"], "SMART", r["currency"], primaryExchange=r.get("exchange") or ""))
                q = ib.qualifyContracts(c)
                if not q:
                    continue
                bars = ib.reqHistoricalData(q[0], "", duration, "1 day", "TRADES", False, 1,
                                            timeout=timeout)
            except Exception:
                continue
            out += _rows(bars, key=r["key"], local=q[0].localSymbol)
    finally:
        ib.disconnect()
    return out
