"""Weekly report (proposal §5.2): Reports/bot_fwd_<Monday>.md, rewritten on each
run of the week. Headline tables use fresh signals only (run_day = 1, one trade
per name-run); repeats are shown apart for the late-entry question (§2.2).
Amounts are written in USD, never with the dollar sign (markdown math rule).
"""
import datetime as dt
import os
import statistics as st

import common as C


def _fmt(x, nd=2):
    return "n/a" if x is None else f"{x:+.{nd}f}"


def _agg(rs):
    rs = [r for r in rs if r is not None]
    if not rs:
        return "0 | n/a | n/a | n/a | n/a"
    win = sum(r > 0 for r in rs) / len(rs) * 100
    return f"{len(rs)} | {_fmt(st.mean(rs))} | {_fmt(st.median(rs))} | {win:.0f}% | {_fmt(sum(rs), 1)}"


def run(conn, log=print):
    today = dt.date.today()
    monday = today - dt.timedelta(days=today.weekday())
    q = conn.execute
    res = [dict(r) for r in q(
        """SELECT r.*, p.vehicle, p.sym AS vsym, p.cost_R, p.fallback_reason, p.accept, p.available,
                  p.entry_asym, p.entry_ext_atr,
                  s.tier, s.tier_reason, s.run_day, s.session_date, s.sym, s.sector, s.group_name
           FROM bot_fwd_result r JOIN bot_fwd_position p ON p.pos_id = r.pos_id
           JOIN bot_fwd_signal s ON s.signal_id = p.signal_id""")]
    # Tier as reported: tier/reason where a reason splits a tier (C.GROUPS).
    for r in res:
        r["tier"] = C.group_of(r["tier"], r["tier_reason"])
    wide = [r for r in res if r["policy"] == "P0" and r["available"] == 0 and r["status"] == "closed"]
    res = [r for r in res if r["available"] != 0]
    p0 = [r for r in res if r["policy"] == "P0"]
    closed = [r for r in p0 if r["status"] == "closed"]
    fresh = [r for r in closed if r["run_day"] == 1]
    L = [f"# BOT forward test — week of {monday}", "",
         f"Generated {C.now_iso()}. Design: `NewTrading/Strategies/Breakouts/bot_forward_test_proposal_20261007.md`.",
         "R = P&L / 1R; 1R = entry cost incl. fees (options), risk to stop incl. fee (stock). "
         "Fills at ask/bid unless stated. Headline tables: fresh signals only (run_day = 1).", ""]

    sig = q("""SELECT status, COUNT(*) n FROM bot_fwd_signal GROUP BY status""").fetchall()
    veh = q("""SELECT vehicle, COUNT(*) n FROM bot_fwd_position GROUP BY vehicle""").fetchall()
    L += ["## Totals", "",
          "Signals: " + ", ".join(f"{r['status']} {r['n']}" for r in sig),
          "", "Positions: " + ", ".join(f"{r['vehicle']} {r['n']}" for r in veh)
          + f" (incl. bid-ask shadows); under P0 open {sum(r['status'] == 'open' for r in p0)},"
          f" closed {len(closed)}", ""]

    # This week
    wk = C.ymd(monday)
    opened = [r for r in p0 if r["session_date"] >= wk]
    shut = [r for r in closed if (r["exit_date"] or "") >= wk]
    L += [f"## Opened this week ({len(opened)})", "",
          "| session | sym | tier | run_day | vehicle | R at last close |", "|---|---|---|---|---|---|"]
    L += [f"| {r['session_date']} | {r['sym']} | {r['tier']} | {r['run_day']} | {r['vehicle']} | {_fmt(r['R'])} |"
          for r in sorted(opened, key=lambda r: (r["session_date"], r["sym"]))]
    L += ["", f"## Closed this week under P0 ({len(shut)})", "",
          "| exit | sym | tier | vehicle | sessions | reason | R |", "|---|---|---|---|---|---|---|"]
    L += [f"| {r['exit_date']} | {r['sym']} | {r['tier']} | {r['vehicle']} | {r['hold_sessions']} | "
          f"{r['exit_reason']} | {_fmt(r['R'])} |"
          for r in sorted(shut, key=lambda r: (r["exit_date"], r["sym"]))]

    hdr = "| n | mean R | median R | win | total R |"
    sep = "|---|---|---|---|---|"
    L += ["", "## Tier x vehicle (P0, closed, fresh)", "", "| tier | vehicle " + hdr, "|---|---" + sep]
    for t in C.GROUPS:
        for v in ("outright", "spread", "stock"):
            L.append(f"| {t} | {v} | " + _agg([r["R"] for r in fresh if r["tier"] == t and r["vehicle"] == v]) + " |")

    L += ["", "## Repeats vs fresh (P0, closed)", "", "| run_day | vehicle " + hdr, "|---|---" + sep]
    for lab, f in (("1", lambda d: d == 1), ("2-3", lambda d: 2 <= d <= 3), ("4+", lambda d: d >= 4)):
        for v in ("outright", "spread", "stock"):
            L.append(f"| {lab} | {v} | " + _agg([r["R"] for r in closed if f(r["run_day"]) and r["vehicle"] == v]) + " |")

    L += ["", "## Exit policies (closed, fresh)", "",
          "P0 primary; P1 stop + last week; P2a/P2b targets 2x/70% and 3x/90%; P3 no time stop; "
          "P4 time stop at session 7; P5 hold to DTE 7 / 5; P6 P0 at mid; P7 P0 + exit when asymmetry is gone.", "",
          "| policy | vehicle " + hdr, "|---|---" + sep]
    for pol in ("P0", "P1", "P2a", "P2b", "P3", "P4", "P5", "P6", "P7"):
        for v in ("outright", "spread", "stock"):
            rs = [r["R"] for r in res if r["policy"] == pol and r["status"] == "closed"
                  and r["run_day"] == 1 and r["vehicle"] == v]
            if rs:
                L.append(f"| {pol} | {v} | " + _agg(rs) + " |")

    def bucket(x, edges, labels):
        if x is None:
            return "n/a"
        for e, lab in zip(edges, labels):
            if x < e:
                return lab
        return labels[-1]

    L += ["", "## Entry extension and asymmetry (P0, closed, fresh)", "",
          "Extension = (entry price - signal price) / ATR: bot_daily reads the last completed "
          "session, the entry is at the run or 30 min after the open. Trading plan 7.1: do not "
          "chase a bar already +1.5 ATR through the level. Entry asymmetry = (target - entry) / "
          "(entry - stop).", "", "| extension | vehicle " + hdr, "|---|---" + sep]
    ext_lab = ["< 0", "0 to 0.5", "0.5 to 1.5", ">= 1.5"]
    for lab in ext_lab:
        for v in ("outright", "spread", "stock"):
            rs = [r["R"] for r in fresh if r["vehicle"] == v
                  and bucket(r["entry_ext_atr"], [0, 0.5, 1.5, 1e9], ext_lab) == lab]
            if rs:
                L.append(f"| {lab} | {v} | " + _agg(rs) + " |")
    L += ["", "| entry asymmetry | vehicle " + hdr, "|---|---" + sep]
    asym_lab = ["< 0.5", "0.5 to 1.5", ">= 1.5"]
    for lab in asym_lab:
        for v in ("outright", "spread", "stock"):
            rs = [r["R"] for r in fresh if r["vehicle"] == v
                  and bucket(r["entry_asym"], [0.5, 1.5, 1e9], asym_lab) == lab]
            if rs:
                L.append(f"| {lab} | {v} | " + _agg(rs) + " |")

    L += ["", "## Exit reasons (P0, closed, fresh)", "", "| reason " + hdr, "|---" + sep]
    for reason in sorted({r["exit_reason"] for r in fresh}):
        L.append(f"| {reason} | " + _agg([r["R"] for r in fresh if r["exit_reason"] == reason]) + " |")

    by = {(r["pos_id"]): r for r in res if r["policy"] == "P6" and r["status"] == "closed"}
    ec = [(r["vehicle"], r["R"] - by[r["pos_id"]]["R"]) for r in fresh
          if r["pos_id"] in by and r["R"] is not None and by[r["pos_id"]]["R"] is not None]
    L += ["", "## Execution cost (P0 at ask/bid minus P6 at mid, in R)", "", "| vehicle | n | mean | median |",
          "|---|---|---|---|"]
    for v in ("outright", "spread", "stock"):
        xs = [x for vv, x in ec if vv == v]
        if xs:
            L.append(f"| {v} | {len(xs)} | {_fmt(st.mean(xs))} | {_fmt(st.median(xs))} |")

    L += ["", f"## Options that failed only the bid-ask test (> {C.MAX_BA_PCT:.0f}% of mid), P0, closed",
          "", "Simulated beside the stock fallback, not in any table above.", "",
          "| vehicle " + hdr, "|---" + sep]
    for v in ("outright", "spread"):
        L.append(f"| {v} | " + _agg([r["R"] for r in wide if r["vehicle"] == v]) + " |")

    fb = q("""SELECT fallback_reason, COUNT(*) n FROM bot_fwd_position WHERE vehicle = 'stock'
              GROUP BY fallback_reason ORDER BY n DESC""").fetchall()
    L += ["", "## Stock fallback reasons", "", "| reason | n |", "|---|---|"]
    L += [f"| {r['fallback_reason']} | {r['n']} |" for r in fb]

    L += ["", "## Sector (P0, closed, fresh, all vehicles)", "", "| sector " + hdr, "|---" + sep]
    for s in sorted({r["sector"] or "n/a" for r in fresh}):
        L.append(f"| {s} | " + _agg([r["R"] for r in fresh if (r["sector"] or "n/a") == s]) + " |")

    sh = q("""SELECT tier, COUNT(*) n, AVG(ret10_atr) r10, AVG(ret20_atr) r20,
                     AVG(hit_up_first) hit, SUM(hit_up_first IS NOT NULL) nh
              FROM bot_fwd_underlying WHERE sessions_seen >= 10 GROUP BY tier""").fetchall()
    L += ["", "## Underlying shadow (all tradable rows, >= 10 sessions seen)", "",
          "| tier | n | mean ret10 ATR | mean ret20 ATR | +1.5 ATR first |", "|---|---|---|---|---|"]
    L += [f"| {r['tier']} | {r['n']} | {_fmt(r['r10'])} | {_fmt(r['r20'])} | "
          f"{'n/a' if r['hit'] is None else f'{r['hit'] * 100:.0f}% of {r['nh']}'} |" for r in sh]

    L += ["", "Cells with fewer than 30 name-runs are descriptive only (proposal §6).", ""]
    path = os.path.join(C.REPORT_DIR, f"bot_fwd_{monday.strftime('%Y%m%d')}.md")
    with open(path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(L))
    log(f"Report: {path}")
