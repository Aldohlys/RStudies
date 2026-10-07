"""BOT forward test — simulate every BOT / BOT- / COUNTER-TREND signal of
bot_daily with a 30-delta call and a bull call spread (stock when neither can be
traded), at IBKR ask/bid with IBKR fees, exited by the trading plan's rules.

Design: NewTrading/Strategies/Breakouts/bot_forward_test_proposal_20261007.md
Tables (mydb.db): bot_fwd_signal, bot_fwd_position, bot_fwd_contract,
bot_fwd_mark, bot_fwd_result, bot_fwd_underlying.

Usage (conda python):
  python reports/bot_fwd/main.py [all|intake|marks|policy|shadow|report]...

`all` (default) runs every step. intake and marks need TWS; without it they are
skipped and caught up on the next run (bot_daily files of the last 10 days are
re-read; marks are back-filled from IBKR history while the contracts live).
Exit code: 0 ok, 3 when TWS was unreachable (intake/marks skipped), 1 on error.
"""
import os
import sys
import traceback

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import common as C  # noqa: E402


def main(argv):
    steps = [a for a in argv if not a.startswith("-")] or ["all"]
    if "all" in steps:
        steps = ["intake", "marks", "policy", "shadow", "report"]
    conn = C.db()
    C.ensure_schema(conn)
    rc = 0
    ib = None
    if {"intake", "marks"} & set(steps):
        ib = C.ib_connect()
        if ib is None:
            print("TWS not reachable: intake and marks skipped")
            rc = 3
        else:
            C.quiet_ib_errors(ib)
    try:
        for s in steps:
            print(f"== {s}")
            if s == "intake":
                import intake
                intake.run(conn, ib)
            elif s == "marks" and ib is not None:
                import marks
                marks.run(conn, ib)
            elif s == "policy":
                import policy
                policy.run(conn)
            elif s == "shadow":
                import shadow
                shadow.run(conn)
            elif s == "report":
                import report
                report.run(conn)
    except Exception:
        traceback.print_exc()
        rc = 1
    finally:
        if ib is not None:
            ib.disconnect()
        conn.close()
    return rc


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
