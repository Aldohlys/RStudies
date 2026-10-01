"""S-8 (TODO 82), step 1: the real BOT trades to test.

Closed BOT trades on U1804173 with a stored Return, entered after the first
portfolio snapshot (2022-10-06), directional only (net open position long or
short; delta-neutral trades have no target side). R = |realised P&L / Return|.
Writes <dir>/s8_trades.csv for bot_time_exit_trial_targets.R.

Run: python bot_time_exit_trial_trades.py <dir>
"""
import sqlite3, sys
import numpy as np, pandas as pd

S = sys.argv[1] if len(sys.argv) > 1 else "."
c = sqlite3.connect("file:C:/Users/aldoh/Documents/RApplication/data/mydb.db?mode=ro", uri=True)
tr = pd.read_sql("SELECT TradeNr, Account, TradeDate, Instrument, Symbol, Pos, Total, Right, EventType, Return "
                 "FROM Trades WHERE Strategy='BOT' AND Account='U1804173'", c)
g = tr.groupby("TradeNr")
op = tr[tr.EventType == "Open"]
side = op.assign(sc=np.sign(op.Pos) * np.where(op.Right == "P", -1, 1) * op.Pos.abs()).groupby("TradeNr").sc.sum()
df = pd.DataFrame({
    "symbol": g.Symbol.first(), "entry": g.TradeDate.min(),
    "close": tr[tr.EventType == "Close"].groupby("TradeNr").TradeDate.max(),
    "pnl": g.Total.sum(), "ret": tr.dropna(subset=["Return"]).groupby("TradeNr").Return.sum(),
    "side": np.sign(side)}).dropna(subset=["close", "ret"])
df = df[df.ret != 0]
df["R"] = (df.pnl / df.ret).abs()
tk = pd.read_sql("SELECT Name, YahooName FROM Tickers", c)
ym = dict(zip(tk.Name, tk.YahooName))
df["yahoo"] = [ym.get(s) or s for s in df.symbol]
df = df[(df.entry >= 20221006) & (df.side != 0)]
df["direction"] = np.where(df.side > 0, "long", "short")
df.reset_index().to_csv(f"{S}/s8_trades.csv", index=False)
print(f"written {len(df)} directional trades ({(df.direction == 'long').sum()} long), {df.yahoo.nunique()} underlyings")
