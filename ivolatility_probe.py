"""
IVolatility Backtest API Plus — trial probe.

Goal: confirm BEFORE paying that the 7-day trial actually returns
  - 25-delta call/put IV for AAPL daily 2021-01-01 .. today
  - clean ATM IV30 daily series same span
  - earnings dates 2021..today
  - stock OHLC same span

Run:
  set IVOL_API_KEY=<paste-from-dashboard>
  python ivolatility_probe.py

Outputs go to ./ivol_probe_out/  (parquet samples + a summary.txt).

API facts used (verified against IVolatility OpenAPI spec):
  base url:  https://restapi.ivolatility.com
  auth:      query param ?apiKey=...
  date fmt:  YYYY-MM-DD
  IVS:       parameterised by MONEYNESS, but each row carries `delta` —
             so we filter rows where |delta - 0.25| is minimal per (date, period).
  IVX:       returns 30d_iv_call / 30d_iv_put / 30d_iv_mean directly.
  large response: returned as gzipped CSV via `urlForDetails`, not inline JSON.
"""

from __future__ import annotations
import os, sys, io, json, gzip, time, argparse
from pathlib import Path
from urllib.parse import urlencode

import requests
import pandas as pd

BASE = "https://restapi.ivolatility.com"

ap = argparse.ArgumentParser()
ap.add_argument("--symbol", default="AAPL")
ap.add_argument("--from-date", default="2021-01-01")
ap.add_argument("--to-date", default=pd.Timestamp.today().strftime("%Y-%m-%d"))
args = ap.parse_args()

SYMBOL = args.symbol.upper()
FROM = args.from_date
TO = args.to_date
OUT = Path(__file__).parent / "ivol_probe_out" / SYMBOL
OUT.mkdir(parents=True, exist_ok=True)

API_KEY = os.environ.get("IVOL_API_KEY", "").strip()
if not API_KEY:
    sys.exit("ERROR: set IVOL_API_KEY env var to your trial key first")

results: list[str] = []


def call(path: str, params: dict, label: str, save_as: str | None = None) -> pd.DataFrame | None:
    """Hit endpoint, transparently follow urlForDetails, return DataFrame."""
    p = {"apiKey": API_KEY, **params}
    url = f"{BASE}{path}?{urlencode(p)}"
    print(f"\n--- {label} ---")
    print(f"GET {path}?{urlencode({k: v for k, v in params.items()})}")
    t0 = time.time()
    r = requests.get(url, timeout=60)
    dt = time.time() - t0
    print(f"  status={r.status_code}  bytes={len(r.content)}  {dt:.1f}s")

    if r.status_code != 200:
        print(f"  FAIL body: {r.text[:400]}")
        results.append(f"FAIL {label}: HTTP {r.status_code}")
        return None

    df = None
    try:
        body = r.json()
    except ValueError:
        # Already CSV/gzip
        body = None

    def find_details_url(obj):
        """Walk JSON and return any URL-ish string from a key containing 'detail' or 'url'."""
        if isinstance(obj, dict):
            for k, v in obj.items():
                if isinstance(v, str) and v.startswith("http") and (
                    "detail" in k.lower() or k.lower() in ("url", "downloadurl", "downloadlink", "filelink")
                ):
                    return v
                hit = find_details_url(v)
                if hit:
                    return hit
        elif isinstance(obj, list):
            for v in obj:
                hit = find_details_url(v)
                if hit:
                    return hit
        return None

    if body is not None:
        st = body.get("status", {}) if isinstance(body, dict) else {}
        if "queriesLeftEOD" in st:
            print(f"  queriesLeftEOD={st['queriesLeftEOD']}  queriesLeftIntraday={st.get('queriesLeftIntraday')}")

        details_url = find_details_url(body)
        data_field = body.get("data") if isinstance(body, dict) else None

        if details_url:
            # Step 2: poll the info endpoint until status COMPLETE and file ready
            sep = "&" if "?" in details_url else "?"
            info_url_keyed = f"{details_url}{sep}apiKey={API_KEY}"
            real_url = None
            poll_deadline = time.time() + 180  # 3 min cap
            attempt = 0
            while time.time() < poll_deadline:
                attempt += 1
                r2 = requests.get(info_url_keyed, timeout=60)
                if r2.status_code != 200:
                    print(f"    info attempt#{attempt} HTTP {r2.status_code}: {r2.text[:200]}")
                    break
                try:
                    info = r2.json()
                except ValueError:
                    print(f"    info attempt#{attempt} not JSON: {r2.content[:200]!r}")
                    break

                # Normalise: response is a list of {meta, data:[...]}
                rec = info[0] if isinstance(info, list) and info else info
                meta = rec.get("meta", {}) if isinstance(rec, dict) else {}
                files = rec.get("data") if isinstance(rec, dict) else None
                status_str = meta.get("status")
                fsize = (files[0].get("fileSize", 0) if isinstance(files, list) and files else 0)
                print(f"    info attempt#{attempt}  status={status_str}  fileSize={fsize}  records={meta.get('recordsCount')}")

                if status_str == "COMPLETE" and fsize and isinstance(files, list) and files:
                    real_url = files[0].get("urlForDownload")
                    if real_url:
                        break
                if status_str in ("FAILED", "ERROR"):
                    print(f"    job failed: {json.dumps(meta)[:400]}")
                    break
                time.sleep(2)

            if real_url:
                print(f"  step3 download -> {real_url}")
                sep2 = "&" if "?" in real_url else "?"
                r3 = requests.get(f"{real_url}{sep2}apiKey={API_KEY}", timeout=300)
                print(f"    download status={r3.status_code} bytes={len(r3.content)} ctype={r3.headers.get('content-type')}")
                if r3.status_code == 200 and r3.content:
                    buf = gzip.decompress(r3.content) if r3.content[:2] == b"\x1f\x8b" else r3.content
                    try:
                        df = pd.read_csv(io.BytesIO(buf))
                    except Exception:
                        try:
                            df = pd.DataFrame(json.loads(buf))
                        except Exception as e:
                            print(f"    parse failed: {e}")
                            print(f"    first 400 bytes: {buf[:400]!r}")
        elif isinstance(data_field, list) and data_field:
            df = pd.DataFrame(data_field)
        elif isinstance(data_field, dict):
            df = pd.DataFrame([data_field]) if data_field else None
        elif isinstance(body, list):
            df = pd.DataFrame(body)

        # If we still have nothing, dump full body so we can see the actual shape
        if df is None or df.empty:
            print(f"  DEBUG full body: {json.dumps(body, default=str)[:1500]}")

    if df is None or df.empty:
        print(f"  FAIL: empty response  body-keys={list(body.keys()) if isinstance(body, dict) else type(body)}")
        results.append(f"FAIL {label}: empty")
        return None

    print(f"  rows={len(df)}  cols={list(df.columns)[:12]}{' ...' if len(df.columns) > 12 else ''}")
    print(df.head(3).to_string(max_cols=10))
    if save_as:
        path_out = OUT / save_as
        df.to_parquet(path_out, index=False)
        print(f"  saved -> {path_out}")
    return df


# --- 1. Stock OHLC (cheapest, validates auth) ----------------------------
spx = call(
    "/equities/eod/stock-prices",
    {"symbol": SYMBOL, "from": FROM, "to": TO, "region": "USA"},
    "Stock OHLC",
    save_as="stock_prices.parquet",
)
if spx is not None:
    span = pd.to_datetime(spx["date"]).agg(["min", "max"])
    print(f"  span: {span['min'].date()} .. {span['max'].date()}  ({len(spx)} rows)")
    results.append(f"PASS Stock OHLC: {len(spx)} rows {span['min'].date()}..{span['max'].date()}")

# --- 2. IVX (ATM IV30) ---------------------------------------------------
ivx = call(
    "/equities/eod/ivx",
    {"symbol": SYMBOL, "from": FROM, "to": TO, "region": "USA"},
    "IVX (ATM IV by tenor)",
    save_as="ivx.parquet",
)
if ivx is not None:
    print(f"  IVX columns ({len(ivx.columns)}): {list(ivx.columns)}")
    iv30_candidates = [c for c in ivx.columns if "30" in c.lower() and ("iv" in c.lower() or "vol" in c.lower())]
    print(f"  IV30 candidate columns: {iv30_candidates}")
    results.append(f"PASS IVX: {len(ivx)} rows; IV30 candidates={iv30_candidates}")

# --- 3. IVS (volatility surface) — small probe first ---------------------
# Single recent date to confirm row schema includes `delta`
ivs_one = call(
    "/equities/eod/ivs",
    {"symbol": SYMBOL, "date": "2024-06-03", "region": "USA"},
    "IVS single-date probe",
    save_as="ivs_one_date.parquet",
)
if ivs_one is not None:
    has_delta = "delta" in ivs_one.columns
    print(f"  has 'delta' column? {has_delta}")
    if has_delta:
        # show 30d slice with deltas closest to ±0.25
        if "period" in ivs_one.columns:
            tenor30 = ivs_one[ivs_one["period"] == 30]
            if len(tenor30):
                print("  30d slice — rows nearest |delta|=0.25:")
                tenor30 = tenor30.assign(d_dist=(tenor30["delta"].abs() - 0.25).abs())
                print(tenor30.nsmallest(4, "d_dist").to_string())
    results.append(f"{'PASS' if has_delta else 'FAIL'} IVS schema: delta column present={has_delta}")

# --- 4. IVS — full 2021..today, the real test ----------------------------
# This response will likely use urlForDetails (gzipped CSV) given size.
ivs_full = call(
    "/equities/eod/ivs",
    {"symbol": SYMBOL, "from": FROM, "to": TO, "region": "USA"},
    "IVS full range",
    save_as="ivs_full.parquet",
)
if ivs_full is not None:
    span = pd.to_datetime(ivs_full["date"]).agg(["min", "max"])
    # find Call/Put column case-insensitively
    cp_col = next((c for c in ivs_full.columns if c.lower() in ("call/put", "callput", "right")), None)
    rows25 = None
    if "delta" in ivs_full.columns and "period" in ivs_full.columns and cp_col:
        # 30d tenor → pick row per (date, call/put) whose |delta| is closest to 0.25
        d = ivs_full[ivs_full["period"] == 30].copy()
        d["d_dist"] = (d["delta"].abs() - 0.25).abs()
        idx = d.groupby(["date", cp_col])["d_dist"].idxmin()
        rows25 = d.loc[idx].sort_values(["date", cp_col]).reset_index(drop=True)
        rows25.to_parquet(OUT / "ivs_25delta_30d.parquet", index=False)
        print(f"  reconstructed 25-delta 30d series: {len(rows25)} rows  saved -> ivs_25delta_30d.parquet")
        print(rows25.head(6).to_string())
        wide = (
            rows25.assign(side=rows25[cp_col].map({"C": "c25", "P": "p25"}))
                  .pivot_table(index="date", columns="side", values="IV", aggfunc="first")
                  .reset_index()
        )
        wide.to_parquet(OUT / "ivs_25delta_30d_wide.parquet", index=False)
        print(f"  wide pivot: {len(wide)} rows -> ivs_25delta_30d_wide.parquet")
    else:
        print(f"  could not extract 25-delta — missing one of: delta={'delta' in ivs_full.columns}, period={'period' in ivs_full.columns}, call/put col={cp_col}")
    results.append(
        f"PASS IVS full: {len(ivs_full)} rows {span['min'].date()}..{span['max'].date()}"
        f"{f'; 25-delta extracted: {len(rows25)} rows' if rows25 is not None else '; 25-delta extraction skipped'}"
    )

# --- 5. Earnings (probe several param shapes) ----------------------------
earn = None
for params, lab in [
    ({"symbols": SYMBOL, "from": FROM, "to": TO}, "Earnings (symbols + from/to)"),
    ({"symbol": SYMBOL, "from": FROM, "to": TO},  "Earnings (symbol + from/to)"),
    ({"symbol": SYMBOL, "region": "USA", "from": FROM, "to": TO}, "Earnings (symbol + region)"),
    ({"symbol": SYMBOL}, "Earnings (no date range)"),
]:
    earn = call("/equities/eod/earnings", params, lab,
                save_as="earnings.parquet" if earn is None else None)
    if earn is not None and not earn.empty:
        results.append(f"PASS {lab}: {len(earn)} rows")
        break
else:
    results.append("FAIL Earnings: all param shapes returned 0 rows (likely not entitled on trial)")

# --- summary -------------------------------------------------------------
summary = OUT / "summary.txt"
with summary.open("w") as f:
    f.write(f"Probe run: {pd.Timestamp.now()}\nsymbol={SYMBOL}  range={FROM}..{TO}\n\n")
    f.write("\n".join(results))
print("\n" + "=" * 60)
print("\n".join(results))
print(f"\nWrote summary -> {summary}")
print(f"Inspect outputs in {OUT}")
