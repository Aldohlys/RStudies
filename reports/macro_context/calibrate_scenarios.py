"""calibrate_scenarios.py — calibration pass for the macro_context scenario layer (archetypes.R).

Reads the CURRENT fingerprints from archetypes.R, rebuilds the daily 1-month match score since 2004
with the same formula as the live code, and measures it against labelled historical episodes:
  - score distribution inside vs outside each scenario's episodes
  - hit / false-positive rate for in-place thresholds 0.20..0.60, Youden J, global and per scenario
  - confusion: which scenario ranks first on each scenario's episode days
  - weight checks: sign of each fingerprint asset during the episodes, candidate assets missing from
    the fingerprint, and a constrained re-weighting (signs fixed, weights in {0, .5, 1, 1.5, 2})
    validated leave-one-episode-out.
Nothing is written back to archetypes.R.

Run:  source /c/Users/aldoh/miniconda3/etc/profile.d/conda.sh && conda activate
      PYTHONIOENCODING=utf-8 python calibrate_scenarios.py
Outputs: NewTrading/Reports/scenario_calibration_YYYYMMDD.md and scenario_scores_history_YYYYMMDD.csv
(a re-run overwrites the report; the "Recommendations" section of the 2026-10-08 report was written by hand)
"""
import re, sys, time, datetime as dt
from pathlib import Path
import numpy as np
import pandas as pd
import yfinance as yf

HERE = Path(__file__).resolve().parent
ARCH = HERE / "archetypes.R"
OUT_DIR = Path(r"C:\Users\aldoh\Documents\NewTrading\Reports")
STAMP = dt.date.today().strftime("%Y%m%d")
EVAL_START = "2004-01-01"          # most FX and ratio legs exist from here on
YIELD_SYMBOLS = {"^TNX", "^IRX", "^TNX-^IRX"}
SKIP_ASSETS = {"ABS_BREADTH"}      # level from daily runs since 2026-03 only: no history to calibrate on
THRESHOLDS = np.round(np.arange(0.10, 0.601, 0.05), 2)
FP_CAP = 0.10                      # second criterion: best hit rate with false positives <= 10% of other days
WEIGHT_GRID = [0, 0.5, 1, 1.5, 2]

# Labelled episodes (judgment, from the `analogs` text of each scenario; inclusive date ranges)
EPISODES = {
    "dash_for_cash":        [("2008-09-15", "2008-11-20"), ("2020-02-24", "2020-03-23")],
    "dollar_wrecking_ball": [("2014-07-01", "2015-03-13"), ("2022-03-01", "2022-09-28")],
    "bond_rout":            [("2013-05-22", "2013-09-05"), ("2023-08-01", "2023-10-31")],
    "stagflation":          [("2008-03-01", "2008-07-11"), ("2022-02-24", "2022-06-15")],
    "reflation":            [("2016-11-09", "2017-03-31"), ("2020-11-09", "2021-03-31")],
    "goldilocks":           [("2017-01-01", "2017-12-31"), ("2019-07-01", "2019-12-31"), ("2023-11-01", "2024-06-30")],
    "growth_scare":         [("2011-07-22", "2011-10-03"), ("2015-08-17", "2016-02-11"), ("2019-05-06", "2019-08-30"),
                             ("2024-07-17", "2024-08-07")],
    "em_china_shock":       [("2015-08-11", "2015-08-31"), ("2018-04-16", "2018-09-07")],
    "debasement":           [("2020-06-01", "2020-08-07"), ("2025-01-13", "2025-06-30")],
    "narrow_mania":         [("2023-03-13", "2023-12-31"), ("2024-01-01", "2024-07-10")],
    "leadership_unwind":    [("2021-02-12", "2021-03-31"), ("2022-01-03", "2022-06-16"), ("2024-07-11", "2024-07-24")],
    "policy_pivot":         [("2019-01-04", "2019-04-30"), ("2019-06-03", "2019-07-31"), ("2023-11-01", "2023-12-31")],
}


# ── Parse archetypes.R ──────────────────────────────────────────────────────
def parse_archetypes(retries=6):
    for attempt in range(retries):
        try:
            txt = ARCH.read_text(encoding="utf-8")
            fa = txt[txt.index("FP_ASSETS <- list("):]
            fa = fa[:fa.index("\n)\n")]
            assets = {}
            for m in re.finditer(r'^\s*(\w+)\s*=\s*list\((c\([^)]*\)|"[^"]*"),\s*(-?\d+),\s*"([^"]*)"\)', fa, re.M):
                syms = re.findall(r'"([^"]+)"', m.group(2))
                assets[m.group(1)] = dict(symbols=syms, sign=int(m.group(3)), label=m.group(4))
            arch = txt[txt.index("ARCHETYPES <- list("):]
            scen = {}
            for m in re.finditer(r'id = "(\w+)", name = "([^"]+)",\s*(?:#[^\n]*\n\s*)*fp = c\(([^)]*)\)', arch):
                fp = {k: float(v) for k, v in re.findall(r'(\w+)\s*=\s*(-?[\d.]+)', m.group(3))}
                scen[m.group(1)] = dict(name=m.group(2), fp=fp)
            act = float(re.search(r'SCEN_ACTIVE <- ([\d.]+)', txt).group(1))
            if not assets or not scen:
                raise ValueError("empty parse")
            n_ids = len(re.findall(r'^\s*id = "', arch, re.M))
            if len(scen) != n_ids:   # a fingerprint the regex could not read must not drop silently
                sys.exit(f"parsed {len(scen)} fingerprints but archetypes.R has {n_ids} scenarios")
            return assets, scen, act
        except Exception as e:  # file mid-edit
            print(f"parse failed ({e}), retry in 20s", flush=True)
            time.sleep(20)
    sys.exit("could not parse archetypes.R")


# ── Data ────────────────────────────────────────────────────────────────────
def leg_symbols(sym):
    if "-^" in sym:
        return sym.split("-", 1)
    if "/" in sym:
        return sym.split("/")
    return [sym]


def fetch(symbols):
    d = yf.download(sorted(symbols), start="1999-01-01", progress=False, auto_adjust=True, threads=True)["Close"]
    d.index = pd.to_datetime(d.index).tz_localize(None)
    return d


def series_for(sym, px):
    """Close series on its own calendar, as fp_series/get_close do (ratios on common dates; FX ratio as-of)."""
    if "-^" in sym:
        a, b = sym.split("-", 1)
        m = pd.concat([px[a].dropna(), px[b].dropna()], axis=1, join="inner")
        return (m.iloc[:, 0] - m.iloc[:, 1])
    if "/" in sym:
        a, b = sym.split("/")
        fa, fb = a.endswith("=X"), b.endswith("=X")
        if fa != fb:
            base, fx = (px[b], px[a]) if fa else (px[a], px[b])
            base, fx = base.dropna(), fx.dropna().reindex(base.dropna().index, method="ffill")
            return (fx / base) if fa else (base / fx)
        m = pd.concat([px[a].dropna(), px[b].dropna()], axis=1, join="inner")
        return m.iloc[:, 0] / m.iloc[:, 1]
    return px[sym].dropna()


def move_z(s, n, kind):
    """z of the n-row change: change / (sd of the last 120 daily changes * sqrt(n)); NA until n + 30 rows."""
    x = s.astype(float)
    dx = x.diff() if kind == "yield" else np.log(x).diff()
    mv = (x - x.shift(n)) if kind == "yield" else np.log(x / x.shift(n))
    sd = dx.rolling(120).std()
    z = mv / (sd * np.sqrt(n))
    z[np.arange(len(z)) < n + 30] = np.nan
    return z.replace([np.inf, -np.inf], np.nan)


def asset_z(assets, px, cal, n=21):
    out = {}
    for k, a in assets.items():
        if k in SKIP_ASSETS:
            continue
        zs = []
        for s in a["symbols"]:
            try:
                ser = series_for(s, px)
            except KeyError:
                continue
            if ser.empty:
                continue
            z = move_z(ser, n, "yield" if s in YIELD_SYMBOLS else "price")
            zs.append(z.reindex(cal, method="ffill", limit=5))
        if zs:
            out[k] = a["sign"] * pd.concat(zs, axis=1).mean(axis=1, skipna=True)
    return pd.DataFrame(out, index=cal)


def clip(z):
    return np.clip(z / 1.5, -1, 1)


def scores(Z, fp):
    """score = sum(w * clip(z)) / sum|w| over assets with data, per day (live formula)."""
    keys = [k for k in fp if k in Z.columns]
    w = pd.Series({k: fp[k] for k in keys})
    C = clip(Z[keys])
    avail = C.notna()
    num = (C.fillna(0) * w).sum(axis=1)
    den = (avail * w.abs()).sum(axis=1)
    return (num / den).where(den > 0)


# ── Metrics ─────────────────────────────────────────────────────────────────
def episode_mask(cal, ranges):
    m = pd.Series(False, index=cal)
    for a, b in ranges:
        m |= (cal >= a) & (cal <= b)
    return m


def auc(pos, neg):
    pos, neg = pos.dropna().values, neg.dropna().values
    if len(pos) == 0 or len(neg) == 0:
        return np.nan
    allv = np.concatenate([pos, neg])
    ranks = pd.Series(allv).rank().values
    return (ranks[:len(pos)].sum() - len(pos) * (len(pos) + 1) / 2) / (len(pos) * len(neg))


def fit_weights(Z, fp, pos_mask, neg_mask, passes=3):
    """Coordinate ascent on AUC, signs fixed, |w| in WEIGHT_GRID, at least two non-zero weights."""
    keys = [k for k in fp if k in Z.columns]
    w = {k: abs(fp[k]) for k in keys}
    sign = {k: np.sign(fp[k]) for k in keys}
    def obj(wd):
        f = {k: sign[k] * v for k, v in wd.items() if v > 0}
        if len(f) < 2:
            return -1
        s = scores(Z, f)
        return auc(s[pos_mask], s[neg_mask])
    best = obj(w)
    for _ in range(passes):
        changed = False
        for k in keys:
            for g in WEIGHT_GRID:
                if g == w[k]:
                    continue
                trial = dict(w); trial[k] = g
                v = obj(trial)
                if v > best + 1e-4:
                    best, w, changed = v, trial, True
        if not changed:
            break
    return {k: sign[k] * v for k, v in w.items()}, best


def fmt_w(fp):
    return ", ".join(f"{k} {v:+g}" for k, v in sorted(fp.items(), key=lambda kv: -abs(kv[1])))


def main():
    assets, scen, scen_active = parse_archetypes()
    print(f"{len(assets)} assets, {len(scen)} scenarios, SCEN_ACTIVE {scen_active}")
    syms = sorted({leg for a in assets.values() for s in a["symbols"] for leg in leg_symbols(s)} - {"S5FI"} | {"^GSPC"})
    px = fetch(syms)
    cal = px["^GSPC"].dropna().index
    cal = cal[cal >= EVAL_START]
    Z = asset_z(assets, px, cal)
    cover = {k: (Z[k].first_valid_index().date().isoformat() if Z[k].notna().any() else "none") for k in Z.columns}
    missing = [s for s in syms if s not in px.columns or px[s].dropna().empty]

    S = pd.DataFrame({sid: scores(Z, sc["fp"]) for sid, sc in scen.items()}, index=cal)
    S.round(4).to_csv(OUT_DIR / f"scenario_scores_history_{STAMP}.csv", index_label="date")

    masks = {sid: episode_mask(cal, EPISODES.get(sid, [])) for sid in scen}
    any_ep = pd.concat(masks.values(), axis=1).any(axis=1)

    # Separation and thresholds
    sep_rows, thr_rows, J = [], [], {}
    for sid in scen:
        pos, neg = S[sid][masks[sid]], S[sid][~masks[sid]]
        if pos.dropna().empty:
            continue
        sep_rows.append((sid, len(pos.dropna()), pos.quantile(.25), pos.median(), pos.quantile(.75),
                         neg.quantile(.25), neg.median(), neg.quantile(.75), auc(pos, neg)))
        J[sid] = {}
        for t in THRESHOLDS:
            hit = (pos.dropna() >= t).mean(); fpr = (neg.dropna() >= t).mean()
            J[sid][t] = (hit, fpr, hit - fpr)
    Jdf = pd.DataFrame({sid: {t: v[2] for t, v in d.items()} for sid, d in J.items()})
    global_t = Jdf.mean(axis=1).idxmax()
    per_t = Jdf.idxmax()
    # Capped criterion: lowest threshold whose false-positive rate is <= FP_CAP (= best hit under the cap)
    cap_t = {sid: next((t for t in THRESHOLDS if d[t][1] <= FP_CAP), THRESHOLDS[-1]) for sid, d in J.items()}
    cap_global = next((t for t in THRESHOLDS if np.mean([J[s][t][1] for s in J]) <= FP_CAP), THRESHOLDS[-1])

    # Confusion: top-ranked scenario on each scenario's episode days
    top = S.idxmax(axis=1)
    conf = {}
    for sid in scen:
        t = top[masks[sid]].dropna()
        if len(t):
            conf[sid] = t.value_counts(normalize=True).head(3)

    # Weight checks: mean clipped z per fingerprint asset over each episode
    sign_rows, cand_rows = [], []
    C = clip(Z)
    for sid, sc in scen.items():
        eps = EPISODES.get(sid, [])
        if not eps:
            continue
        for k, w in sc["fp"].items():
            if k not in C.columns:
                continue
            per_ep = [C[k][episode_mask(cal, [e])].mean() for e in eps]
            agree = sum(1 for v in per_ep if pd.notna(v) and np.sign(v) == np.sign(w) and abs(v) >= 0.1)
            flag = ""
            if all(pd.isna(v) for v in per_ep):
                flag = "no data"
            elif agree == 0:
                flag = "wrong sign or ~0 in every episode"
            elif agree < len([v for v in per_ep if pd.notna(v)]):
                flag = "mixed"
            sign_rows.append((sid, k, w, " / ".join("n/a" if pd.isna(v) else f"{v:+.2f}" for v in per_ep), flag))
        for k in C.columns:
            if k in sc["fp"]:
                continue
            per_ep = [C[k][episode_mask(cal, [e])].mean() for e in eps]
            vals = [v for v in per_ep if pd.notna(v)]
            if len(vals) == len(eps) and len(vals) >= 2 and all(abs(v) >= 0.4 for v in vals) and len({np.sign(v) for v in vals}) == 1:
                cand_rows.append((sid, k, " / ".join(f"{v:+.2f}" for v in vals)))

    # Constrained re-weighting with leave-one-episode-out validation
    rw_rows = []
    for sid, sc in scen.items():
        eps = EPISODES.get(sid, [])
        if len(eps) < 2:
            continue
        neg = ~masks[sid]
        w_all, auc_all = fit_weights(Z, sc["fp"], masks[sid], neg)
        s0 = S[sid]
        auc0 = auc(s0[masks[sid]], s0[neg])
        loo_old, loo_new = [], []
        for i, e in enumerate(eps):
            train = episode_mask(cal, [x for j, x in enumerate(eps) if j != i])
            test = episode_mask(cal, [e])
            w_tr, _ = fit_weights(Z, sc["fp"], train, neg & ~test)
            s_new = scores(Z, w_tr)
            loo_old.append(auc(s0[test], s0[neg]))
            loo_new.append(auc(s_new[test], s_new[neg]))
        rw_rows.append((sid, auc0, auc_all, np.nanmean(loo_old), np.nanmean(loo_new), fmt_w(sc["fp"]), fmt_w(w_all)))
        print(f"reweight {sid}: AUC {auc0:.3f} -> fit {auc_all:.3f}, LOEO {np.nanmean(loo_old):.3f} -> {np.nanmean(loo_new):.3f}", flush=True)

    write_report(assets, scen, scen_active, cover, missing, sep_rows, J, Jdf, global_t, per_t, conf,
                 sign_rows, cand_rows, rw_rows, S, cal, cap_t, cap_global)


def write_report(assets, scen, scen_active, cover, missing, sep_rows, J, Jdf, global_t, per_t, conf,
                 sign_rows, cand_rows, rw_rows, S, cal, cap_t, cap_global):
    name = {sid: sc["name"] for sid, sc in scen.items()}
    L = []
    L.append(f"# Scenario calibration — {dt.date.today().isoformat()}\n")
    L.append("Script: `RStudies/reports/macro_context/calibrate_scenarios.py`. Daily scores: "
             f"`Reports/scenario_scores_history_{STAMP}.csv`. Fingerprints read from `archetypes.R` at run time; "
             f"in-place threshold in the code: {scen_active:.2f}.\n")
    L.append("## Method\n")
    L.append("- 1-month match score rebuilt for every S&P session from 2004-01-01 with the live formula: "
             "z = 21-day change / (sd of the last 120 daily changes × √21); log returns for prices and ratios, "
             "percentage-point changes for ^TNX, ^IRX and the 10y−3m curve; clip(z / 1.5) to [−1, 1]; "
             "score = Σ w × clip / Σ |w| over the assets with data that day.")
    L.append("- Adjusted closes for all symbols (the live code adjusts only HYG/LQD/IEF/TIP/TLT; the difference is dividends on ETFs, small at 21 days).")
    L.append("- ABS_BREADTH (stocks above their 50-day average) is excluded: it is stored daily only since 2026-03-17, so it has no history to calibrate on. "
             "The scores here are therefore the fingerprints without that asset.")
    L.append("- Each scenario's episodes are judged against every other day (including other scenarios' episodes). "
             "Hit rate = share of episode days with score ≥ threshold; false-positive rate = share of other days ≥ threshold; "
             "Youden J = hit − false positive. AUC = probability that a random episode day scores above a random other day.\n")
    L.append("## Data coverage\n")
    L.append("| Asset | Scored from |")
    L.append("|---|---|")
    for k, v in cover.items():
        L.append(f"| {assets[k]['label']} ({k}) | {v} |")
    if missing:
        L.append(f"\nNo Yahoo data: {', '.join(missing)}.")
    L.append("\nAssets that start after 2004 are simply absent before their start date, as in the live code (score on available assets).\n")

    L.append("## Labelled episodes\n")
    L.append("Ranges chosen from the past episodes each scenario cites; judgment, kept few. Pre-2004 analogs (1994, 1998, 1999-2000) are not scored: most legs have no data.\n")
    L.append("| Scenario | Episodes |")
    L.append("|---|---|")
    for sid, eps in EPISODES.items():
        if sid in scen:
            L.append(f"| {name[sid]} | " + "; ".join(f"{a} → {b}" for a, b in eps) + " |")

    L.append("\n## Separation: score inside vs outside the episodes\n")
    L.append("| Scenario | Episode days | Episodes p25 / median / p75 | Other days p25 / median / p75 | AUC |")
    L.append("|---|---|---|---|---|")
    for r in sep_rows:
        L.append(f"| {name[r[0]]} | {r[1]} | {r[2]:+.0%} / {r[3]:+.0%} / {r[4]:+.0%} | {r[5]:+.0%} / {r[6]:+.0%} / {r[7]:+.0%} | {r[8]:.2f} |")

    L.append("\n## Threshold\n")
    L.append("Youden J (hit − false positive) by threshold; best per scenario marked *.\n")
    L.append("| Scenario | " + " | ".join(f"{t:.2f}" for t in THRESHOLDS) + " |")
    L.append("|---|" + "---|" * len(THRESHOLDS))
    for sid in Jdf.columns:
        cells = []
        for t in THRESHOLDS:
            v = Jdf.loc[t, sid]
            cells.append(f"{v:+.2f}{'*' if t == per_t[sid] else ''}")
        L.append(f"| {name[sid]} | " + " | ".join(cells) + " |")
    L.append("| Mean | " + " | ".join(f"{Jdf.loc[t].mean():+.2f}{'*' if t == global_t else ''}" for t in THRESHOLDS) + " |")
    L.append("\nHit and false-positive rates at the current and the best global threshold:\n")
    L.append(f"| Scenario | Hit @ {scen_active:.2f} | False pos. @ {scen_active:.2f} | Hit @ {global_t:.2f} | False pos. @ {global_t:.2f} | Best own threshold |")
    L.append("|---|---|---|---|---|---|")
    for sid in Jdf.columns:
        a = J[sid].get(round(scen_active, 2), (np.nan, np.nan, np.nan)); b = J[sid][global_t]
        L.append(f"| {name[sid]} | {a[0]:.0%} | {a[1]:.0%} | {b[0]:.0%} | {b[1]:.0%} | {per_t[sid]:.2f} |")

    L.append("\n## Confusion: which scenario ranks first on each scenario's episode days\n")
    L.append("| Episodes of | Top-ranked scenario (share of episode days) |")
    L.append("|---|---|")
    for sid, vc in conf.items():
        L.append(f"| {name[sid]} | " + "; ".join(f"{name.get(k, k)} {v:.0%}" for k, v in vc.items()) + " |")

    L.append("\n## Weight checks\n")
    L.append("Mean clipped move of each fingerprint asset during each episode (same order as the episode table). "
             "Flag when the asset never moves the expected way by at least 0.1, or does so only in some episodes.\n")
    L.append("| Scenario | Asset | Weight | Mean per episode | Flag |")
    L.append("|---|---|---|---|---|")
    for r in sign_rows:
        if r[4]:
            L.append(f"| {name[r[0]]} | {assets[r[1]]['label']} | {r[2]:+g} | {r[3]} | {r[4]} |")
    L.append("\nOnly flagged rows are shown; all other fingerprint assets moved the expected way in every episode.\n")
    if cand_rows:
        L.append("Assets not in the fingerprint that moved the same way by at least 0.4 in every episode (candidates to add):\n")
        L.append("| Scenario | Asset | Mean per episode |")
        L.append("|---|---|---|")
        for r in cand_rows:
            L.append(f"| {name[r[0]]} | {assets[r[1]]['label']} | {r[2]} |")

    L.append("\n## Constrained re-weighting\n")
    L.append("Signs fixed, weights in {0, 0.5, 1, 1.5, 2}, coordinate search maximising AUC. "
             "LOEO = leave one episode out: fit on the other episodes, score the held-out one; the mean held-out AUC is the honest measure. "
             "Scenarios with one episode are skipped.\n")
    L.append("| Scenario | AUC now | AUC fitted (in-sample) | LOEO AUC now | LOEO AUC fitted | Current weights | Fitted weights |")
    L.append("|---|---|---|---|---|---|---|")
    for r in rw_rows:
        L.append(f"| {name[r[0]]} | {r[1]:.2f} | {r[2]:.2f} | {r[3]:.2f} | {r[4]:.2f} | {r[5]} | {r[6]} |")

    L.append("\n## Current reading (last session in the data)\n")
    last = S.iloc[-1].sort_values(ascending=False)
    L.append(f"Session {cal[-1].date().isoformat()}, without ABS_BREADTH:\n")
    L.append("| Scenario | Score |")
    L.append("|---|---|")
    for sid, v in last.items():
        L.append(f"| {name[sid]} | {v:+.0%} |")

    L.append("\n## Limitations\n")
    L.append("- Episode ranges are judgment and few (two to four per scenario): every number above has wide error bars.")
    L.append("- Scenarios share assets and co-occur (2023: narrow leadership and bond rout), so another scenario's episode days count as false positives here even when both were genuinely in play.")
    L.append("- The in-sample re-weighting overfits by construction; only the leave-one-episode-out column is evidence.")
    L.append("- ABS_BREADTH is excluded; the live scores include it, so live levels differ from these.")
    L.append("- Pre-2004 analogs are not tested; Bund/gilt ETF proxies and MOVE start late, so earlier episodes are scored on fewer assets.")
    text = "\n".join(L) + "\n"
    text = re.sub(r'(?<!\\)\$', r'\\$', text)
    out = OUT_DIR / f"scenario_calibration_{STAMP}.md"
    out.write_text(text, encoding="utf-8")
    print("written", out)
    print(f"Youden global {global_t:.2f}; capped global {cap_global:.2f}; capped per scenario:", {k: float(v) for k, v in cap_t.items()})


if __name__ == "__main__":
    main()
