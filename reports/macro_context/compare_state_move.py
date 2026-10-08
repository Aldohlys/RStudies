"""compare_state_move.py - move score vs state score vs a mix, per macro_context scenario.

Move score  = the live score (21-session z-scores, archetypes.R).
State score = same weights applied to each asset's position in its rolling range:
              state = 2 * (level - min) / (max - min) - 1 over W sessions (W = 252 and 756), sign as in FP_ASSETS.
Mix         = a * move + (1 - a) * state252, a in {0, .25, .5, .75, 1}, a chosen leave-one-episode-out.

Same episodes and data as calibrate_scenarios.py (imported from it). Separation = AUC of episode days
vs all other days. Timing at the threshold that leaves 10% of other days in place, for each variant:
detection delay (sessions from episode start to first day in place) and lag after the episode ends
(sessions until the score drops below the threshold, capped at 60).

Output: NewTrading/Reports/scenario_state_vs_move_YYYYMMDD.md
Run: python -I compare_state_move.py
"""
import importlib.util, datetime as dt
from pathlib import Path
import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
_spec = importlib.util.spec_from_file_location("cal", HERE / "calibrate_scenarios.py")
cal_mod = importlib.util.module_from_spec(_spec); _spec.loader.exec_module(cal_mod)

ALPHAS = [0.0, 0.25, 0.5, 0.75, 1.0]
WINDOWS = [252, 756]
FP_LIMIT = 0.10
LAG_CAP = 60
OUT = cal_mod.OUT_DIR / f"scenario_state_vs_move_{dt.date.today():%Y%m%d}.md"


def asset_state(assets, px, cal, w):
    out = {}
    for k, a in assets.items():
        if k in cal_mod.SKIP_ASSETS:
            continue
        ss = []
        for s in a["symbols"]:
            try:
                ser = cal_mod.series_for(s, px).astype(float)
            except KeyError:
                continue
            if ser.empty:
                continue
            lo, hi = ser.rolling(w, min_periods=w).min(), ser.rolling(w, min_periods=w).max()
            st = 2 * (ser - lo) / (hi - lo) - 1
            ss.append(st.replace([np.inf, -np.inf], np.nan).reindex(cal, method="ffill", limit=5))
        if ss:
            out[k] = a["sign"] * pd.concat(ss, axis=1).mean(axis=1, skipna=True)
    return pd.DataFrame(out, index=cal)


def state_scores(S, fp):
    """Weighted average of positions (already in [-1, 1], no cap needed)."""
    keys = [k for k in fp if k in S.columns]
    w = pd.Series({k: fp[k] for k in keys})
    avail = S[keys].notna()
    num = (S[keys].fillna(0) * w).sum(axis=1)
    den = (avail * w.abs()).sum(axis=1)
    return (num / den).where(den > 0)


def per_episode_auc(score, ranges, cal, other):
    vals = []
    for r in ranges:
        m = cal_mod.episode_mask(cal, [r])
        vals.append(cal_mod.auc(score[m], score[other]))
    return np.nanmean(vals) if len(vals) else np.nan


def thr_at_fp(score, other):
    s = score[other].dropna()
    return float(np.nanquantile(s, 1 - FP_LIMIT)) if len(s) else np.nan


def timing(score, ranges, cal, thr):
    delays, lags = [], []
    for a, b in ranges:
        idx = cal[(cal >= a) & (cal <= b)]
        if len(idx) == 0:
            continue
        s = score.reindex(idx)
        on = np.where(s.values >= thr)[0]
        delays.append(on[0] if len(on) else np.nan)
        after = score[cal > pd.Timestamp(b)].iloc[:LAG_CAP]
        if len(on) and s.iloc[-1] >= thr:
            off = np.where(after.values < thr)[0]
            lags.append(off[0] if len(off) else LAG_CAP)
        else:
            lags.append(0)
    return (np.nanmedian(delays) if np.isfinite(delays).any() else np.nan,
            np.nanmedian(lags) if len(lags) else np.nan, len(delays) - int(np.isfinite(delays).sum()))


def main():
    assets, scen, _ = cal_mod.parse_archetypes()
    syms = sorted({leg for a in assets.values() for s in a["symbols"] for leg in cal_mod.leg_symbols(s)} - {"S5FI"} | {"^GSPC"})
    px = cal_mod.fetch(syms)
    cal = px["^GSPC"].dropna().index
    cal = cal[cal >= cal_mod.EVAL_START]
    Z = cal_mod.asset_z(assets, px, cal)
    S = {w: asset_state(assets, px, cal, w) for w in WINDOWS}

    rows, trows = [], []
    for sid, sc in scen.items():
        ranges = cal_mod.EPISODES.get(sid)
        if not ranges:
            continue
        ep = cal_mod.episode_mask(cal, ranges); other = ~ep
        mv = cal_mod.scores(Z, sc["fp"])
        st = {w: state_scores(S[w], sc["fp"]) for w in WINDOWS}
        res = {"move": mv, "state252": st[252], "state756": st[756]}
        auc_all = {k: cal_mod.auc(v[ep], v[other]) for k, v in res.items()}
        ep_auc = {k: per_episode_auc(v, ranges, cal, other) for k, v in res.items()}

        # Mix: a chosen on the other episodes, scored on the held-out one
        mix_held, a_held = [], []
        for i, r in enumerate(ranges):
            train = [x for j, x in enumerate(ranges) if j != i]
            if not train:
                continue
            tr_ep = cal_mod.episode_mask(cal, train)
            held = cal_mod.episode_mask(cal, [r])
            neg = ~(tr_ep | held)
            best = max(ALPHAS, key=lambda a: cal_mod.auc((a * mv + (1 - a) * st[252])[tr_ep], (a * mv + (1 - a) * st[252])[neg]))
            m = best * mv + (1 - best) * st[252]
            mix_held.append(cal_mod.auc(m[held], m[~(tr_ep | held)])); a_held.append(best)
        a_all = max(ALPHAS, key=lambda a: cal_mod.auc((a * mv + (1 - a) * st[252])[ep], (a * mv + (1 - a) * st[252])[other]))
        mix = a_all * mv + (1 - a_all) * st[252]
        rows.append(dict(sid=sid, name=sc["name"], n_ep=len(ranges),
                         move=ep_auc["move"], s252=ep_auc["state252"], s756=ep_auc["state756"],
                         mix=np.nanmean(mix_held) if mix_held else np.nan, a_all=a_all,
                         a_held="/".join(f"{a:g}" for a in a_held),
                         move_all=auc_all["move"], s252_all=auc_all["state252"], s756_all=auc_all["state756"],
                         today_move=mv.dropna().iloc[-1], today_state=st[252].dropna().iloc[-1], today_mix=mix.dropna().iloc[-1]))
        for k, v in [("move", mv), ("state252", st[252]), ("mix", mix)]:
            thr = thr_at_fp(v, other)
            d, lag, never = timing(v, ranges, cal, thr)
            trows.append(dict(name=sc["name"], variant=k, thr=thr, delay=d, lag=lag, never=never, n=len(ranges)))
        print(f"{sid:22s} move {ep_auc['move']:.2f} state252 {ep_auc['state252']:.2f} state756 {ep_auc['state756']:.2f} mix {rows[-1]['mix']:.2f} (a={a_all})", flush=True)

    R = pd.DataFrame(rows); T = pd.DataFrame(trows)

    # Global state threshold: lowest level at which scenarios are in place on at most FP_LIMIT of their
    # non-episode days on average (same rule as the move threshold in calibrate_scenarios.py)
    sweep = []
    for t in np.round(np.arange(0.0, 0.91, 0.05), 2):
        fps, hits = [], []
        for sid, sc in scen.items():
            ranges = cal_mod.EPISODES.get(sid)
            if not ranges:
                continue
            ep = cal_mod.episode_mask(cal, ranges)
            s = state_scores(S[252], sc["fp"])
            fps.append((s[~ep].dropna() >= t).mean()); hits.append((s[ep].dropna() >= t).mean())
        sweep.append((t, np.mean(fps), np.mean(hits)))
    ok = [x for x in sweep if x[1] <= FP_LIMIT]
    state_t = ok[0][0] if ok else np.nan
    print("state threshold sweep (t, mean FP, mean hit):", [(t, round(f, 3), round(h, 3)) for t, f, h in sweep])
    print("STATE_ACTIVE =", state_t)
    write(R, T, cal[-1], sweep, state_t)


def write(R, T, last, sweep, state_t):
    f = lambda v: "n/a" if pd.isna(v) else f"{v:.2f}"
    L = [f"# Move score vs state score — {dt.date.today():%Y-%m-%d}", "",
         "Generated by `RStudies/reports/macro_context/compare_state_move.py`; episodes and data as in `calibrate_scenarios.py`. "
         "Absolute breadth excluded (no history).", "",
         "## Separation, held-out episodes (mean per-episode AUC; 0.5 = chance)", "",
         "| Scenario | Episodes | Move | State 1y | State 3y | Mix (a chosen out of sample) | a on all episodes | a per held-out episode |",
         "|---|---|---|---|---|---|---|---|"]
    for _, r in R.iterrows():
        L.append(f"| {r['name']} | {r.n_ep} | {f(r.move)} | {f(r.s252)} | {f(r.s756)} | {f(r.mix)} | {r.a_all:g} | {r.a_held} |")
    L.append(f"| Mean | | {f(R.move.mean())} | {f(R.s252.mean())} | {f(R.s756.mean())} | {f(R.mix.mean())} | | |")
    L += ["", "a = share of the move score in the mix (1 = move only, 0 = state only).", "",
          "## Separation, all episodes together (AUC)", "",
          "| Scenario | Move | State 1y | State 3y |", "|---|---|---|---|"]
    for _, r in R.iterrows():
        L.append(f"| {r['name']} | {f(r.move_all)} | {f(r.s252_all)} | {f(r.s756_all)} |")
    L += ["", f"## Timing at the threshold that leaves {int(FP_LIMIT*100)}% of other days in place", "",
          "Delay = median sessions from episode start to the first day in place. Lag = median sessions after the episode ends "
          f"until the score drops below the threshold (capped at {LAG_CAP}). Missed = episodes never in place.", "",
          "| Scenario | Variant | Threshold | Delay | Lag after end | Missed / episodes |", "|---|---|---|---|---|---|"]
    for _, t in T.iterrows():
        L.append(f"| {t['name']} | {t.variant} | {t.thr:+.2f} | {f(t.delay) if not pd.isna(t.delay) else 'n/a'} | {f(t.lag)} | {t.never} / {t.n} |")
    L += ["", "## State threshold (1-year range)", "",
          f"Lowest threshold at which scenarios are in place on at most {int(FP_LIMIT*100)}% of their non-episode days on average: "
          f"**{state_t:.2f}**.", "", "| Threshold | Mean false positives | Mean hit rate |", "|---|---|---|"]
    for t, fp_, h in sweep:
        L.append(f"| {t:.2f} | {fp_:.0%} | {h:.0%} |")
    L += ["", f"## Current reading ({last:%Y-%m-%d}, without absolute breadth)", "",
          "| Scenario | Move | State 1y | Mix (a on all episodes) |", "|---|---|---|---|"]
    for _, r in R.sort_values("today_move", ascending=False).iterrows():
        L.append(f"| {r['name']} | {r.today_move:+.0%} | {r.today_state:+.0%} | {r.today_mix:+.0%} |")
    OUT.write_text("\n".join(L) + "\n", encoding="utf-8")
    print("written", OUT)


if __name__ == "__main__":
    main()
