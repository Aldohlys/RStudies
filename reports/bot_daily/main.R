# reports/bot_daily/main.R — BOT_daily, the technical tool.
#
# Writes bot_daily_<date>.csv exactly as specified in
# docs/BOT_TOOLS_DESIGN.md section 3. Price and volume only: no TWS, no option
# chain, no database writes.
#
# Run from the RStudies project root (renv):
#   Rscript reports/bot_daily/main.R [--detail] [--direction long|short|both]
#                                    [--out PATH] [SYM ...]
#
# --detail emits all 74 fields; the default emits the 44 key/decision fields.
# Naming an explicit symbol list bypasses universe membership.

suppressPackageStartupMessages({ library(Tdata) })

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else file.path("reports", "bot_daily")
})
SH <- file.path(SCRIPT_DIR, "..", "shared")
source(file.path(SH, "indicators.R"))
source(file.path(SH, "weekly.R"))
source(file.path(SH, "zones.R"))
source(file.path(SH, "gates.R"))

EM_DAYS    <- 10
# Fetch 5 years so the weekly resample has enough bars: calc_ind() needs 130
# rows, and 2 years of daily gives only ~104 weeks. Zones stay on the last 2
# years, which is the window the design document's trial was run on.
FETCH_YEARS <- 5
ZONE_YEARS  <- 2
OUT_DIR    <- "C:/Users/aldoh/Documents/NewTrading/reports"
UNIVERSE_CSV <- "C:/Users/aldoh/Documents/NewTrading/Strategies/tradable_universe_20260827.csv"

# ── Args ───────────────────────────────────────────────────────────────────
args      <- commandArgs(trailingOnly = TRUE)
detail    <- "--detail" %in% args
opt_at    <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && length(args) > i) args[i + 1L] else default
}
direction_arg <- opt_at("--direction", "long")
out_path      <- opt_at("--out", NA_character_)
syms_arg      <- args[!grepl("^--", args)]
consumed      <- c(opt_at("--direction", NULL), opt_at("--out", NULL))
syms_arg      <- setdiff(syms_arg, consumed)

DIRECTIONS <- if (identical(direction_arg, "both")) c("long", "short") else direction_arg

# ── Universe ───────────────────────────────────────────────────────────────
# Membership belongs to BOT_monthly (Tickers.BOT_Eligible). Until that column
# exists the curated book_BOT flag in the universe CSV is the fallback, and the
# fallback is logged rather than silent.
load_universe <- function() {
  if (length(syms_arg)) {
    return(data.frame(name = syms_arg, yahoo = syms_arg,
                      atr_band = NA_character_, gap_tercile = NA_character_,
                      bench = NA_character_, stringsAsFactors = FALSE))
  }
  from_tickers <- tryCatch({
    conn <- Tdata::safe_db_connect()
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    cols <- DBI::dbGetQuery(conn, "SELECT * FROM Tickers LIMIT 1")
    if (!"BOT_Eligible" %in% names(cols)) NULL else
      DBI::dbGetQuery(conn,
        "SELECT Name AS name, YahooName AS yahoo, ATR_Band AS atr_band,
                GapShare_Tercile AS gap_tercile
           FROM Tickers WHERE BOT_Eligible = 1")
  }, error = function(e) NULL)

  if (!is.null(from_tickers) && nrow(from_tickers)) {
    message(sprintf("Universe: %d names from Tickers.BOT_Eligible", nrow(from_tickers)))
    from_tickers$bench <- NA_character_
    return(from_tickers)
  }

  message("Universe: Tickers.BOT_Eligible not populated - falling back to ",
          basename(UNIVERSE_CSV), " book_BOT (run BOT_monthly to replace this)")
  u <- utils::read.csv(UNIVERSE_CSV, sep = ";", stringsAsFactors = FALSE)
  u <- u[u$book_BOT %in% c("Y", "y", "YES", "Yes"), , drop = FALSE]
  out <- data.frame(name = u$ibkr_name, yahoo = u$yahoo,
                    atr_band = NA_character_, gap_tercile = NA_character_,
                    bench = u$bench, stringsAsFactors = FALSE)
  # A name is never its own benchmark: rs20 would be identically zero and S3
  # could never pass. Carried over from bot_scan_universe.py::resolve_bench()
  # when that scanner was retired — TLT carried db_sector 'US bonds' -> TLT and
  # scored S=0 on every scan until it was caught. The CSV is clean today, so
  # this guards the next hand-edit of the bench column, not current data.
  self_bench <- !is.na(out$bench) & nzchar(out$bench) & out$bench == out$yahoo
  if (any(self_bench)) {
    message(sprintf("Bench == name for %d row(s) (%s) - S3 abstains for them",
                    sum(self_bench), paste(out$name[self_bench], collapse = ", ")))
    out$bench[self_bench] <- NA_character_
  }
  out
}

# ── Per-name read ──────────────────────────────────────────────────────────
fetch_daily <- function(sym) {
  d <- tryCatch(getSymIntervalDate(sym, Sys.Date() - round(FETCH_YEARS * 365), Sys.Date()),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) < 130) return(NULL)
  calc_ind(d)
}

.n <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) NA_real_ else x
.pct <- function(num, den) if (is.finite(num) && is.finite(den) && den != 0) num / den * 100 else NA_real_

one_row <- function(row, direction, bench_ret20) {
  d <- fetch_daily(row$yahoo)
  if (is.null(d)) return(NULL)
  last <- get_last(list(x = d), "x")
  if (is.null(last) || !nrow(last)) return(NULL)

  px  <- as.numeric(tail(d$Close, 1))
  atr <- as.numeric(tail(d$atr14, 1))
  if (!is.finite(px) || !is.finite(atr) || atr <= 0) return(NULL)

  # Expected move: the denominator for every "% of a typical 10-day move".
  em  <- tryCatch(atr_expected_move(row$yahoo, EM_DAYS, conf = 0.80, spot = px),
                  error = function(e) NULL)
  em_lo  <- .n(em$move_lower_pct);  em_hi <- .n(em$move_upper_pct)
  em_abs <- if (is.finite(em_hi)) px * em_hi / 100 else NA_real_
  em_div <- .n(em$regime_divergence)

  # Zones read the last ZONE_YEARS of the series; the rest of the fetch exists
  # for indicator warm-up and for the weekly resample.
  zd <- d[as.Date(d$date) >= Sys.Date() - round(ZONE_YEARS * 365), , drop = FALSE]
  lr <- level_read(if (nrow(zd) >= 130) zd else d, atr, em_abs)

  # Weekly half of the daily-vs-weekly read, resampled from the full fetch.
  wk  <- to_weekly(d)
  wkc <- if (!is.null(wk) && nrow(wk) >= 130) calc_ind(wk) else NULL
  wlast <- if (!is.null(wkc)) get_last(list(x = wkc), "x") else NULL
  w_ema50 <- if (!is.null(wlast) && nrow(wlast)) .n(wlast$ma50) else NA_real_

  rs20 <- if (is.finite(.n(last$ret20)) && is.finite(bench_ret20))
            .n(last$ret20) - bench_ret20 else NA_real_
  gi <- gate_inputs(last, rs20)
  gd <- eval_gates(gi, px, direction)
  gw <- if (!is.null(wlast) && nrow(wlast)) eval_gates(gate_inputs(wlast), px, direction) else NULL
  cs <- cluster_states(gd)
  wi <- if (!is.null(wlast) && nrow(wlast)) gate_inputs(wlast) else NULL

  res <- lr$res; sup <- lr$sup; fb <- lr$fib
  res_ref <- zone_ref(res, "res"); sup_ref <- zone_ref(sup, "sup")

  # Entry factors F1/F2, ported from bot_scan_universe.py::score_one() when that
  # scanner was retired. Validated on 93 realised trades (bot_book_design
  # 20260827.md 9b): ATR in its own top quartile carried 20.7R of 42.9R, and a
  # prior move of 2-3 ATR returned 0.76R per trade. Reported, never gated.
  atr_pct_now <- atr / px * 100
  atr_hist <- utils::tail(d$atr14 / d$Close * 100, 504)
  atr_hist <- atr_hist[is.finite(atr_hist)]
  atr_pctile <- if (length(atr_hist) > 250) mean(atr_hist < atr_pct_now) * 100 else NA_real_
  # Signed, in ATR units: (Close_t - Close_t-20) / atr14. Not ret20% / atr%,
  # which carries a spurious C_t/C_t-20 factor and inflates large up-moves.
  prior20_atr <- if (nrow(d) > 20 && is.finite(atr) && atr > 0)
                   (px - d$Close[nrow(d) - 20]) / atr else NA_real_
  f1 <- is.finite(atr_pctile) && atr_pctile >= 75
  f2 <- is.finite(prior20_atr) && abs(prior20_atr) >= 2

  # Overnight gap risk, measured against THIS trade's stop rather than against
  # the name's peers. Tickers.GapShare is the overnight SHARE of variance and
  # #82 A.3 established it as a stable name attribute, but across these 169
  # rows it does not predict whether a gap clears the stop: Spearman 0.073,
  # inside one standard error of zero. What does is the stop distance itself
  # (Spearman -0.889), which the zone engine sets per session. So the quantity
  # is trade-specific and belongs here, not in monthly membership. gap_share
  # still tracks gap SIZE as designed (Spearman 0.477 against gap_p95_atr).
  gdat <- utils::tail(d, 505)   # NOT `gd` - that is the gate vector from eval_gates()
  r_on <- log(gdat$Open[-1] / gdat$Close[-nrow(gdat)])
  r_on <- r_on[is.finite(r_on)]
  gap_p95_pct <- if (length(r_on) >= 200)
                   unname(stats::quantile(abs(r_on), 0.95)) * 100 else NA_real_
  stop_dist <- px - .n(lr$stop_px)
  gap_vs_stop <- if (is.finite(gap_p95_pct) && is.finite(stop_dist) && stop_dist > 0)
                   gap_p95_pct / 100 * px / stop_dist else NA_real_

  # Veto reasons compose: a row can be standing in a zone AND unable to hold
  # its stop overnight, and the reader wants both.
  veto <- character(0)
  if (isTRUE(lr$in_res_zone) && isTRUE(lr$in_sup_zone)) veto <- "in_both"
  else if (isTRUE(lr$in_res_zone)) veto <- "in_resistance"
  else if (isTRUE(lr$in_sup_zone)) veto <- "in_support"
  # >= 1 means a 95th-percentile overnight move covers the whole stop, so the
  # stop is not a stop: price gaps through it instead of trading through it.
  if (isTRUE(gap_vs_stop >= 1)) veto <- c(veto, "gap_through_stop")

  list(
    date = as.character(as.Date(tail(d$date, 1))),
    name = row$name, yahoo = row$yahoo, direction = direction,
    px = round(px, 4), atr = round(atr, 4), atr_pct = round(atr_pct_now, 3),
    atr_pctile = round(atr_pctile, 1), prior20_atr = round(prior20_atr, 2),
    entry_factors = as.integer(f1) + as.integer(f2),
    zz_th = round(lr$zz_th * 100, 3), n_pivots = lr$n_pivots,
    rng_pct_20 = round(.n(gi$rng_pct_20), 2), rng_dyn = round(.n(lr$rng_dyn), 2),
    zone_window_sessions = lr$zone_window_sessions,

    res_zone_lo = if (!is.null(res)) round(res$lo, 4) else NA_real_,
    res_zone_hi = if (!is.null(res)) round(res$hi, 4) else NA_real_,
    res_touches = if (!is.null(res)) res$touches else NA_integer_,
    res_first   = if (!is.null(res)) res$first else NA_character_,
    res_last    = if (!is.null(res)) res$last  else NA_character_,
    # Distances are to the edge in play (zone_ref): the lower edge when the zone
    # is overhead, the upper edge when spot stands inside it. Both stay >= 0 and
    # both answer the same question — how far to the level `target` sits on.
    res_dist_pct    = if (!is.null(res)) round(.pct(res_ref - px, px), 3) else NA_real_,
    res_dist_atr    = if (!is.null(res)) round((res_ref - px) / atr, 3) else NA_real_,
    res_pct_of_em10 = if (!is.null(res)) round(.pct(res_ref - px, em_abs), 1) else NA_real_,
    res2_zone_lo = if (!is.null(lr$res2)) round(lr$res2$lo, 4) else NA_real_,
    res2_zone_hi = if (!is.null(lr$res2)) round(lr$res2$hi, 4) else NA_real_,
    res2_touches = if (!is.null(lr$res2)) lr$res2$touches else NA_integer_,
    n_res_to_ext = lr$n_res_to_ext,

    sup_zone_lo = if (!is.null(sup)) round(sup$lo, 4) else NA_real_,
    sup_zone_hi = if (!is.null(sup)) round(sup$hi, 4) else NA_real_,
    sup_touches = if (!is.null(sup)) sup$touches else NA_integer_,
    sup_first   = if (!is.null(sup)) sup$first else NA_character_,
    sup_last    = if (!is.null(sup)) sup$last  else NA_character_,
    sup_dist_pct    = if (!is.null(sup)) round(.pct(px - sup_ref, px), 3) else NA_real_,
    sup_dist_atr    = if (!is.null(sup)) round((px - sup_ref) / atr, 3) else NA_real_,
    sup_pct_of_em10 = if (!is.null(sup)) round(.pct(px - sup_ref, em_abs), 1) else NA_real_,

    leg_low         = if (!is.null(fb)) round(fb$leg_low, 4) else NA_real_,
    leg_anchor_date = if (!is.null(fb)) fb$anchor_date else NA_character_,
    leg_high        = if (!is.null(fb)) round(fb$leg_high, 4) else NA_real_,
    fib_ret_382 = if (!is.null(fb)) round(unname(fb$ret[1]), 4) else NA_real_,
    fib_ret_500 = if (!is.null(fb)) round(unname(fb$ret[2]), 4) else NA_real_,
    fib_ret_618 = if (!is.null(fb)) round(unname(fb$ret[3]), 4) else NA_real_,
    fib_ext_1272 = if (!is.null(fb)) round(unname(fb$ext[1]), 4) else NA_real_,
    fib_ext_1618 = if (!is.null(fb)) round(unname(fb$ext[2]), 4) else NA_real_,

    target = round(.n(lr$target), 4), target_source = lr$target_source,
    target_agree = if (is.na(lr$fib_confirms_res)) NA_integer_
                   else as.integer(isTRUE(lr$fib_confirms_res)),
    stop = round(.n(lr$stop_px), 4), stop_source = lr$stop_source,
    # What asym rests on. Zones are validated by REPETITION (>= 2 attempts to
    # cross); Fibonacci levels and a flat 3x ATR stop are validated by nothing,
    # so a high asym built from both is a ratio of two unobserved levels.
    level_basis = {
      tz <- lr$target_source %in% c("zone", "in_zone")
      sz <- lr$stop_source %in% c("zone_stop", "in_zone_stop")
      if (tz && sz) "zone" else if (!tz && !sz) "geometric" else "mixed"
    },
    stop_agree = if (is.na(lr$fib_confirms_sup)) NA_integer_
                 else as.integer(isTRUE(lr$fib_confirms_sup)),
    asym = round(.n(lr$asym), 3), asym_fib = round(.n(lr$asym_fib), 3),

    em10_lo = em_lo, em10_hi = em_hi, em10_regime_div = em_div,

    ema50 = round(.n(gi$ema50), 4),
    ema50_disp_pct = round(.pct(px - .n(gi$ema50), .n(gi$ema50)), 3),
    ema50_slope = round(.n(gi$ema50_slope), 3),
    w_ema50 = round(w_ema50, 4),
    w_ema50_disp_pct = round(.pct(px - w_ema50, w_ema50), 3),

    d_squeeze = round(.n(gi$d_squeeze), 4),
    w_squeeze = if (!is.null(wi)) round(.n(wi$d_squeeze), 4) else NA_real_,
    d_vol_decline = round(.n(gi$d_vol_decline), 4),
    w_vol_decline = if (!is.null(wi)) round(.n(wi$d_vol_decline), 4) else NA_real_,
    d_vol_surge = round(.n(gi$d_vol_surge), 4),
    w_vol_surge = if (!is.null(wi)) round(.n(wi$d_vol_surge), 4) else NA_real_,

    obv_slope = .n(gi$obv_slope),
    obv_slope_days = round(.n(gi$obv_slope_days), 3),
    rsi14 = round(.n(gi$rsi14), 2), rsi_slope = round(.n(gi$rsi_slope), 2),
    updn_ratio = round(.n(gi$updn_ratio), 3), ret20 = round(.n(gi$ret20), 3),
    rs20 = round(.n(gi$rs20), 3), adx10 = round(.n(gi$adx10), 2),

    trend_state = cs$trend_state, compression_state = cs$compression_state,
    supply_state = cs$supply_state, rs_state = cs$rs_state,
    confluence = confluence_state(gd, gw),

    atr_band = row$atr_band, gap_tercile = row$gap_tercile,

    # Entry veto, assembled above. Price standing in a zone is mid-struggle,
    # with the level that decides the move at arm's length on both sides; a
    # stop a single overnight move can clear is not a stop. The row is kept
    # either way so the level read stays visible.
    tradable = as.integer(length(veto) == 0),
    veto_reason = paste(veto, collapse = "+"),
    gap_p95_pct = round(gap_p95_pct, 2),
    gap_vs_stop = round(gap_vs_stop, 2),

    note = if (identical(lr$stop_source, "atr_stop"))
             sprintf("nearest support %.1f ATR away - ATR stop used",
                     if (!is.null(sup)) (px - sup$hi) / atr else NA_real_) else "")
}

# Column order and tiers are the spec's, kept here so a schema change is one edit.
COLS <- c("date","name","yahoo","direction","tradable","veto_reason",
  "px","atr","atr_pct","atr_pctile","prior20_atr","entry_factors",
  "zz_th","n_pivots",
  "rng_pct_20","rng_dyn","zone_window_sessions",
  "res_zone_lo","res_zone_hi","res_touches","res_first","res_last",
  "res_dist_pct","res_dist_atr","res_pct_of_em10",
  "res2_zone_lo","res2_zone_hi","res2_touches","n_res_to_ext",
  "sup_zone_lo","sup_zone_hi","sup_touches","sup_first","sup_last",
  "sup_dist_pct","sup_dist_atr","sup_pct_of_em10",
  "leg_low","leg_anchor_date","leg_high",
  "fib_ret_382","fib_ret_500","fib_ret_618","fib_ext_1272","fib_ext_1618",
  "target","target_source","target_agree","stop","stop_source","stop_agree",
  "gap_p95_pct","gap_vs_stop",
  "level_basis","asym","asym_fib","em10_lo","em10_hi","em10_regime_div",
  "ema50","ema50_disp_pct","ema50_slope","w_ema50","w_ema50_disp_pct",
  "d_squeeze","w_squeeze","d_vol_decline","w_vol_decline","d_vol_surge","w_vol_surge",
  "obv_slope","obv_slope_days","rsi14","rsi_slope","updn_ratio","ret20","rs20","adx10",
  "trend_state","compression_state","supply_state","rs_state","confluence",
  "atr_band","gap_tercile","note")
DETAIL_ONLY <- c("yahoo","atr_pct","zz_th","n_pivots","rng_pct_20","rng_dyn",
  "res_first","res_dist_pct","sup_first","sup_dist_pct",
  "fib_ret_382","fib_ret_500","em10_lo","ema50","ema50_slope","w_ema50",
  "d_squeeze","w_squeeze","d_vol_decline","w_vol_decline","d_vol_surge","w_vol_surge",
  "obv_slope","obv_slope_days","rsi14","rsi_slope","updn_ratio","ret20","rs20","adx10")

# ── Run ────────────────────────────────────────────────────────────────────
uni <- load_universe()
message(sprintf("BOT_daily: %d names x %d direction(s)", nrow(uni), length(DIRECTIONS)))

# Benchmark 20-day returns for S3, one fetch per distinct benchmark.
bench_ret <- list()
for (b in unique(stats::na.omit(uni$bench))) {
  bd <- tryCatch(getSymIntervalDate(b, Sys.Date() - 120, Sys.Date()), error = function(e) NULL)
  if (!is.null(bd) && nrow(bd) > 21) {
    cl <- bd$Close[!is.na(bd$Close)]
    bench_ret[[b]] <- (cl[length(cl)] / cl[length(cl) - 20] - 1) * 100
  }
}

rows <- list()
for (i in seq_len(nrow(uni))) {
  r <- uni[i, , drop = FALSE]
  br <- if (!is.na(r$bench) && !is.null(bench_ret[[r$bench]])) bench_ret[[r$bench]] else NA_real_
  for (dir in DIRECTIONS) {
    out <- tryCatch(one_row(r, dir, br), error = function(e) {
      message(sprintf("  %s (%s): %s", r$name, dir, conditionMessage(e))); NULL })
    if (!is.null(out)) rows[[length(rows) + 1]] <- out
  }
  if (i %% 20 == 0) message(sprintf("  ... %d/%d", i, nrow(uni)))
}

if (!length(rows)) { message("No rows produced."); quit(status = 1) }

df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
df <- df[, COLS, drop = FALSE]
if (!detail) df <- df[, setdiff(COLS, DETAIL_ONLY), drop = FALSE]

# Reading order: tradable rows first, then trend_state desc, then
# res_pct_of_em10 asc. Not a ranking. Vetoed rows keep their level read and sit
# below the block that can actually be traded today.
# asym is NOT a sort key (TODO 88.3): it is unbounded, anti-correlated with the
# trend cluster (Spearman -0.335 on 69 names) and highest on rows whose target
# rests on no observed level, so sorting on it put 0/6-trend names with
# unreachable targets at the top while the book enters S1 at 79.8% of trades.
trend_n <- suppressWarnings(as.integer(sub("/.*$", "", df$trend_state)))
df <- df[order(-df$tradable,
               -ifelse(is.na(trend_n), -1L, trend_n),
               ifelse(is.na(df$res_pct_of_em10), Inf, df$res_pct_of_em10)), , drop = FALSE]

if (is.na(out_path))
  out_path <- file.path(OUT_DIR, sprintf("bot_daily_%s.csv", format(Sys.Date(), "%Y%m%d")))
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
utils::write.table(df, out_path, sep = ";", row.names = FALSE, na = "", qmethod = "double")

message(sprintf("Wrote %d rows x %d cols -> %s", nrow(df), ncol(df), out_path))
message(sprintf("Tradable: %d of %d  (vetoed: %s)", sum(df$tradable == 1L), nrow(df),
                paste(sprintf("%s %d", names(table(df$veto_reason[df$tradable == 0L])),
                              table(df$veto_reason[df$tradable == 0L])), collapse = ", ")))
print(utils::head(df[, intersect(c("name","direction","px","target","target_source",
  "stop","stop_source","asym","trend_state","confluence"), names(df))], 12))
