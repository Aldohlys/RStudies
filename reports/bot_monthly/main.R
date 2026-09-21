# reports/bot_monthly/main.R — BOT_monthly, universe membership.
#
# Writes the Tickers columns of docs/BOT_TOOLS_DESIGN.md section 1 and
# bot_monthly_<date>.csv of section 2. Replaces the hand-curated book_BOT flag,
# so every filter that used to live in that curation is explicit here.
#
# Three phases, because two of the criteria are cross-sectional: the gap-share
# and vol-of-vol terciles are cut against the universe, not against a name's own
# history, so membership cannot be decided one ticker at a time.
#
# Run from the RStudies project root (renv):
#   Rscript reports/bot_monthly/main.R [--dry-run] [--db PATH] [--no-tws]
#                                      [--out PATH] [--limit N] [SYM ...]
#
# --dry-run computes and writes the CSV but does not touch Tickers.
# --db runs against a copy of the database; use it before the first real run.
# The ATM bid-ask needs live quotes, so a full run belongs in market hours.

suppressPackageStartupMessages({ library(Tdata); library(DBI) })

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else file.path("reports", "bot_monthly")
})
SH <- file.path(SCRIPT_DIR, "..", "shared")
source(file.path(SH, "indicators.R"))
source(file.path(SH, "gates.R"))
source(file.path(SH, "name_attributes.R"))
source(file.path(SH, "live_sources.R"))

GATE_VERSION <- "v1"   # bump when eval_gates() changes; makes a stale count detectable
EM_DAYS      <- 10
FETCH_YEARS  <- 5
ADV_FLOOR    <- 85     # CHF m/day; reproduces today's book exactly (min of book_BOT=Y is 89)
BIDASK_STOCK <- 8      # above this the option vehicles are unavailable
OUT_DIR      <- "C:/Users/aldoh/Documents/NewTrading/reports"

args    <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
no_tws  <- "--no-tws"  %in% args
opt <- function(flag, default = NA_character_) {
  i <- match(flag, args); if (!is.na(i) && length(args) > i) args[i + 1L] else default
}
db_path  <- opt("--db"); out_path <- opt("--out")
lim      <- suppressWarnings(as.integer(opt("--limit")))
syms_arg <- setdiff(args[!grepl("^--", args)], c(opt("--db"), opt("--out"), opt("--limit")))

`%||%` <- function(a, b) if (is.null(a) || length(a) != 1 || is.na(a)) b else a
.n <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) NA_real_ else x
# A FAILED row carries only status and notes, so every other field is NULL there.
# A NULL field becomes a ZERO-LENGTH column and as.data.frame() then refuses the
# whole row list ("les arguments impliquent des nombres de lignes differentes").
.s <- function(x) if (is.null(x) || length(x) != 1) NA_character_ else as.character(x)
.i <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) NA_integer_ else as.integer(x)

SCHEMA <- c(
  BOT_Eligible = "INTEGER", BOT_Reason = "TEXT", BOT_VehicleHint = "TEXT",
  BOT_LastUpdate = "TEXT", BOT_GateVersion = "TEXT",
  ATR_Pct = "REAL", ATR_Med5y = "REAL", ATR_P25 = "REAL", ATR_P75 = "REAL",
  ATR_Band = "TEXT", GapShare = "REAL", GapShare_Tercile = "TEXT",
  VoV = "REAL", VoV_Pctile = "REAL", VoV_Tercile = "TEXT",
  AtmBidAskPct = "REAL", AtmBidAsk_AsOf = "TEXT",
  ADV_CHF_M = "REAL", ADV_Pass = "INTEGER",
  ATR_MoveCoefHi = "REAL", EM10_Hi_Pct = "REAL",
  BOT_ModelSigma = "REAL", BOT_IVSource = "TEXT", BOT_ModelCallCost = "REAL",
  BOT_BreakevenPct = "REAL", BOT_BreakevenAtr = "REAL", BOT_BreakevenPctEm10 = "REAL",
  BOT_Opportunities_2y = "INTEGER", BOT_LastOpportunity = "TEXT")

connect <- function() {
  if (!is.na(db_path)) DBI::dbConnect(RSQLite::SQLite(), db_path) else Tdata::safe_db_connect()
}

migrate <- function(conn) {
  have <- names(DBI::dbGetQuery(conn, "SELECT * FROM Tickers LIMIT 1"))
  add <- setdiff(names(SCHEMA), have)
  for (col in add)
    DBI::dbExecute(conn, sprintf('ALTER TABLE Tickers ADD COLUMN "%s" %s', col, SCHEMA[[col]]))
  message(if (length(add)) sprintf("Tickers: added %d column(s)", length(add))
          else "Tickers: schema already current")
}

# ── Phase 1: per-ticker quantities ─────────────────────────────────────────
compute_one <- function(tk, fx_rate, tws_up) {
  yh <- if (!is.na(tk$YahooName) && nzchar(tk$YahooName)) tk$YahooName else tk$Name
  base <- list(name = tk$Name, yahoo = yh, type = tk$Type, currency = tk$Currency)

  d <- tryCatch(getSymIntervalDate(yh, Sys.Date() - round(FETCH_YEARS * 365), Sys.Date()),
                error = function(e) NULL)
  if (is.null(d) || nrow(d) < 150)
    return(c(base, list(status = "FAILED", notes = "no price history")))

  di <- calc_ind(d)
  if (is.null(di)) return(c(base, list(status = "FAILED", notes = "indicators unavailable")))
  px <- as.numeric(tail(d$Close, 1))
  notes <- character(0)

  ap  <- atr_profile(d)
  gs  <- gap_share(d)
  adv <- adv_chf_m(d, tk$Currency, fx_rate)
  if (!is.finite(adv)) notes <- c(notes, "adv: no FX rate")

  vv  <- tryCatch(Tdata::compute_vol_of_vol(yh), error = function(e) NULL)
  em  <- tryCatch(atr_expected_move(yh, EM_DAYS, conf = 0.80, spot = px), error = function(e) NULL)
  coef_hi <- .n(em$coef_upper); em_hi <- .n(em$move_upper_pct)
  em_abs  <- if (is.finite(em_hi)) px * em_hi / 100 else NA_real_
  if (!is.finite(coef_hi)) notes <- c(notes, "expected-move: insufficient history")

  sigma <- hist_vol(d, 20);  iv_src <- "hv20"
  if (!is.finite(sigma)) { sigma <- hist_vol(d, 120); iv_src <- "hv120" }
  atr_abs <- if (!is.null(ap)) ap$atr_pct / 100 * px else NA_real_
  mult <- suppressWarnings(as.numeric(tk$Multiplier)); if (!is.finite(mult)) mult <- 100
  bk <- model_call_breakeven(px, sigma, atr_abs, em_abs, dte = 30, multiplier = mult)

  ba <- NA_real_; ba_asof <- NA_character_; status <- "OK"
  if (identical(tk$Type, "FUT")) {
    status <- "NA_TYPE"; notes <- c(notes, "bid-ask: not applicable to a future")
  } else if (!tws_up) {
    notes <- c(notes, "bid-ask: TWS not reachable")
  } else {
    # resolve_option_spread() prices only the ATM strike and the 30-delta wings
    # rather than the whole chain, and force-refreshes: the parquet cache can
    # hold rows whose bid/ask are NaN, which would read as a spurious miss.
    q <- tryCatch(resolve_option_spread(tk$Name, px, target_dte = 30, tws_ok = TRUE),
                  error = function(e) NULL)
    # NOT `q$value %||% NULL`: the %||% above is scalar-only (length(a) != 1
    # falls through to b), and q$value is an 8-element list, so it returned
    # NULL for every ticker on every run. That is why AtmBidAskPct and
    # BOT_VehicleHint were NULL on all 352 rows even with TWS connected, while
    # the note recorded "bid-ask: LIVE" — the fetch had genuinely succeeded.
    v <- if (!is.null(q) && is.list(q)) q$value else NULL
    if (!is.null(v) && is.finite(.n(v$atm_bid_ask_pct))) {
      ba <- .n(v$atm_bid_ask_pct); ba_asof <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    } else {
      # paste0() on a zero-length argument yields character(0), which would
      # append nothing and leave the row explaining itself with an empty note.
      # live_sources.R uses .ok()/.miss(), which carry the cause in $reason and
      # the LIVE / NO DATA / FETCH FAILED distinction in $status.
      why <- paste(stats::na.omit(c(q$status, q$reason)), collapse = ": ")
      if (!length(why) || !nzchar(why)) why <- "no ATM spread returned"
      notes <- c(notes, paste0("bid-ask: ", why))
    }
  }

  opp <- count_opportunities(di, lookback = 504, direction = "long")
  notes <- notes[nzchar(notes)]
  if (length(notes) && identical(status, "OK")) status <- "PARTIAL"

  c(base, list(
    status = status, notes = notes, px = px,
    atr_pct = if (!is.null(ap)) ap$atr_pct else NA_real_,
    atr_med5y = if (!is.null(ap)) ap$med5y else NA_real_,
    atr_p25 = if (!is.null(ap)) ap$p25 else NA_real_,
    atr_p75 = if (!is.null(ap)) ap$p75 else NA_real_,
    atr_band = if (!is.null(ap)) ap$band else NA_character_,
    gap_share = gs, vov = .n(vv$vol_of_vol), vov_pctile = .n(vv$vov_percentile),
    adv = adv, atm_bid_ask = ba, atm_bid_ask_asof = ba_asof,
    coef_hi = coef_hi, em_hi = em_hi, sigma = sigma, iv_src = iv_src,
    call_cost = bk$call_cost, be_pct = bk$breakeven_pct,
    be_atr = bk$breakeven_atr, be_pct_em10 = bk$breakeven_pct_em10,
    opp_n = opp$n, opp_last = opp$last_date))
}

# ── Run ────────────────────────────────────────────────────────────────────
conn <- connect()
migrate(conn)
tickers <- DBI::dbGetQuery(conn,
  "SELECT Name, YahooName, Type, Currency, Multiplier FROM Tickers
    WHERE Type IN ('STK','ETF','FUT') OR IV = 'YES'")
DBI::dbDisconnect(conn)

if (length(syms_arg)) tickers <- tickers[tickers$Name %in% syms_arg, , drop = FALSE]
if (!is.na(lim) && lim > 0) tickers <- utils::head(tickers, lim)
message(sprintf("BOT_monthly: %d tickers", nrow(tickers)))

tws_up <- if (no_tws) FALSE else isTRUE(tryCatch(Tdata::isIBAvailable(), error = function(e) FALSE))
message(if (tws_up) "TWS reachable - fetching ATM bid-ask"
        else "TWS not reachable - AtmBidAskPct stays NULL, with a reason per row")

# getLastCHFValue() returns a data.frame(date, currency, chf_value), not a scalar.
fx <- list()
for (cur in unique(stats::na.omit(tickers$Currency))) {
  fx[[cur]] <- if (identical(cur, "CHF")) 1 else tryCatch({
    v <- Tdata::getLastCHFValue(cur)
    if (is.data.frame(v) && nrow(v)) as.numeric(v$chf_value[1]) else as.numeric(v)[1]
  }, error = function(e) NA_real_)
}

res <- list()
for (i in seq_len(nrow(tickers))) {
  tk <- tickers[i, , drop = FALSE]
  r <- tryCatch(compute_one(tk, fx[[tk$Currency]] %||% NA_real_, tws_up),
                error = function(e) { message(sprintf("  %s: %s", tk$Name, conditionMessage(e))); NULL })
  if (!is.null(r)) res[[length(res) + 1]] <- r
  if (i %% 25 == 0) message(sprintf("  ... %d/%d", i, nrow(tickers)))
}
if (!length(res)) { message("nothing computed"); quit(status = 1) }
cache_path <- file.path(tempdir(), "bot_monthly_phase1.rds")
saveRDS(res, cache_path)
message("phase 1 cached at ", cache_path, " (", length(res), " rows)")

# ── Phase 2: cross-sectional terciles ──────────────────────────────────────
# Cut against the universe, which is why this cannot be a per-ticker decision.
gs_all  <- vapply(res, function(r) .n(r$gap_share), numeric(1))
vov_all <- vapply(res, function(r) .n(r$vov_pctile), numeric(1))
gs_brk  <- as.numeric(stats::quantile(gs_all,  c(1/3, 2/3), na.rm = TRUE))
vov_brk <- as.numeric(stats::quantile(vov_all, c(1/3, 2/3), na.rm = TRUE))
tercile <- function(x, brk) {
  if (!is.finite(x) || any(!is.finite(brk))) return(NA_character_)
  if (x <= brk[1]) "low" else if (x <= brk[2]) "mid" else "high"
}
message(sprintf("gap-share terciles at %.3f / %.3f; vov-percentile terciles at %.1f / %.1f",
                gs_brk[1], gs_brk[2], vov_brk[1], vov_brk[2]))

# ── Phase 3: membership, CSV, Tickers ──────────────────────────────────────
today <- format(Sys.Date(), "%Y-%m-%d")
rows <- lapply(res, function(r) {
  gs_t  <- tercile(.n(r$gap_share), gs_brk)
  vov_t <- tercile(.n(r$vov_pctile), vov_brk)
  adv_pass <- if (is.finite(.n(r$adv))) as.integer(.n(r$adv) >= ADV_FLOOR) else NA_integer_

  # Bid-ask sets the vehicle, it does not exclude the name: a wide quote removes
  # the option vehicles, and plain stock is pure delta.
  hint <- if (is.finite(.n(r$atm_bid_ask)))
            (if (.n(r$atm_bid_ask) > BIDASK_STOCK) "stock_only" else "options") else NA_character_

  # GapShare_Tercile is NOT a membership criterion (TODO 88.4, closed
  # 2026-09-21). It was meant to keep out names whose price gaps THROUGH a
  # stop, but across 169 daily rows GapShare does not predict that: Spearman
  # 0.073 against the share of the stop a p95 overnight move covers, while the
  # stop distance itself correlates -0.889. The risk is per-trade, so BOT_daily
  # tests it directly as gap_vs_stop and vetoes with gap_through_stop.
  # GapShare and its tercile are still computed and written, as sizing context.
  reason <- if (identical(r$status, "FAILED")) "no_data"
            else if (is.na(.s(r$atr_band))) "no_atr"
            else if (identical(.s(r$atr_band), "low")) "atr_band"
            else if (identical(adv_pass, 0L)) "adv"
            else if (is.finite(.n(r$opp_n)) && r$opp_n < 1) "no_opportunity"
            else "eligible"
  elig <- as.integer(identical(reason, "eligible"))

  list(name = .s(r$name), yahoo = .s(r$yahoo), type = .s(r$type), currency = .s(r$currency),
       bot_eligible = .i(elig), bot_reason = .s(reason), vehicle_hint = .s(hint),
       atr_pct = round(.n(r$atr_pct), 3), atr_band = .s(r$atr_band),
       gap_share = round(.n(r$gap_share), 4), gap_tercile = .s(gs_t),
       vov = round(.n(r$vov), 4), vov_pctile = round(.n(r$vov_pctile), 1), vov_tercile = .s(vov_t),
       atm_bid_ask_pct = round(.n(r$atm_bid_ask), 3),
       adv_chf_m = round(.n(r$adv), 1), adv_pass = .i(adv_pass),
       atr_move_coef_hi = round(.n(r$coef_hi), 3), em10_hi_pct = round(.n(r$em_hi), 3),
       model_sigma = round(.n(r$sigma), 4), iv_source = .s(r$iv_src),
       model_call_cost = round(.n(r$call_cost), 2),
       breakeven_pct = round(.n(r$be_pct), 3), breakeven_atr = round(.n(r$be_atr), 3),
       breakeven_pct_em10 = round(.n(r$be_pct_em10), 1),
       opportunities_2y = .i(r$opp_n), last_opportunity = .s(r$opp_last),
       gate_version = GATE_VERSION, fetch_status = .s(r$status),
       fetch_note = paste(r$notes, collapse = "; "),
       .atr_med5y = .n(r$atr_med5y), .atr_p25 = .n(r$atr_p25), .atr_p75 = .n(r$atr_p75),
       .ba_asof = .s(r$atm_bid_ask_asof))
})
df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))

CSV_COLS <- c("name","yahoo","type","currency","bot_eligible","bot_reason","vehicle_hint",
  "atr_pct","atr_band","gap_share","gap_tercile","vov","vov_pctile","vov_tercile",
  "atm_bid_ask_pct","adv_chf_m","adv_pass","atr_move_coef_hi","em10_hi_pct",
  "model_sigma","iv_source","model_call_cost","breakeven_pct","breakeven_atr",
  "breakeven_pct_em10","opportunities_2y","last_opportunity","gate_version",
  "fetch_status","fetch_note")
if (is.na(out_path))
  out_path <- file.path(OUT_DIR, sprintf("bot_monthly_%s.csv", format(Sys.Date(), "%Y%m%d")))
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
utils::write.table(df[, CSV_COLS, drop = FALSE], out_path, sep = ";",
                   row.names = FALSE, na = "", qmethod = "double")
message(sprintf("Wrote %d rows -> %s", nrow(df), out_path))

if (dry_run) {
  message("--dry-run: Tickers not written")
} else {
  conn <- connect(); on.exit(DBI::dbDisconnect(conn), add = TRUE)
  sql <- paste0("UPDATE Tickers SET ",
    paste(sprintf('"%s" = :%s', names(SCHEMA), names(SCHEMA)), collapse = ", "),
    " WHERE Name = :Name")
  params <- data.frame(
    BOT_Eligible = df$bot_eligible, BOT_Reason = df$bot_reason,
    BOT_VehicleHint = df$vehicle_hint, BOT_LastUpdate = today, BOT_GateVersion = df$gate_version,
    ATR_Pct = df$atr_pct, ATR_Med5y = df$.atr_med5y, ATR_P25 = df$.atr_p25, ATR_P75 = df$.atr_p75,
    ATR_Band = df$atr_band, GapShare = df$gap_share, GapShare_Tercile = df$gap_tercile,
    VoV = df$vov, VoV_Pctile = df$vov_pctile, VoV_Tercile = df$vov_tercile,
    AtmBidAskPct = df$atm_bid_ask_pct, AtmBidAsk_AsOf = df$.ba_asof,
    ADV_CHF_M = df$adv_chf_m, ADV_Pass = df$adv_pass,
    ATR_MoveCoefHi = df$atr_move_coef_hi, EM10_Hi_Pct = df$em10_hi_pct,
    BOT_ModelSigma = df$model_sigma, BOT_IVSource = df$iv_source,
    BOT_ModelCallCost = df$model_call_cost, BOT_BreakevenPct = df$breakeven_pct,
    BOT_BreakevenAtr = df$breakeven_atr, BOT_BreakevenPctEm10 = df$breakeven_pct_em10,
    BOT_Opportunities_2y = df$opportunities_2y, BOT_LastOpportunity = df$last_opportunity,
    Name = df$name, stringsAsFactors = FALSE)
  DBI::dbExecute(conn, sql, params = as.list(params))
  message(sprintf("Tickers updated for %d names", nrow(params)))
}

message(sprintf("eligible %d / %d", sum(df$bot_eligible == 1L, na.rm = TRUE), nrow(df)))
print(table(df$bot_reason, useNA = "ifany"))
