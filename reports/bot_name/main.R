# reports/bot_name/main.R — BOT_name, execution for one name.
#
# Writes bot_name_<SYM>_<date>.csv as specified in
# docs/BOT_TOOLS_DESIGN.md section 4: one row per candidate structure across all
# three vehicles, each marked ACCEPT or REJECT against a per-vehicle test, and a
# verdict that can be "no acceptable execution".
#
# Self-contained: it recomputes the level read rather than reading BOT_daily's
# CSV, so a single name can be analysed without a daily run having happened.
#
# Run from the RStudies project root (renv):
#   Rscript reports/bot_name/main.R SYM [--direction long|short] [--budget 400]
#                                       [--out PATH] [--no-tws]

suppressPackageStartupMessages({ library(Tdata) })

SCRIPT_DIR <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else file.path("reports", "bot_name")
})
SH <- file.path(SCRIPT_DIR, "..", "shared")
for (f in c("indicators.R", "weekly.R", "zones.R", "gates.R", "name_attributes.R"))
  source(file.path(SH, f))

EM_DAYS     <- 10
FETCH_YEARS <- 5
ZONE_YEARS  <- 2
HOLD_DAYS   <- 12          # median hold: 17 calendar days ~ 12 trading sessions
DTE_TARGET  <- 30
WIDTHS      <- c(5, 10)
MIN_PAYOFF  <- 2           # avg win / avg loss > 2 (strategy profile)
MAX_DEBIT_PCT_WIDTH <- 33  # debit <= 33% of width gives >= 2:1 on a vertical
OUT_DIR     <- "C:/Users/aldoh/Documents/NewTrading/reports"

args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag, default = NA_character_) {
  i <- match(flag, args); if (!is.na(i) && length(args) > i) args[i + 1L] else default
}
no_tws    <- "--no-tws" %in% args
direction <- opt("--direction", "long")
budget    <- suppressWarnings(as.numeric(opt("--budget", "400")))
out_path  <- opt("--out")
sym <- setdiff(args[!grepl("^--", args)],
               c(opt("--direction"), opt("--budget", "400"), opt("--out")))[1]
if (is.na(sym) || !nzchar(sym)) stop("usage: Rscript reports/bot_name/main.R SYM [...]")

`%||%` <- function(a, b) if (is.null(a) || length(a) != 1 || is.na(a)) b else a
.n <- function(x) if (is.null(x) || length(x) != 1 || !is.finite(x)) NA_real_ else x
.col <- function(df, ...) { for (n in c(...)) if (n %in% names(df)) return(df[[n]]); rep(NA_real_, nrow(df)) }

# ── Structural read: target and stop come from the same engine as BOT_daily ──
tk  <- tryCatch(Tdata::getTicker(sym), error = function(e) NULL)
yh  <- if (!is.null(tk) && nzchar(tk$YahooName[1] %||% "")) tk$YahooName[1] else sym
mult <- suppressWarnings(as.numeric(tk$Multiplier[1])); if (!is.finite(mult)) mult <- 100
tclass <- if (!is.null(tk) && nzchar(tk$TradingClass[1] %||% "")) tk$TradingClass[1] else sym

d <- getSymIntervalDate(yh, Sys.Date() - round(FETCH_YEARS * 365), Sys.Date())
if (is.null(d) || nrow(d) < 150) stop("no price history for ", sym)
di  <- calc_ind(d)
px  <- as.numeric(tail(d$Close, 1))
atr <- as.numeric(tail(di$atr14, 1))
em  <- tryCatch(atr_expected_move(yh, EM_DAYS, conf = 0.80, spot = px), error = function(e) NULL)
em_hi_abs <- if (is.finite(.n(em$move_upper_pct))) px * .n(em$move_upper_pct) / 100 else NA_real_

zd <- d[as.Date(d$date) >= Sys.Date() - round(ZONE_YEARS * 365), , drop = FALSE]
lr <- level_read(if (nrow(zd) >= 130) zd else d, atr, em_hi_abs)
target <- .n(lr$target); stop_px <- .n(lr$stop_px)

message(sprintf("%s  px %.2f  target %.2f (%s)  stop %.2f (%s)  asym %.2f:1  %s of a %dd move",
                sym, px, target, lr$target_source, stop_px, lr$stop_source, .n(lr$asym),
                if (is.finite(.n(lr$target_pct_of_em))) sprintf("%.0f%%", lr$target_pct_of_em) else "n/a",
                EM_DAYS))

tws_ok <- if (no_tws) FALSE else isTRUE(tryCatch(Tdata::isIBAvailable(), error = function(e) FALSE))
asof <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# ── Expiry near DTE_TARGET ─────────────────────────────────────────────────
expiry <- NA_character_; dte <- NA_integer_
if (tws_ok) {
  exps <- tryCatch(as.character(Tdata:::tdata_py$getExpirationDates(sym)), error = function(e) NULL)
  if (!is.null(exps)) {
    dd <- as.integer(as.Date(exps, "%Y%m%d") - Sys.Date())
    ok <- which(!is.na(dd) & dd >= 14)
    if (length(ok)) { i <- ok[which.min(abs(dd[ok] - DTE_TARGET))]; expiry <- exps[i]; dte <- dd[i] }
  }
}

rows <- list()
add <- function(...) rows[[length(rows) + 1]] <<- list(...)

# ── Vehicle 1: stock ───────────────────────────────────────────────────────
# No convexity and no theta, so the payoff has to come entirely from stop
# placement: the structural asymmetry IS the payoff ratio.
stock_ratio <- .n(lr$asym)
add(vehicle = "stock", expiry = NA_character_, dte = NA_integer_,
    long_strike = NA_real_, short_strike = NA_real_, width = NA_real_,
    debit = NA_real_, premium = round(px * 100, 2), breakeven = round(px, 4),
    max_reward = round((target - px) * 100, 2), payoff_ratio = round(stock_ratio, 3),
    debit_pct_width = NA_real_, theta_pct_prem_day = 0,
    pct_of_em10_to_breakeven = 0,
    accept = if (is.finite(stock_ratio) && stock_ratio > MIN_PAYOFF) "ACCEPT" else "REJECT",
    reject_reason = if (is.finite(stock_ratio) && stock_ratio > MIN_PAYOFF) ""
                    else "stop_too_wide",
    prob_success = NA_real_, edge = NA_real_, ev = NA_real_,
    net_delta = 1, net_gamma = 0, net_theta = 0,
    bid_ask_pct = NA_real_, oi_long = NA_integer_, oi_short = NA_integer_,
    source = "computed")

# ── Vehicle 2: outright ────────────────────────────────────────────────────
right <- if (identical(direction, "long")) "C" else "P"
if (tws_ok && !is.na(expiry)) {
  strikes <- tryCatch({
    all <- sort(as.numeric(unlist(Tdata:::tdata_py$getStrikesInRange(
      sym = sym, expiration = expiry, center_strike = px, range_pct = 0.08))))
    all[!is.na(all)]
  }, error = function(e) numeric(0))
  if (length(strikes)) {
    q <- tryCatch(Tdata::getOptMarketData(sym, right, strikes, expiry, force_refresh = TRUE),
                  error = function(e) NULL)
    if (!is.null(q) && nrow(q)) {
      k  <- .col(q, "strike"); bid <- .col(q, "bid"); ask <- .col(q, "ask")
      mid <- .col(q, "mid");   dl <- .col(q, "delta")
      gm <- .col(q, "gamma");  th <- .col(q, "theta")
      oi <- .col(q, "open_interest", "openInterest", "oi")
      for (i in seq_len(nrow(q))) {
        prem <- .n(mid[i]) * mult
        if (!is.finite(prem) || prem <= 0) next
        be <- k[i] + prem / mult
        # Decay over the hold, from the quoted theta; the model call cost is not
        # used here because a live quote is available.
        decay <- if (is.finite(.n(th[i]))) abs(.n(th[i])) * mult * HOLD_DAYS else 0
        payoff <- if (is.finite(target) && target > be) (target - be) * mult / prem else NA_real_
        net <- if (is.finite(target)) (target - be) * mult - decay else NA_real_
        ba <- if (is.finite(.n(bid[i])) && is.finite(.n(ask[i])) && is.finite(.n(mid[i])) && mid[i] > 0)
                (ask[i] - bid[i]) / mid[i] * 100 else NA_real_
        rej <- if (prem > budget) "premium_over_budget"
               else if (is.finite(net) && net <= 0) "decay_too_fast"
               else if (!is.finite(payoff) || payoff <= MIN_PAYOFF) "payoff_below_target"
               else ""
        add(vehicle = "outright", expiry = expiry, dte = dte,
            long_strike = k[i], short_strike = NA_real_, width = NA_real_,
            debit = NA_real_, premium = round(prem, 2), breakeven = round(be, 4),
            max_reward = NA_real_, payoff_ratio = round(payoff, 3),
            debit_pct_width = NA_real_,
            theta_pct_prem_day = if (is.finite(.n(th[i])) && prem > 0)
                                   round(.n(th[i]) * mult / prem * 100, 3) else NA_real_,
            pct_of_em10_to_breakeven = if (is.finite(em_hi_abs))
                                         round((be - px) / em_hi_abs * 100, 1) else NA_real_,
            accept = if (nzchar(rej)) "REJECT" else "ACCEPT", reject_reason = rej,
            prob_success = round(abs(.n(dl[i])), 4), edge = NA_real_, ev = NA_real_,
            net_delta = round(.n(dl[i]), 4), net_gamma = round(.n(gm[i]), 6),
            net_theta = round(.n(th[i]) * mult, 4),
            bid_ask_pct = round(ba, 3), oi_long = .n(oi[i]), oi_short = NA_integer_,
            source = "live")
      }
    }
  }
}

# ── Vehicle 3: vertical ────────────────────────────────────────────────────
if (tws_ok && !is.na(expiry)) {
  sp <- tryCatch(reticulate::import("tdata_py.spread", delay_load = TRUE), error = function(e) NULL)
  if (!is.null(sp)) for (w in WIDTHS) {
    mny <- max(0.06, w / px + 0.03)
    df <- tryCatch(sp$compute_spread_risk_reward(
      sym = sym, trading_class = tclass, expiration = expiry, current_price = px,
      moneyness_pct = mny, spread_width = as.integer(w), right = right,
      multiplier = as.integer(mult), currency = "USD",
      exchangeSec = "SMART", exchangeOpt = "SMART", force_refresh = TRUE),
      error = function(e) NULL)
    if (is.null(df) || !nrow(df)) next
    if ("spread_type" %in% names(df)) df <- df[df$spread_type == "DEBIT", , drop = FALSE]
    if (!nrow(df)) next
    ls <- .col(df, "long_strike"); ss <- .col(df, "short_strike")
    deb <- abs(.col(df, "net_premium", "max_risk")); mr <- .col(df, "max_reward")
    rr <- .col(df, "reward_risk_ratio"); ps <- .col(df, "prob_success_delta")
    ev <- .col(df, "expected_value"); ed <- .col(df, "edge")
    for (i in seq_len(nrow(df))) {
      if (!is.finite(.n(deb[i])) || deb[i] <= 0) next
      dpw <- deb[i] / (w * mult) * 100
      be_i <- ls[i] + deb[i] / mult
      # debit/width is a PAYOFF test, not a reachability one: a 33%-of-width
      # spread can still have its breakeven beyond the structural target, in
      # which case it cannot profit at the level this tool is aiming for.
      unreachable <- is.finite(target) &&
        (if (identical(direction, "long")) be_i > target else be_i < target)
      rej <- if (deb[i] > budget) "premium_over_budget"
             else if (dpw > MAX_DEBIT_PCT_WIDTH) "debit_pct_width"
             else if (unreachable) "breakeven_beyond_target"
             else ""
      add(vehicle = "vertical", expiry = expiry, dte = dte,
          long_strike = ls[i], short_strike = ss[i], width = w,
          debit = round(deb[i], 2), premium = NA_real_,
          breakeven = round(be_i, 4),
          max_reward = round(.n(mr[i]), 2), payoff_ratio = round(.n(rr[i]), 3),
          debit_pct_width = round(dpw, 2), theta_pct_prem_day = NA_real_,
          pct_of_em10_to_breakeven = if (is.finite(em_hi_abs))
            round((be_i - px) / em_hi_abs * 100, 1) else NA_real_,
          accept = if (nzchar(rej)) "REJECT" else "ACCEPT", reject_reason = rej,
          prob_success = round(.n(ps[i]), 4), edge = round(.n(ed[i]), 4),
          ev = round(.n(ev[i]), 2), net_delta = NA_real_, net_gamma = NA_real_,
          net_theta = NA_real_, bid_ask_pct = NA_real_,
          oi_long = NA_integer_, oi_short = NA_integer_, source = "live")
    }
  }
}

# ── Assemble, order, write ─────────────────────────────────────────────────
COLS <- c("sym","asof","direction","vehicle","expiry","dte","long_strike","short_strike",
  "width","debit","premium","breakeven","max_reward","payoff_ratio","debit_pct_width",
  "theta_pct_prem_day","pct_of_em10_to_breakeven","accept","reject_reason",
  "prob_success","edge","ev","net_delta","net_gamma","net_theta","bid_ask_pct",
  "oi_long","oi_short","source")
df <- do.call(rbind, lapply(rows, function(r)
  as.data.frame(c(list(sym = sym, asof = asof, direction = direction), r),
                stringsAsFactors = FALSE)))
df <- df[, COLS, drop = FALSE]
# ACCEPT first, then payoff_ratio descending within each block.
df <- df[order(df$accept != "ACCEPT",
               -ifelse(is.na(df$payoff_ratio), -Inf, df$payoff_ratio)), , drop = FALSE]

if (is.na(out_path))
  out_path <- file.path(OUT_DIR, sprintf("bot_name_%s_%s.csv",
                                         gsub("[^A-Za-z0-9]", "", sym),
                                         format(Sys.Date(), "%Y%m%d")))
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
utils::write.table(df, out_path, sep = ";", row.names = FALSE, na = "", qmethod = "double")
message(sprintf("Wrote %d structures -> %s", nrow(df), out_path))

# ── Verdict ────────────────────────────────────────────────────────────────
acc <- df[df$accept == "ACCEPT", , drop = FALSE]
cat("\n")
if (nrow(acc)) {
  b <- acc[1, ]
  cat(sprintf("EXECUTE - %s\n", toupper(b$vehicle)))
  cat(sprintf("  %s %s%s  %s%s\n", sym,
      if (is.na(b$long_strike)) "" else format(b$long_strike),
      if (is.na(b$short_strike)) "" else paste0("/", b$short_strike),
      if (is.na(b$expiry)) "stock" else b$expiry,
      if (is.na(b$dte)) "" else sprintf(" (%d DTE)", b$dte)))
  cat(sprintf("  cost %s  payoff %s:1  breakeven %s (%s%% of a %dd move)\n",
      format(b$debit %||% b$premium), format(b$payoff_ratio), format(b$breakeven),
      format(b$pct_of_em10_to_breakeven), EM_DAYS))
  cat(sprintf("  target %.2f (%s)  stop %.2f (%s)  structural asym %.2f:1\n",
      target, lr$target_source, stop_px, lr$stop_source, .n(lr$asym)))
} else {
  # The binding constraint is the one stopping the BEST structure, not the most
  # frequent reason across all of them: the near-miss is what you would have to
  # relax to get a trade.
  cand <- df[nzchar(df$reject_reason) & !is.na(df$payoff_ratio), , drop = FALSE]
  binding <- if (nrow(cand)) cand$reject_reason[which.max(cand$payoff_ratio)]
             else df$reject_reason[nzchar(df$reject_reason)][1]
  cat("NO ACCEPTABLE EXECUTION\n")
  cat(sprintf("  %s - binding constraint: %s\n", sym, binding %||% "no structures returned"))
  for (v in unique(df$vehicle)) {
    sub <- df[df$vehicle == v, , drop = FALSE]
    r <- sub$reject_reason[nzchar(sub$reject_reason)]
    cat(sprintf("  %-9s REJECT  %s\n", v,
        if (length(r)) paste(sort(unique(r)), collapse = ", ") else "no rows"))
  }
  if (!tws_ok) cat("  (TWS not reachable - option vehicles were not evaluated)\n")
}
