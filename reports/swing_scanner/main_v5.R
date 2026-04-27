# main_v5.R — Swing Scanner v5 entry point (front-run option flow)
#
# Pipeline: Phase A (Universe) → B (Pull) → C (Cheap) → D (Setup/Chain/R:R)
#         → E (Classification + Display)
#
# Run: Rscript RStudies/reports/swing_scanner/main_v5.R

suppressPackageStartupMessages({
  library(Tdata); library(Tbasics); library(dplyr); library(DBI)
})

.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa) > 0) return(normalizePath(dirname(sub("^--file=", "", fa[1]))))
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)$ofile; if (!is.null(f)) return(normalizePath(dirname(f)))
  }
  getwd()
}
SCRIPT_DIR <- .get_script_dir()

source(file.path(SCRIPT_DIR, "..", "shared", "universe.R"))
source(file.path(SCRIPT_DIR, "fetch.R"))
source(file.path(SCRIPT_DIR, "indicators.R"))
source(file.path(SCRIPT_DIR, "scoring.R"))         # legacy: provides score_breakout
source(file.path(SCRIPT_DIR, "sector_gate.R"))
source(file.path(SCRIPT_DIR, "vol_profile.R"))
source(file.path(SCRIPT_DIR, "universe_filter.R"))
source(file.path(SCRIPT_DIR, "pull_score.R"))
source(file.path(SCRIPT_DIR, "cheap_score.R"))
source(file.path(SCRIPT_DIR, "setup_chain_rr.R"))
source(file.path(SCRIPT_DIR, "final_classify_v5.R"))
source(file.path(SCRIPT_DIR, "render_html_v5.R"))

message("=== SWING SCANNER v5 (front-run option flow) ===")
message("Run date: ", format(Sys.Date()))

RR_MIN <- 0.5  # calibrated from BOT winners 25th pctile (2026-04-27)
.today <- as.character(Sys.Date())

# ── Load macro context (sector RS data for Step B.2) ───────────────────────
conn <- safe_db_connect()
macro <- tryCatch(dbGetQuery(conn,
  "SELECT * FROM macro_context_results WHERE cache_date = ?",
  params = list(.today)), error = function(e) NULL)
mm_sectors <- tryCatch(dbGetQuery(conn,
  "SELECT * FROM macro_context_mismatches WHERE cache_date = ?",
  params = list(.today)), error = function(e) NULL)
if (is.null(macro) || nrow(macro) == 0) {
  message("WARNING: No macro context for today — proceeding without overlay.")
  macro <- data.frame(bias = "NEUTRAL", stringsAsFactors = FALSE)
  mm_sectors <- data.frame(sector = character(0), type = character(0))
}
macro_bias <- macro$bias[1]

# ── Load universe ──────────────────────────────────────────────────────────
sectors     <- get_sectors()
sector_etfs <- get_sector_etfs()
SPY <- "SPY"
all_etfs   <- c(SPY, unname(sector_etfs))
all_stocks <- unique(unlist(lapply(sectors, get_sector_stocks)))
all_tix    <- unique(c(all_etfs, all_stocks))
message(sprintf("Universe: %d tickers", length(all_tix)))

# ── Phase A: rich-options gate ─────────────────────────────────────────────
message("Phase A: rich-options universe gate...")
rich_eval <- evaluate_rich_universe(all_stocks, conn, force_refresh = FALSE)
n_rich <- sum(rich_eval$passes_gate == 1, na.rm = TRUE)
message(sprintf("  Phase A pass: %d / %d", n_rich, length(all_stocks)))

# ── Fetch price data + compute indicators (full universe; cheap) ──────────
message("Fetching price data and computing indicators...")
raw <- fetch_scanner_data(all_tix)
if (is.null(raw) || nrow(raw) == 0) stop("No price data")
computed <- compute_all_indicators(raw, all_tix)

# ── Sector RS rank (B.2 input) ─────────────────────────────────────────────
spy_ret <- { l <- get_last(computed, SPY); if (!is.null(l)) l$ret20 else 0 }
sector_ok <- evaluate_sector_gates(sectors, sector_etfs, computed, spy_ret,
                                    mm_sectors, macro_bias, get_last, macro)
long_sectors <- names(Filter(function(x) x$long, sector_ok))
sector_rs <- sapply(sector_ok[long_sectors], function(x) x$rs)
sector_rank_map <- setNames(rank(-sector_rs, ties.method = "first"), long_sectors)
n_long <- length(long_sectors)
message(sprintf("  LONG-passing sectors (RS-ranked): %d", n_long))

# ── Phase B: Pull score on Phase-A survivors ──────────────────────────────
message("Phase B: Pull screening...")
rich_pass_set <- rich_eval$sym[rich_eval$passes_gate == 1]
trend_cache <- list()
results <- list()

for (sec in sectors) {
  sect_gate <- sector_ok[[sec]]
  etf_ret <- if (!is.null(sect_gate)) sect_gate$ret20 else 0
  stocks <- intersect(get_sector_stocks(sec), rich_pass_set)
  for (tk in stocks) {
    last <- get_last(computed, tk)
    if (is.null(last)) next
    price <- last$Close

    trend_res <- if (!is.null(trend_cache[[tk]])) trend_cache[[tk]]
                 else tryCatch(Tdata::isTrendContinuation(tk),
                               error = function(e) list(passes = NA, rs_vs_bench_3m = NA))
    trend_cache[[tk]] <- trend_res

    sec_rank <- sector_rank_map[sec]
    pull <- score_pull(last, price, etf_ret,
                       sector_rs_rank = if (!is.na(sec_rank)) sec_rank else 99,
                       n_long_sectors = n_long,
                       sector_long_gate = isTRUE(sect_gate$long),
                       sector_short_gate = isTRUE(sect_gate$short),
                       trend_passes = trend_res$passes,
                       rs_3m = trend_res$rs_vs_bench_3m)
    results[[paste0(sec, "_", tk)]] <- list(
      sym = tk, sector = sec, price = price, last = last,
      rich_pass = TRUE, pull = pull)
  }
}
# Add Phase-A failures as SKIP rows (so funnel + filter chips work)
for (tk in setdiff(all_stocks, rich_pass_set)) {
  results[[paste0("X_", tk)]] <- list(
    sym = tk, sector = NA_character_, price = NA_real_, last = NULL,
    rich_pass = FALSE, pull = NULL)
}

n_b_pass <- sum(sapply(results, function(r)
  isTRUE(r$rich_pass) && isTRUE(r$pull$passes)))
message(sprintf("  Phase B pass: %d", n_b_pass))

# ── Phase C: Cheap score on Phase-B survivors ─────────────────────────────
message("Phase C: Cheap screening (option fetch)...")
phase_b_syms <- as.character(sapply(Filter(function(r)
  isTRUE(r$rich_pass) && isTRUE(r$pull$passes), results), function(r) r$sym))

if (length(phase_b_syms) > 0) {
  vol_data <- load_vol_profiles(phase_b_syms, conn)
  gate3 <- evaluate_gate3(vol_data, phase_b_syms)
} else {
  message("  No Phase B survivors — skipping Phase C")
  gate3 <- data.frame(Ticker = character(0), IV30 = numeric(0),
                      IVP_2y = numeric(0), VRP = numeric(0),
                      stringsAsFactors = FALSE)
}

# Sector cross-sectional median IVP_2y per sector (rich universe)
sector_ivp_median <- list()
for (sec in unique(unlist(lapply(results, `[[`, "sector"))[!is.na(unique(unlist(lapply(results, `[[`, "sector"))))])) {
  sec_syms <- sapply(Filter(function(r) !is.na(r$sector) && r$sector == sec,
                            results), function(r) r$sym)
  rows <- gate3[gate3$Ticker %in% sec_syms & !is.na(gate3$IVP_2y), ]
  sector_ivp_median[[sec]] <- if (nrow(rows) > 0) median(rows$IVP_2y) else NA
}

for (key in names(results)) {
  r <- results[[key]]
  if (!isTRUE(r$rich_pass) || !isTRUE(r$pull$passes)) next
  vol_row <- gate3[gate3$Ticker == r$sym, ][1, , drop = FALSE]
  skew_history <- tryCatch(dbGetQuery(conn,
    "SELECT cache_date, skew_25d FROM option_skew_history
     WHERE sym = ? AND skew_25d IS NOT NULL
       AND cache_date >= ?",
    params = list(r$sym, format(Sys.Date() - 365, "%Y-%m-%d"))),
    error = function(e) NULL)
  cheap <- score_cheap(vol_row, skew_history,
                       sector_iv_median = sector_ivp_median[[r$sector]],
                       pull_direction = r$pull$pull_direction)
  results[[key]]$cheap <- cheap
  results[[key]]$vol_row <- vol_row
}
n_c_pass <- sum(sapply(results, function(r) {
  isTRUE(r$rich_pass) && isTRUE(r$pull$passes) && !is.null(r$cheap) && isTRUE(r$cheap$passes)
}))
message(sprintf("  Phase C pass: %d", n_c_pass))

# ── Phase D: Setup, Chain, R:R on Phase-C survivors ────────────────────────
message("Phase D: Setup, Chain, R:R...")
for (key in names(results)) {
  r <- results[[key]]
  if (!isTRUE(r$rich_pass) || !isTRUE(r$pull$passes) ||
      is.null(r$cheap) || !isTRUE(r$cheap$passes)) next

  # ATM bid/ask % (placeholder — populated by daily option fetch)
  atm_pct <- NA_real_

  vex <- pick_vehicle_expiry(r$price, r$cheap$cheap_score, r$pull$stage, atm_pct)

  # Compute structural target from this ticker's price history
  hist <- raw[raw$ticker == r$sym, ] |> dplyr::arrange(date)
  st <- compute_structural_target(r$price, hist$Close, hist$High)

  # Pick strike: for call → first OTM strike >= price (rounded to nearest $5)
  # For spread → long ATM, short at spot_target_low (rounded to nearest $5)
  strike_long <- NA_real_; strike_short <- NA_real_
  if (vex$vehicle == "call") {
    strike_long <- ceiling(r$price / 5) * 5
  } else if (vex$vehicle == "spread") {
    strike_long <- round(r$price / 5) * 5
    strike_short <- if (!is.na(st$spot_target_low))
      round(st$spot_target_low / 5) * 5 else NA_real_
  }

  # Target expiry = nearest available with DTE in target_dte ± 7
  exp_dt <- format(Sys.Date() + vex$target_dte, "%Y%m%d")

  # Chain walk (reads from option_chain_oi_history)
  chain <- if (!is.na(st$spot_target_low) && !is.na(st$spot_target_high))
    walk_chain_oi(r$sym, exp_dt, st$spot_target_low, st$spot_target_high,
                  r$price, conn)
  else
    list(oi_cap_call = NA_real_, oi_cap_call_magnitude = NA_integer_,
         oi_cap_put = NA_real_, oi_cap_put_magnitude = NA_integer_,
         oi_concentration_pct = NA_real_, total_chain_oi = NA_integer_,
         chain_state = "NO DATA",
         effective_target = st$spot_target_low,
         chain_walk_status = "FAILED")

  # Entry premium estimate: BS at current spot/strike with current IV (placeholder
  # for live mid). Using IV30 from gate3 as proxy.
  iv_now <- if (!is.null(r$vol_row) && !is.na(r$vol_row$IV30))
    r$vol_row$IV30 / 100 else 0.30

  entry_prem <- if (vex$vehicle == "call" && !is.na(strike_long)) {
    tryCatch(Tbasics::getOptPrice(type = "Call", S = r$price, K = strike_long,
                                  r = 0.045, DTE = vex$target_dte, sig = iv_now),
             error = function(e) NA_real_)
  } else if (vex$vehicle == "spread" && !is.na(strike_long) && !is.na(strike_short)) {
    long_p <- tryCatch(Tbasics::getOptPrice(type = "Call", S = r$price, K = strike_long,
                                            r = 0.045, DTE = vex$target_dte, sig = iv_now),
                       error = function(e) NA_real_)
    short_p <- tryCatch(Tbasics::getOptPrice(type = "Call", S = r$price, K = strike_short,
                                             r = 0.045, DTE = vex$target_dte, sig = iv_now),
                        error = function(e) NA_real_)
    if (!is.na(long_p) && !is.na(short_p)) max(long_p - short_p, 0.05) else NA_real_
  } else if (vex$vehicle == "stock") {
    if (!is.na(r$last$ma50)) max(r$price - r$last$ma50 * 0.99, 0.5) else r$price * 0.05
  } else NA_real_

  rr_obj <- if (!is.na(entry_prem) && !is.na(chain$effective_target)) {
    compute_rr_entry(vex$vehicle, strike_long, exp_dt, r$price,
                     chain$effective_target, iv_now, entry_prem,
                     spread_short_strike = strike_short,
                     spot_target_high = st$spot_target_high,
                     rr_min = RR_MIN)
  } else {
    list(rr = NA_real_, entry_floor = NA_real_, entry_ceiling = NA_real_,
         headroom_band = "n/a", reward = NA_real_)
  }
  entry_state <- classify_entry_state(rr_obj$entry_floor, rr_obj$entry_ceiling,
                                      chain$chain_walk_status)

  results[[key]]$vex <- vex
  results[[key]]$st <- st
  results[[key]]$strike_long <- strike_long
  results[[key]]$strike_short <- strike_short
  results[[key]]$expiry <- exp_dt
  results[[key]]$chain <- chain
  results[[key]]$rr_obj <- rr_obj
  results[[key]]$entry_state <- entry_state
}

# ── Build flat data.frame for classification + render ──────────────────────
df <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    sym = r$sym,
    sector = ifelse(is.na(r$sector), "", r$sector),
    rich_pass = isTRUE(r$rich_pass),
    pull_pass = !is.null(r$pull) && isTRUE(r$pull$passes),
    cheap_pass = !is.null(r$cheap) && isTRUE(r$cheap$passes),
    stage = if (!is.null(r$pull)) r$pull$stage else NA_character_,
    pull_score = if (!is.null(r$pull)) r$pull$pull_score else NA_integer_,
    pull_direction = if (!is.null(r$pull)) r$pull$pull_direction else NA_character_,
    sector_rs_rank = if (!is.null(r$pull)) r$pull$sector_rs_rank else NA_integer_,
    stage_pts = if (!is.null(r$pull)) r$pull$stage_pts else NA_integer_,
    sector_pts = if (!is.null(r$pull)) r$pull$sector_pts else NA_integer_,
    footprint_pts = if (!is.null(r$pull)) r$pull$footprint_pts else NA_integer_,
    cheap_score = if (!is.null(r$cheap)) r$cheap$cheap_score else NA_integer_,
    cheap_side  = if (!is.null(r$cheap)) r$cheap$cheap_side else NA_character_,
    ivp_used = if (!is.null(r$cheap)) r$cheap$ivp_used else NA_real_,
    ivp_2y = if (!is.null(r$cheap)) r$cheap$ivp_2y else NA_real_,
    vrp    = if (!is.null(r$cheap)) r$cheap$vrp    else NA_real_,
    vehicle = if (!is.null(r$vex)) r$vex$vehicle else NA_character_,
    strike  = if (!is.null(r$strike_long)) r$strike_long else NA_real_,
    expiry  = if (!is.null(r$expiry)) r$expiry else NA_character_,
    spot_target_low  = if (!is.null(r$st)) r$st$spot_target_low else NA_real_,
    spot_target_high = if (!is.null(r$st)) r$st$spot_target_high else NA_real_,
    targets_agreeing = if (!is.null(r$st)) r$st$targets_agreeing else NA_integer_,
    fib_confirms     = if (!is.null(r$st)) r$st$fib_confirms else NA,
    oi_cap_call = if (!is.null(r$chain)) r$chain$oi_cap_call else NA_real_,
    oi_cap_put  = if (!is.null(r$chain)) r$chain$oi_cap_put else NA_real_,
    oi_concentration_pct = if (!is.null(r$chain)) r$chain$oi_concentration_pct else NA_real_,
    chain_state = if (!is.null(r$chain)) r$chain$chain_state else NA_character_,
    crowded_flag = NA,
    effective_target = if (!is.null(r$chain)) r$chain$effective_target else NA_real_,
    rr            = if (!is.null(r$rr_obj)) r$rr_obj$rr else NA_real_,
    entry_floor   = if (!is.null(r$rr_obj)) r$rr_obj$entry_floor else NA_real_,
    entry_ceiling = if (!is.null(r$rr_obj)) r$rr_obj$entry_ceiling else NA_real_,
    headroom_band = if (!is.null(r$rr_obj)) r$rr_obj$headroom_band else NA_character_,
    entry_state   = if (!is.null(r$entry_state)) r$entry_state else NA_character_,
    stringsAsFactors = FALSE)
}))
rownames(df) <- NULL

# ── Phase E: classify ──────────────────────────────────────────────────────
message("Phase E: classification...")
df <- classify_final(df, rr_min = RR_MIN)

n_top <- sum(df$rank == "TOP PICK"); n_watch <- sum(df$rank == "WATCH")
message(sprintf("  TOP PICK: %d  WATCH: %d  SKIP: %d",
                n_top, n_watch, sum(df$rank == "SKIP")))

# ── Persist results to scanner_results_v5 ──────────────────────────────────
keep_cols <- intersect(c("sym","sector","stage","pull_score","pull_direction",
  "sector_rs_rank","cheap_score","cheap_side","ivp_2y","vrp","vehicle",
  "strike","expiry","spot_target_low","spot_target_high","targets_agreeing",
  "fib_confirms","oi_cap_call","oi_cap_put","chain_state","crowded_flag",
  "effective_target","entry_floor","entry_ceiling","entry_state","rr",
  "headroom_band","rank","phase_of_drop"), names(df))
to_persist <- df[, keep_cols]
to_persist$cache_date <- .today
to_persist$fib_confirms <- as.integer(to_persist$fib_confirms)
to_persist$crowded_flag <- as.integer(to_persist$crowded_flag)
tryCatch({
  dbExecute(conn, "DELETE FROM scanner_results_v5 WHERE cache_date = ?",
            params = list(.today))
  dbWriteTable(conn, "scanner_results_v5", to_persist,
               append = TRUE, row.names = FALSE)
}, error = function(e) message("scanner_results_v5 persist failed: ", e$message))
dbDisconnect(conn)

# ── Funnel for HTML ─────────────────────────────────────────────────────────
funnel <- c(
  "Universe"  = nrow(df),
  "Pull"      = sum(df$pull_pass),
  "Cheap"     = sum(df$cheap_pass),
  "Setup R:R" = sum(df$rank %in% c("TOP PICK", "WATCH")),
  "TOP PICK"  = n_top
)

# ── Render ──────────────────────────────────────────────────────────────────
out_dir <- "C:/Users/aldoh/Documents/NewTrading/reports"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
csv_file <- file.path(out_dir, sprintf("swing_scanner_v5_%s.csv",
                                       format(Sys.Date(), "%Y%m%d")))
write.csv2(df, csv_file, row.names = FALSE)
message(sprintf("CSV written: %s", csv_file))

html_file <- render_scanner_html_v5(df, funnel, out_dir,
                                    run_date = Sys.Date(), rr_min = RR_MIN)
message(sprintf("HTML written: %s", html_file))
if (interactive()) utils::browseURL(html_file)
