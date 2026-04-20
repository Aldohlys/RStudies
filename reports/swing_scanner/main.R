# main.R — Swing Scanner Entry Point
#
# Orchestrates: universe → fetch → indicators → sector gate → score → gate3 → history → filter → render
# Run: Rscript RStudies/reports/swing_scanner/main.R

library(Tdata)
library(dplyr)
library(DBI)

# Resolve script directory robustly (works with Rscript, source(), and interactive)
.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(dirname(sub("^--file=", "", file_arg[1]))))
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(normalizePath(dirname(f)))
  }
  getwd()
}
SCRIPT_DIR <- .get_script_dir()

source(file.path(SCRIPT_DIR, "..", "shared", "universe.R"))
source(file.path(SCRIPT_DIR, "fetch.R"))
source(file.path(SCRIPT_DIR, "indicators.R"))
source(file.path(SCRIPT_DIR, "scoring.R"))
source(file.path(SCRIPT_DIR, "sector_gate.R"))
source(file.path(SCRIPT_DIR, "vol_profile.R"))
source(file.path(SCRIPT_DIR, "history.R"))
source(file.path(SCRIPT_DIR, "final_filter.R"))
source(file.path(SCRIPT_DIR, "render_html.R"))

# Source scenarios.R from macro_context for get_scenario_sector_map()
source(file.path(SCRIPT_DIR, "..", "macro_context", "scenarios.R"))

message("=== TOP-DOWN SWING SCANNER v4 (macro + vol + scenarios) ===")
message("Run date: ", format(Sys.Date()))

PRICE_MAX <- 500
.today <- as.character(Sys.Date())

# ── Load macro context from DB ──────────────────────────────────────────────
conn <- safe_db_connect()
macro <- tryCatch(
  dbGetQuery(conn, "SELECT * FROM macro_context_results WHERE cache_date = ?",
             params = list(.today)),
  error = function(e) NULL)
mm_sectors <- tryCatch(
  dbGetQuery(conn, "SELECT * FROM macro_context_mismatches WHERE cache_date = ?",
             params = list(.today)),
  error = function(e) NULL)

# Load regime scores from DB
scenario_scores <- tryCatch(
  dbGetQuery(conn, "SELECT * FROM macro_regimes WHERE cache_date = ?",
             params = list(.today)),
  error = function(e) NULL)

# Load previous scanner results for transitions/persistence
prev_results <- load_previous_results(conn)

# Load active alerts
active_alerts <- load_active_alerts(conn)

dbDisconnect(conn)

if (is.null(macro) || nrow(macro) == 0) {
  message("WARNING: No macro context for today — run macro_context first!")
  message("Proceeding without macro overlay.")
  macro <- data.frame(bias = "NEUTRAL", bias_zone = "ORANGE", vix = NA, s5fi = NA,
    vix_stress = FALSE, backwardation = FALSE, rates_high = FALSE,
    s5fi_bear = FALSE, s5fi_bull = FALSE, stringsAsFactors = FALSE)
  mm_sectors <- data.frame(sector = character(0), type = character(0), stringsAsFactors = FALSE)
}
macro_bias <- macro$bias[1]
message("Macro bias: ", macro_bias,
  " | VIX:", ifelse(is.na(macro$vix[1]), "n/a", round(macro$vix[1], 1)),
  " | S5FI:", ifelse(is.na(macro$s5fi[1]), "n/a", round(macro$s5fi[1], 1)))

# ── Load universe from DB ──────────────────────────────────────────────────
sectors     <- get_sectors()
sector_etfs <- get_sector_etfs()
SPY <- "SPY"

all_etfs   <- c(SPY, unname(sector_etfs))
all_stocks <- unique(unlist(lapply(sectors, get_sector_stocks)))
all_tix    <- unique(c(all_etfs, all_stocks))
message("Universe: ", length(all_tix), " tickers | ", length(all_etfs), " ETFs + ", length(all_stocks), " stocks")

# ── Fetch data ──────────────────────────────────────────────────────────────
raw <- fetch_scanner_data(all_tix)
if (is.null(raw) || nrow(raw) == 0) stop("No data returned")
message("Rows: ", nrow(raw))

# ── Compute indicators ─────────────────────────────────────────────────────
message("Computing indicators...")
computed <- compute_all_indicators(raw, all_tix)

# ── Sector gate ─────────────────────────────────────────────────────────────
spy_ret <- { l <- get_last(computed, SPY); if (!is.null(l)) l$ret20 else 0 }

sector_ok <- evaluate_sector_gates(sectors, sector_etfs, computed, spy_ret,
                                    mm_sectors, macro_bias, get_last, macro)

long_approved  <- names(Filter(function(x) x$long,  sector_ok))
short_approved <- names(Filter(function(x) x$short, sector_ok))
message("Long approved:  ", paste(long_approved,  collapse = ", "))
message("Short approved: ", paste(short_approved, collapse = ", "))

# ── Score stocks ────────────────────────────────────────────────────────────
message("Scoring stocks...")
results <- list()

for (sec in sectors) {
  sect_gate <- sector_ok[[sec]]
  etf       <- sector_etfs[sec]
  etf_ret   <- sect_gate$ret20
  stocks    <- get_sector_stocks(sec)
  mmod      <- sect_gate$macro_mod

  # Sector gate flags — informational, do NOT block scoring

  sg_long  <- sect_gate$long
  sg_short <- sect_gate$short

  for (tk in stocks) {
    last <- get_last(computed, tk)
    if (is.null(last)) next
    price   <- last$Close
    atr_pct <- round(last$atr14 / price * 100, 2)

    # Always score both directions — sector gate does not block
    sl_obj <- NULL; long_sig <- "n/a"; long_score <- NA_integer_
    if (price > PRICE_MAX) {
      long_sig <- "SKIP"
    } else {
      sl_obj <- score_long(last, price, etf_ret)
      long_score <- sl_obj$score + mmod$long_boost
      long_sig <- sig_label(long_score)
    }

    ss_obj <- score_short(last, price, etf_ret)
    short_score <- ss_obj$score + mmod$short_boost
    short_sig   <- sig_label(short_score)

    # RS_vs_ETF always available from long score (both compute the same RS)
    rs_val <- if (!is.null(sl_obj)) sl_obj$rs else ss_obj$rs

    # ── BOT breakout score ─────────────────────────────────────────────
    bot_obj <- NULL; bot_score <- NA_integer_
    if (price <= PRICE_MAX) {
      bot_obj <- score_breakout(last, price, etf_ret)
      bot_score <- bot_obj$score
    }

    results[[paste0(sec, "_", tk)]] <- data.frame(
      Ticker = tk, Sector = sec, Sector_ETF = etf,
      Price = round(price, 2), ATR_pct = atr_pct,
      Long_Score = ifelse(is.null(sl_obj), NA_integer_, long_score),
      Long_Signal = long_sig,
      Long_Entry = if (!is.null(sl_obj) && price <= PRICE_MAX) round(price * 1.002, 2) else NA_real_,
      Long_Stop = if (!is.null(sl_obj) && price <= PRICE_MAX) round(last$ma50 * 0.99, 2) else NA_real_,
      Long_Target = if (!is.null(sl_obj) && price <= PRICE_MAX) round(price * 1.002 + 2.5 * (price * 1.002 - last$ma50 * 0.99), 2) else NA_real_,
      Short_Score = short_score,
      Short_Signal = short_sig,
      Short_Entry = round(price * 0.998, 2),
      Short_Stop = round(last$ma50 * 1.01, 2),
      Short_Target = round(price * 0.998 - 2.5 * (last$ma50 * 1.01 - price * 0.998), 2),
      BOT_Score = bot_score,
      BOT_Setup = if (!is.null(bot_obj)) bot_obj$setup else NA_integer_,
      BOT_Breakout = if (!is.null(bot_obj)) bot_obj$breakout else NA_integer_,
      BOT_Squeeze = if (!is.null(bot_obj)) bot_obj$squeeze else NA_real_,
      BOT_VolDec = if (!is.null(bot_obj)) bot_obj$vol_dec else NA_real_,
      BOT_VolSurge = if (!is.null(bot_obj)) bot_obj$vol_surge else NA_real_,
      BOT_Flags = if (!is.null(bot_obj)) bot_obj$flags else "",
      ADX10 = round(last$adx10, 1), RSI14 = round(last$rsi14, 1),
      RSI_slope = round(last$rsi_slope, 1),
      MA50_dist = round(ifelse(!is.na(last$ma50) && last$ma50 > 0, (price - last$ma50) / last$ma50 * 100, 0), 1),
      OBV_rising = !is.na(last$obv_slope) && last$obv_slope > 0,
      UpDn_ratio = round(ifelse(is.na(last$updn_ratio), 0, last$updn_ratio), 2),
      RS_vs_ETF = rs_val,
      Macro_Note = mmod$macro_note,
      Sector_Long_Gate = sg_long,
      Sector_Short_Gate = sg_short,
      stringsAsFactors = FALSE)
  }
}

if (length(results) == 0) { message("No results."); quit("no") }

out <- bind_rows(results)

# Best_Signal: prefer the direction where sector gate is open + highest score
# Both directions always have scores now — use sector gate to pick the actionable one
out <- out |>
  mutate(
    # Effective signal: score-based signal filtered by sector gate
    Long_Eff  = ifelse(Sector_Long_Gate  & Long_Signal  %in% c("TRADE","WATCH"), Long_Signal,  "SKIP"),
    Short_Eff = ifelse(Sector_Short_Gate & Short_Signal %in% c("TRADE","WATCH"), Short_Signal, "SKIP"),
    Best_Signal = case_when(
      Long_Eff == "TRADE"  ~ "LONG TRADE",  Short_Eff == "TRADE" ~ "SHORT TRADE",
      Long_Eff == "WATCH"  ~ "LONG WATCH",  Short_Eff == "WATCH" ~ "SHORT WATCH",
      TRUE ~ "SKIP"),
    Sort = case_when(
      Best_Signal == "LONG TRADE" ~ 1, Best_Signal == "SHORT TRADE" ~ 2,
      Best_Signal == "LONG WATCH" ~ 3, Best_Signal == "SHORT WATCH" ~ 4, TRUE ~ 5),
    Best_Score = pmax(ifelse(is.na(Long_Score), 0L, Long_Score),
                      ifelse(is.na(Short_Score), 0L, Short_Score))) |>
  arrange(Sort, desc(Best_Score)) |> select(-Sort, -Best_Score, -Long_Eff, -Short_Eff)

message("Stocks scored: ", nrow(out))
message("Signals: ", paste(names(table(out$Best_Signal)), table(out$Best_Signal), sep = ":", collapse = " | "))

# ── Earnings enrichment ──────────────────────────────────────────────────
# Refresh stale earnings dates (usually touches 0-5 tickers whose date passed),
# then left-join NextEarnings into the scored table and derive days-until.
tryCatch(Tdata::updateStaleEarnings(), error = function(e)
  message("updateStaleEarnings failed: ", e$message, " — continuing without refresh"))

conn <- safe_db_connect()
earnings <- DBI::dbGetQuery(conn,
  "SELECT Name AS Ticker, NextEarnings FROM Tickers WHERE NextEarnings IS NOT NULL")
dbDisconnect(conn)

out <- out |>
  left_join(earnings, by = "Ticker") |>
  mutate(
    EarningsInDays = ifelse(
      !is.na(NextEarnings),
      as.integer(as.Date(NextEarnings, format = "%Y%m%d") - Sys.Date()),
      NA_integer_))

n_soon <- sum(!is.na(out$EarningsInDays) & out$EarningsInDays <= 14, na.rm = TRUE)
message("Earnings within 14 days: ", n_soon, " ticker(s)")

# ── Gate 3: Vol Profile enrichment ────────────────────────────────────────
message("Loading vol profiles (Optionality gate)...")
conn <- safe_db_connect()
vol_data <- load_vol_profiles(out$Ticker, conn)
gate3 <- evaluate_gate3(vol_data, out$Ticker)
check_tws <- flag_missing(gate3)

# Merge optionality columns into output
out <- merge(out, gate3[, c("Ticker", "IV30", "RV30", "IVP", "RVP", "VRP", "Optionality", "TermStr", "Vol_Date")],
             by = "Ticker", all.x = TRUE)

# ── History: save results, compute transitions + persistence ──────────────
message("Processing scanner history...")
save_scanner_results(out, conn)
transitions <- compute_transitions(out, prev_results)
persistence <- compute_persistence(out, prev_results)
dbDisconnect(conn)

if (!is.null(transitions)) {
  n_trans <- table(transitions$transition_type)
  message("Transitions: ", paste(names(n_trans), n_trans, sep = ":", collapse = " | "))
}

# ── Final Filter: composite ranking, top picks, watch cap ────────────────
message("Applying final filter...")
out <- final_rank(out, macro, scenario_scores, persistence, max_trades = 3, max_watch = 10)

n_top   <- sum(out$Rank == "TOP PICK", na.rm = TRUE)
n_watch <- sum(out$Best_Signal %in% c("LONG WATCH", "SHORT WATCH") |
               out$Best_Signal %in% c("LONG TRADE", "SHORT TRADE"), na.rm = TRUE)
message(sprintf("Final: %d TOP PICK, %d WATCH/TRADE displayed", n_top, n_watch))

# ── CSV output (full including SKIP for audit) ─────────────────────────────
out_dir  <- file.path("C:/Users/aldoh/Documents/NewTrading/reports")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
csv_file <- file.path(out_dir, sprintf("swing_scanner_%s.csv", format(Sys.Date(), "%Y%m%d")))
write.csv2(out, csv_file, row.names = FALSE)
message(sprintf("CSV written: %s", csv_file))

# ── HTML output (filtered: no SKIP) ────────────────────────────────────────
html_file <- render_scanner_html(out, sector_ok, macro_bias, macro, csv_file, out_dir,
                                  PRICE_MAX, transitions, active_alerts)
message(sprintf("HTML written: %s", html_file))
if (interactive()) utils::browseURL(html_file)
