# main.R — Swing Scanner Entry Point
#
# Orchestrates: universe → fetch → indicators → sector gate → score → render
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
source(file.path(SCRIPT_DIR, "render_html.R"))

message("=== TOP-DOWN SWING SCANNER v3 (macro-aware) ===")
message("Run date: ", format(Sys.Date()))

PRICE_MAX <- 300
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
dbDisconnect(conn)

if (is.null(macro) || nrow(macro) == 0) {
  message("WARNING: No macro context for today \u2014 run macro_context first!")
  message("Proceeding without macro overlay.")
  macro <- data.frame(bias = "NEUTRE", bias_zone = "ORANGE", vix = NA, s5fi = NA,
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
                                    mm_sectors, macro_bias, get_last)

long_approved  <- names(Filter(function(x) x$long,  sector_ok))
short_approved <- names(Filter(function(x) x$short, sector_ok))
message("Long approved:  ", paste(long_approved,  collapse = ", "))
message("Short approved: ", paste(short_approved, collapse = ", "))

# ── Score stocks ────────────────────────────────────────────────────────────
message("Scoring stocks...")
results <- list()

for (sec in sectors) {
  do_long  <- sec %in% long_approved
  do_short <- sec %in% short_approved
  if (!do_long && !do_short) next

  etf     <- sector_etfs[sec]
  etf_ret <- sector_ok[[sec]]$ret20
  stocks  <- get_sector_stocks(sec)
  mmod    <- sector_ok[[sec]]$macro_mod

  for (tk in stocks) {
    last <- get_last(computed, tk)
    if (is.null(last)) next
    price   <- last$Close
    atr_pct <- round(last$atr14 / price * 100, 2)

    sl_obj <- NULL; long_sig <- "n/a"; long_score <- NA_integer_
    if (do_long) {
      if (price > PRICE_MAX) { long_sig <- "SKIP" }
      else {
        sl_obj <- score_long(last, price, etf_ret)
        long_score <- sl_obj$score + mmod$long_boost
        long_sig <- sig_label(long_score)
      }
    }

    ss_obj <- NULL; short_sig <- "n/a"; short_score <- NA_integer_
    if (do_short) {
      ss_obj <- score_short(last, price, etf_ret)
      short_score <- ss_obj$score + mmod$short_boost
      short_sig <- sig_label(short_score)
    }

    results[[paste0(sec, "_", tk)]] <- data.frame(
      Ticker = tk, Sector = sec, Sector_ETF = etf,
      Price = round(price, 2), ATR_pct = atr_pct,
      Long_Score = ifelse(is.null(sl_obj), NA_integer_, long_score),
      Long_Signal = long_sig,
      Long_Entry = if (!is.null(sl_obj) && price <= PRICE_MAX) round(price * 1.002, 2) else NA_real_,
      Long_Stop = if (!is.null(sl_obj) && price <= PRICE_MAX) round(last$ma50 * 0.99, 2) else NA_real_,
      Long_Target = if (!is.null(sl_obj) && price <= PRICE_MAX) round(price * 1.002 + 2.5 * (price * 1.002 - last$ma50 * 0.99), 2) else NA_real_,
      Short_Score = ifelse(is.null(ss_obj), NA_integer_, short_score),
      Short_Signal = short_sig,
      Short_Entry = if (!is.null(ss_obj)) round(price * 0.998, 2) else NA_real_,
      Short_Stop = if (!is.null(ss_obj)) round(last$ma50 * 1.01, 2) else NA_real_,
      Short_Target = if (!is.null(ss_obj)) round(price * 0.998 - 2.5 * (last$ma50 * 1.01 - price * 0.998), 2) else NA_real_,
      ADX10 = round(last$adx10, 1), RSI14 = round(last$rsi14, 1),
      RSI_slope = round(last$rsi_slope, 1),
      MA50_dist = round(ifelse(!is.na(last$ma50) && last$ma50 > 0, (price - last$ma50) / last$ma50 * 100, 0), 1),
      OBV_rising = !is.na(last$obv_slope) && last$obv_slope > 0,
      UpDn_ratio = round(ifelse(is.na(last$updn_ratio), 0, last$updn_ratio), 2),
      RS_vs_ETF = if (!is.null(sl_obj)) sl_obj$rs else if (!is.null(ss_obj)) ss_obj$rs else NA_real_,
      Macro_Note = mmod$macro_note,
      stringsAsFactors = FALSE)
  }
}

if (length(results) == 0) { message("No results."); quit("no") }

out <- bind_rows(results) |>
  mutate(
    Best_Signal = case_when(
      Long_Signal == "TRADE"  ~ "LONG TRADE",  Short_Signal == "TRADE" ~ "SHORT TRADE",
      Long_Signal == "WATCH"  ~ "LONG WATCH",  Short_Signal == "WATCH" ~ "SHORT WATCH",
      TRUE ~ "SKIP"),
    Sort = case_when(
      Best_Signal == "LONG TRADE" ~ 1, Best_Signal == "SHORT TRADE" ~ 2,
      Best_Signal == "LONG WATCH" ~ 3, Best_Signal == "SHORT WATCH" ~ 4, TRUE ~ 5),
    Best_Score = pmax(ifelse(is.na(Long_Score), 0L, Long_Score),
                      ifelse(is.na(Short_Score), 0L, Short_Score))) |>
  arrange(Sort, desc(Best_Score)) |> select(-Sort, -Best_Score)

message("Stocks scored: ", nrow(out))
message("Signals: ", paste(names(table(out$Best_Signal)), table(out$Best_Signal), sep = ":", collapse = " | "))

# ── CSV output ──────────────────────────────────────────────────────────────
out_dir  <- file.path(SCRIPT_DIR, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
csv_file <- file.path(out_dir, sprintf("swing_scanner_%s.csv", format(Sys.Date(), "%Y%m%d")))
write.csv2(out, csv_file, row.names = FALSE)
message(sprintf("CSV written: %s", csv_file))

# ── HTML output ─────────────────────────────────────────────────────────────
html_file <- render_scanner_html(out, sector_ok, macro_bias, macro, csv_file, out_dir, PRICE_MAX)
message(sprintf("HTML written: %s", html_file))
if (interactive()) utils::browseURL(html_file)
