# main.R — Macro Context Report Entry Point
#
# Orchestrates: fetch → breadth → analyze → render → export to DB
# Run: Rscript RStudies/reports/macro_context/main.R

library(Tdata)
library(dplyr)
library(DBI)
library(logger)
library(Tlogger)

setup_namespace_logging(
  "RStudies",
  console_level = "INFO",
  file_level = "INFO"
)

# Resolve script directory robustly (works with Rscript, source(), and interactive)
.get_script_dir <- function() {
  # Rscript --file=path
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(dirname(sub("^--file=", "", file_arg[1]))))
  # source()
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(normalizePath(dirname(f)))
  }
  # fallback
  getwd()
}
SCRIPT_DIR <- .get_script_dir()

source(file.path(SCRIPT_DIR, "..", "shared", "universe.R"))
source(file.path(SCRIPT_DIR, "fetch.R"))
source(file.path(SCRIPT_DIR, "breadth.R"))
source(file.path(SCRIPT_DIR, "analyze.R"))
source(file.path(SCRIPT_DIR, "events.R"))
source(file.path(SCRIPT_DIR, "render_html.R"))
source(file.path(SCRIPT_DIR, "positioning.R"))
source(file.path(SCRIPT_DIR, "macro_outcomes.R"))
source(file.path(SCRIPT_DIR, "scenarios.R"))

message("=== MACRO CONTEXT REPORT ===")
message("Run date: ", format(Sys.Date(), "%d %B %Y"))

# 1. Load tickers from DB
macro_tickers <- get_macro_tickers()
sector_etf_tickers <- unname(get_sector_etfs())
ALL_TICKERS <- unique(c(macro_tickers, sector_etf_tickers))
message("Tickers: ", length(ALL_TICKERS))

# 2. Fetch data (cached daily)
raw <- fetch_macro_data(ALL_TICKERS)
if (is.null(raw) || nrow(raw) == 0) stop("No data returned")
message("Rows: ", nrow(raw))

# 3. Breadth calculation (cached daily)
breadth <- calculate_breadth()
S5FI_VALUE <- breadth$pct
message(sprintf("Breadth: %.1f%% (%d/%d) in %.1fs",
  S5FI_VALUE, breadth$n_above, breadth$n_valid, breadth$elapsed))

# 4. Analyze
vix_res    <- analyze_vix(raw)
rates_res  <- analyze_rates(raw)
comm_res   <- analyze_commodities(raw)
spy_res    <- analyze_spy(raw)
macro_env  <- build_macro_env(vix_res, rates_res, breadth, comm_res)
mismatches <- analyze_mismatches(raw, macro_env)
synthesis  <- synthesize(vix_res, rates_res, breadth, comm_res, mismatches)

message("Bias: ", synthesis$bias, " (L:", synthesis$sl, " S:", synthesis$ss, ")")

# 5. Evaluate regimes (continuous signals, not binary flags)
conn <- safe_db_connect()
scenario_scores <- run_scenarios(raw, vix_res, rates_res, breadth, comm_res, EVENTS, conn)
dbDisconnect(conn)

# 6. Render HTML
out_dir  <- file.path("C:/Users/aldoh/Documents/NewTrading/reports")
sections <- build_sections(vix_res, rates_res, breadth, spy_res, comm_res, mismatches, synthesis, EVENTS, scenario_scores)
out_file <- render_macro_html(sections, synthesis, breadth, out_dir)
if (interactive()) utils::browseURL(out_file)

# 6. Export macro context to DB for swing_scanner
.today <- as.character(Sys.Date())

.macro_results <- data.frame(
  cache_date    = .today,
  bias          = synthesis$bias,
  bias_zone     = synthesis$bias_zone,
  long_pts      = synthesis$sl,
  short_pts     = synthesis$ss,
  vix           = ifelse(is.na(vix_res$vix), NA_real_, vix_res$vix),
  vix_zone      = vix_res$vix_zone,
  s5fi          = ifelse(is.na(S5FI_VALUE), NA_real_, S5FI_VALUE),
  vix_stress    = isTRUE(macro_env$vix_stress),
  backwardation = isTRUE(macro_env$backwardation),
  rates_high    = isTRUE(macro_env$rates_high),
  curve_inv     = isTRUE(macro_env$curve_inv),
  dxy_strong    = isTRUE(macro_env$dxy_strong),
  dxy_weak      = isTRUE(macro_env$dxy_weak),
  oil_surging   = isTRUE(macro_env$oil_surging),
  gold_rising   = isTRUE(macro_env$gold_rising),
  s5fi_bear     = isTRUE(macro_env$s5fi_bear),
  s5fi_bull     = isTRUE(macro_env$s5fi_bull),
  stringsAsFactors = FALSE
)

.mm_export <- if (length(mismatches) > 0) {
  do.call(rbind, lapply(mismatches, function(m) data.frame(
    cache_date = .today, sector = m$sector, etf = m$etf,
    type = m$type, signal = m$signal, stringsAsFactors = FALSE)))
} else {
  data.frame(cache_date = character(0), sector = character(0), etf = character(0),
             type = character(0), signal = character(0), stringsAsFactors = FALSE)
}

conn <- safe_db_connect()
tryCatch(dbExecute(conn, "DELETE FROM macro_context_results WHERE cache_date = ?",
                   params = list(.today)), error = function(e) NULL)
dbWriteTable(conn, "macro_context_results", .macro_results, append = TRUE)
tryCatch(dbExecute(conn, "DELETE FROM macro_context_mismatches WHERE cache_date = ?",
                   params = list(.today)), error = function(e) NULL)
if (nrow(.mm_export) > 0) dbWriteTable(conn, "macro_context_mismatches", .mm_export, append = TRUE)
dbDisconnect(conn)
message("Macro context exported to DB")
