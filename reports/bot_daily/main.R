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
# The default emits the 12 columns read daily (BOT_READ_DEFAULT); --detail
# emits every field.
# The per-name read itself lives in shared/bot_read.R, shared with /analyze.
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
source(file.path(SH, "bot_read.R"))

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
  if (length(syms_arg)) return(bot_read_ticker_rows(syms_arg))
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
    out <- tryCatch(bot_read_row(r, dir, br), error = function(e) {
      message(sprintf("  %s (%s): %s", r$name, dir, conditionMessage(e))); NULL })
    if (!is.null(out)) rows[[length(rows) + 1]] <- out
  }
  if (i %% 20 == 0) message(sprintf("  ... %d/%d", i, nrow(uni)))
}

if (!length(rows)) { message("No rows produced."); quit(status = 1) }

df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
df <- df[, BOT_READ_COLS, drop = FALSE]

# Reading order: tradable rows first, then asym_em desc, then res_pct_of_em10
# asc. Not a ranking. Vetoed rows keep their level read below the block that
# can be traded today. The edge is asymmetry over many bets, so asymmetry is
# the key, in its bounded form (asym_em, shared/bot_read.R): raw asym is
# unbounded and let unreachable or geometric targets lead the file (TODO 88.3).
# asym_em still runs against trend (asym vs trend count Spearman -0.335), so
# trend_state stays a default column for the reader to weigh.
df <- df[order(-df$tradable,
               -ifelse(is.na(df$asym_em), -Inf, df$asym_em),
               ifelse(is.na(df$res_pct_of_em10), Inf, df$res_pct_of_em10)), , drop = FALSE]

if (is.na(out_path))
  out_path <- file.path(OUT_DIR, sprintf("bot_daily_%s.csv", format(Sys.Date(), "%Y%m%d")))
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
out <- if (detail) df else df[, BOT_READ_DEFAULT, drop = FALSE]
utils::write.table(out, out_path, sep = ";", row.names = FALSE, na = "", qmethod = "double")

message(sprintf("Wrote %d rows x %d cols -> %s", nrow(out), ncol(out), out_path))
stale <- unique(df$name[df$bar_lag > 0])
if (length(stale))
  message(sprintf("Stale last bar (weekdays missing, holidays included) for %d name(s): %s",
                  length(stale), paste(utils::head(stale, 30), collapse = ", ")))
message(sprintf("Tradable: %d of %d  (vetoed: %s)", sum(df$tradable == 1L), nrow(df),
                paste(sprintf("%s %d", names(table(df$veto_reason[df$tradable == 0L])),
                              table(df$veto_reason[df$tradable == 0L])), collapse = ", ")))
print(utils::head(df[, BOT_READ_DEFAULT], 12))
