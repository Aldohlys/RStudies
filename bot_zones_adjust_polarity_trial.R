# bot_zones_adjust_polarity_trial.R — how many BOT_daily rows change when the
# level engine (a) reads dividend-adjusted OHLC and (b) lets a zone wholly
# beyond spot play the role of the side it sits on (polarity).
#
# For every Tickers.BOT_Eligible name, long, the level read is computed four
# ways on the same fetch: baseline (raw, no polarity), adjusted only,
# polarity only, both. Reported: rows whose target / stop / target_source /
# asym change against the baseline.
#
# Run from the RStudies project root:
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" bot_zones_adjust_polarity_trial.R [SYM ...]

suppressPackageStartupMessages({ library(Tdata); library(DBI) })
for (f in c("indicators.R", "weekly.R", "zones.R", "gates.R", "bot_read.R"))
  source(file.path("reports", "shared", f))

syms <- commandArgs(trailingOnly = TRUE)
if (!length(syms)) {
  conn <- Tdata::safe_db_connect()
  syms <- dbGetQuery(conn, "SELECT Name FROM Tickers WHERE BOT_Eligible = 1")$Name
  dbDisconnect(conn)
}
tk <- bot_read_ticker_rows(syms)

read_one <- function(d, polarity) {
  zd <- d[as.Date(d$date) >= Sys.Date() - round(BOT_ZONE_YEARS * 365), , drop = FALSE]
  zd <- if (nrow(zd) >= 130) zd else d
  atr <- tail(d$atr14, 1)
  lr <- level_read(zd, atr, cfg = modifyList(ZONE_DEFAULTS, list(polarity = polarity)))
  data.frame(target = lr$target, target_source = lr$target_source,
             stop = lr$stop_px, stop_source = lr$stop_source, asym = lr$asym)
}

out <- list()
for (i in seq_len(nrow(tk))) {
  raw <- tryCatch(bot_fetch_daily(tk$yahoo[i], adjusted = FALSE), error = function(e) NULL)
  adj <- tryCatch(bot_fetch_daily(tk$yahoo[i], adjusted = TRUE), error = function(e) NULL)
  if (is.null(raw) || is.null(adj)) next
  f <- tail(raw$Close, 520) / tail(adj$Close, 520)
  v <- list(base = read_one(raw, FALSE), adj = read_one(adj, FALSE),
            pol = read_one(raw, TRUE), both = read_one(adj, TRUE))
  row <- data.frame(name = tk$name[i], px = tail(raw$Close, 1),
                    adj_span = round(max(f, na.rm = TRUE) / min(f, na.rm = TRUE) - 1, 4))
  for (k in names(v)) { x <- v[[k]]; names(x) <- paste0(k, "_", names(x)); row <- cbind(row, x) }
  out[[length(out) + 1]] <- row
  if (i %% 25 == 0) message(i, "/", nrow(tk))
}
R <- do.call(rbind, out)
dir.create("output", showWarnings = FALSE)
saveRDS(R, "output/bot_zones_adjust_polarity_result.rds")

chg <- function(a, b) sum(abs(a - b) > 1e-6 * pmax(1, abs(a)), na.rm = TRUE) + sum(xor(is.na(a), is.na(b)))
cat(sprintf("\n%d names, long\n", nrow(R)))
cat(sprintf("adjustment span over the 2-year zone window > 1%%: %d names, > 5%%: %d\n",
            sum(R$adj_span > 0.01), sum(R$adj_span > 0.05)))
for (k in c("adj", "pol", "both")) {
  cat(sprintf("%-5s target changed %3d  stop changed %3d  target_source changed %3d",
              k, chg(R$base_target, R[[paste0(k, "_target")]]), chg(R$base_stop, R[[paste0(k, "_stop")]]),
              sum(R$base_target_source != R[[paste0(k, "_target_source")]])))
  ts <- table(R[[paste0(k, "_target_source")]]); ss <- table(R[[paste0(k, "_stop_source")]])
  cat("   targets:", paste(names(ts), ts, collapse = ", "), " | stops:", paste(names(ss), ss, collapse = ", "), "\n")
}
cat("\nbaseline target sources:", paste(names(table(R$base_target_source)), table(R$base_target_source), collapse = ", "), "\n")
cat(sprintf("median asym  base %.2f  adj %.2f  pol %.2f  both %.2f\n",
            median(R$base_asym, na.rm = TRUE), median(R$adj_asym, na.rm = TRUE),
            median(R$pol_asym, na.rm = TRUE), median(R$both_asym, na.rm = TRUE)))
big <- R[order(-abs(R$both_target / R$base_target - 1)), ]
cat("\nLargest target moves (both vs base):\n")
print(head(big[, c("name", "px", "adj_span", "base_target", "base_target_source", "both_target",
                   "both_target_source", "base_stop", "both_stop", "both_stop_source")], 15), row.names = FALSE)
