# bot_target_engines_trial.R — TODO 93.6: how far apart are the two target
# engines on the same names and the same day?
#
#   level_read()                (BOT_daily; shared/zones.R)  zones, polarity,
#       Fibonacci fallback, capped at the 10-session expected move; stop held
#       between 1.0 and 2.5 ATR
#   compute_structural_target() (/analyze Phase D; shared/setup_chain_rr.R)
#       prior swing highs, 52-week high, round numbers, Fibonacci overlay;
#       the stock row uses a fixed 5% stop
#
# Both run on the same dividend-adjusted daily series (bot_fetch_daily), long.
# Reported: target distances in ATR and in EM10 units, how often each engine
# lies beyond the expected move, agreement between the two, and the stop gap.
#
# Run from the RStudies project root:
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" bot_target_engines_trial.R [SYM ...]

suppressPackageStartupMessages({ library(Tdata); library(DBI) })
for (f in c("indicators.R", "weekly.R", "zones.R", "gates.R", "market_calendar.R", "bot_read.R", "setup_chain_rr.R"))
  source(file.path("reports", "shared", f))

syms <- commandArgs(trailingOnly = TRUE)
if (!length(syms)) {
  conn <- Tdata::safe_db_connect()
  syms <- dbGetQuery(conn, "SELECT Name FROM Tickers WHERE BOT_Eligible = 1")$Name
  dbDisconnect(conn)
}
tk <- bot_read_ticker_rows(syms)

out <- list()
for (i in seq_len(nrow(tk))) {
  d <- tryCatch(bot_fetch_daily(tk$yahoo[i]), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 300) next
  px <- tail(d$Close, 1); atr <- tail(d$atr14, 1)
  c_hi <- suppressWarnings(as.numeric(tk$coef_hi[i]))
  em_abs <- if (is.finite(c_hi)) px * (atr / px * 100) * sqrt(10) * c_hi / 100 else NA_real_
  zd <- d[as.Date(d$date) >= Sys.Date() - round(BOT_ZONE_YEARS * 365), , drop = FALSE]
  lr <- tryCatch(level_read(if (nrow(zd) >= 130) zd else d, atr, em_abs), error = function(e) NULL)
  h <- tail(d, 300)
  st <- tryCatch(compute_structural_target(px, h$Close, h$High, hist_low = h$Low, direction = "long"),
                 error = function(e) NULL)
  if (is.null(lr) || is.null(st)) next
  out[[length(out) + 1]] <- data.frame(
    name = tk$name[i], px = px, atr = atr, em_abs = em_abs,
    t_bot = lr$target, t_bot_src = lr$target_source, stop_bot = lr$stop_px,
    t_struct = st$spot_target_low, t_struct_hi = st$spot_target_high,
    agreeing = st$targets_agreeing)
  if (i %% 25 == 0) message(i, "/", nrow(tk))
}
R <- do.call(rbind, out)
saveRDS(R, "output/bot_target_engines_result.rds")

R$d_bot   <- (R$t_bot - R$px) / R$atr
R$d_str   <- (R$t_struct - R$px) / R$atr
R$em_atr  <- R$em_abs / R$atr
R$stop5   <- 0.05 * R$px / R$atr
R$stopbot <- (R$px - R$stop_bot) / R$atr
q <- function(x) round(stats::quantile(x, c(.1, .25, .5, .75, .9), na.rm = TRUE), 2)
cat(sprintf("\n%d names, long\n", nrow(R)))
cat("structural target missing:", sum(!is.finite(R$t_struct)), "\n")
cat("\nTarget distance, ATR (p10 p25 p50 p75 p90):\n")
cat("  BOT_daily :", q(R$d_bot), "\n  Phase D   :", q(R$d_str), "\n  EM10 (ATR):", q(R$em_atr), "\n")
ok <- is.finite(R$d_bot) & is.finite(R$d_str)
cat(sprintf("\nPhase D target beyond the 10-session expected move: %d of %d (%.0f%%)\n",
            sum(R$d_str[ok] > R$em_atr[ok], na.rm = TRUE), sum(ok),
            100 * mean(R$d_str[ok] > R$em_atr[ok], na.rm = TRUE)))
cat(sprintf("BOT_daily target beyond it: %d (capped by construction)\n", sum(R$d_bot[ok] > R$em_atr[ok] + 1e-6, na.rm = TRUE)))
gap <- abs(R$d_bot - R$d_str)
cat(sprintf("Targets within 0.5 ATR of each other: %d of %d (%.0f%%); within 1 ATR: %.0f%%\n",
            sum(gap[ok] <= 0.5), sum(ok), 100 * mean(gap[ok] <= 0.5), 100 * mean(gap[ok] <= 1)))
cat(sprintf("Phase D nearer than BOT_daily: %.0f%%; farther: %.0f%%\n",
            100 * mean(R$d_str[ok] < R$d_bot[ok] - 0.5), 100 * mean(R$d_str[ok] > R$d_bot[ok] + 0.5)))
cat(sprintf("Spearman(d_bot, d_str): %.2f\n", stats::cor(R$d_bot[ok], R$d_str[ok], method = "spearman")))
cat("\nStop distance, ATR (p10 p25 p50 p75 p90):\n")
cat("  BOT_daily (1-2.5 ATR band):", q(R$stopbot), "\n  Phase D stock row (5%)    :", q(R$stop5), "\n")
cat(sprintf("5%% stop farther than 2.5 ATR: %.0f%%; nearer than 1 ATR: %.0f%%\n",
            100 * mean(R$stop5 > 2.5, na.rm = TRUE), 100 * mean(R$stop5 < 1, na.rm = TRUE)))
cat("\nBOT_daily target sources:", paste(names(table(R$t_bot_src)), table(R$t_bot_src), collapse = ", "), "\n")
cat("Phase D targets_agreeing:", paste(names(table(R$agreeing, useNA = "ifany")), table(R$agreeing, useNA = "ifany"), collapse = ", "), "\n")
