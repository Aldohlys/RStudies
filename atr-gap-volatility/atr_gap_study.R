# ============================================================
# ATR, daily-range distribution and overnight-gap vs session volatility
# ============================================================
# Questions:
#   (1) How often does a day's true range exceed ATR(14)? Distribution of
#       TR, high-low and |close-to-close| expressed in multiples of the
#       ATR known at the previous close (no look-ahead).
#   (2) How much of daily variance comes from the overnight gap
#       (prev close -> open) versus the regular session (open -> close)?
#       Close-to-close, overnight, open-close, Rogers-Satchell, Parkinson,
#       Yang-Zhang; concentration of gap variance in the largest gaps;
#       rolling 63-session decomposition.
#
# Usage (from RStudies/):
#   Rscript atr-gap-volatility/atr_gap_study.R            # AAPL, 2 years
#   Rscript atr-gap-volatility/atr_gap_study.R MSFT 3     # ticker, years
#
# Data: daily OHLC from Tdata::getYahooData (same loader as
#       reports/shared/indicators.R). ATR = TTR::ATR(n = 14), Wilder smoothing,
#       as in calc_ind().
# Output: output/atr_gap/{TICKER}/  (summary.txt, exceedance.csv,
#         tr_vs_atr.png, gap_vs_session.png)
# Findings: atr-gap-volatility/FINDINGS.md

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(Tdata)
})

args    <- commandArgs(trailingOnly = TRUE)
TICKER  <- if (length(args) >= 1) toupper(args[1]) else "AAPL"
YEARS   <- if (length(args) >= 2) as.numeric(args[2]) else 2
N_ATR   <- 14
WARMUP  <- 30          # sessions dropped so the Wilder seed has decayed
ROLL_W  <- 63          # rolling window, ~3 months
ANN     <- 252
K_GRID  <- c(0.5, 0.75, 1, 1.25, 1.5, 2, 2.5, 3)
OUT_DIR <- file.path("output", "atr_gap", TICKER)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- data ------------------------------------------------------------------
yn  <- tryCatch(Tdata::getYahooName(TICKER), error = function(e) TICKER)
if (is.na(yn) || yn == "BASE_CURRENCY") yn <- TICKER
raw <- Tdata::getYahooData(tickers = yn,
                           from_date = Sys.Date() - round(YEARS * 365.25) - 90,
                           to_date   = Sys.Date())
d <- as.data.table(raw)[, .(date = as.Date(date), open = Open, high = High,
                            low = Low, close = Close)]
d <- d[complete.cases(d)][order(date)]
# keep only completed sessions: drop today's bar while the US session is open
if (d[.N, date] == Sys.Date()) d <- d[-.N]

d[, prev_close := shift(close)]
d[, tr   := pmax(high, prev_close) - pmin(low, prev_close)]
d[, hl   := high - low]
d[, atr  := as.numeric(TTR::ATR(cbind(high, low, close), n = N_ATR)[, "atr"])]
d[, atr_prev := shift(atr)]

# overnight / session log returns
d[, r_on := log(open / prev_close)]
d[, r_id := log(close / open)]
d[, r_cc := r_on + r_id]

start <- d[.N, date] - round(YEARS * 365.25)
first <- which(d$date > start)[1]
stopifnot(first > N_ATR + WARMUP)   # Wilder seed has decayed before the sample starts
s <- d[first:.N]
N <- nrow(s)

s[, x_tr := tr / atr_prev]
s[, x_hl := hl / atr_prev]
s[, x_cc := abs(close - prev_close) / atr_prev]

out <- character()
say <- function(...) { line <- sprintf(...); out <<- c(out, line); cat(line, "\n") }

say("%s  %s .. %s  sessions %d  last close %.2f  ATR(14) %.2f (%.2f%%)",
    TICKER, s[1, date], s[.N, date], N, s[.N, close], d[.N, atr], d[.N, atr] / d[.N, close] * 100)

# ---- (1) range distributions vs ATR ----------------------------------------
series <- list(tr = "True range", hl = "High - low", cc = "|Close-to-close|")
exc <- rbindlist(lapply(names(series), function(k) {
  x <- s[[paste0("x_", k)]]
  data.table(measure = series[[k]], k = K_GRID, pct_above = sapply(K_GRID, function(m) mean(x > m) * 100))
}))
fwrite(exc, file.path(OUT_DIR, "exceedance.csv"))

say("")
say("Share of days with move > k x ATR(prev close)")
say("%-18s %7s %s", "", "median", paste(sprintf("%7s", paste0(">", K_GRID, "x")), collapse = ""))
for (k in names(series)) {
  x <- s[[paste0("x_", k)]]
  say("%-18s %6.2fx %s", series[[k]], median(x),
      paste(sprintf("%6.1f%%", sapply(K_GRID, function(m) mean(x > m) * 100)), collapse = ""))
}

e  <- s$x_tr > 1
pe <- mean(e)
say("")
say("TR > ATR: P(exceed | prev exceeded) %.1f%%  P(exceed | prev not) %.1f%%",
    mean(e[-1][e[-N]]) * 100, mean(e[-1][!e[-N]]) * 100)
cnt <- frollsum(as.integer(e), 14)[14:N]
say("TR > ATR days per 14-session window: mean %.2f  range %d-%d  sd %.2f (binomial %.2f)  P(>=7) %.1f%%",
    mean(cnt), min(cnt), max(cnt), sd(cnt), sqrt(14 * pe * (1 - pe)), mean(cnt >= 7) * 100)
say("TR > H-L (gap extends the range) on %.1f%% of days", mean(s$tr > s$hl + 1e-9) * 100)
say("Mean TR %.2f = H-L %.2f (%.1f%%) + gap extension %.2f (%.1f%%)",
    mean(s$tr), mean(s$hl), mean(s$hl) / mean(s$tr) * 100,
    mean(s$tr - s$hl), mean(s$tr - s$hl) / mean(s$tr) * 100)
top_tr <- s$tr > quantile(s$tr, 0.9)
say("Top-decile TR days: gap extension = %.1f%% of TR",
    sum((s$tr - s$hl)[top_tr]) / sum(s$tr[top_tr]) * 100)

# ---- (2) overnight vs session variance -------------------------------------
v_on <- var(s$r_on); v_id <- var(s$r_id); v_cc <- var(s$r_cc); cv <- cov(s$r_on, s$r_id)
lh <- log(s$high / s$open); ll <- log(s$low / s$open); lc <- log(s$close / s$open)
rs   <- mean(lh * (lh - lc) + ll * (ll - lc))
park <- mean(log(s$high / s$low)^2) / (4 * log(2))
kyz  <- 0.34 / (1.34 + (N + 1) / (N - 1))
yz   <- v_on + kyz * v_id + (1 - kyz) * rs
av   <- function(v) sqrt(v * ANN) * 100
kurt <- function(x) { x <- x - mean(x); mean(x^4) / mean(x^2)^2 - 3 }

say("")
say("Annualized vol: close-close %.1f%%  overnight %.1f%%  open-close %.1f%%  Rogers-Satchell %.1f%%  Parkinson %.1f%%  Yang-Zhang %.1f%%",
    av(v_cc), av(v_on), av(v_id), av(rs), av(park), av(yz))
say("Variance shares of close-close: overnight %.1f%%  session %.1f%%  2cov %.1f%%  corr %.3f",
    v_on / v_cc * 100, v_id / v_cc * 100, 2 * cv / v_cc * 100, cv / sqrt(v_on * v_id))
say("Overnight share  on/(on+session) %.1f%%   of Yang-Zhang %.1f%%",
    v_on / (v_on + v_id) * 100, v_on / yz * 100)
say("Variance per hour, session (6.5h) / overnight (17.5h): %.1fx", (v_id / 6.5) / (v_on / 17.5))
say("Excess kurtosis: overnight %.1f  session %.1f  close-close %.1f", kurt(s$r_on), kurt(s$r_id), kurt(s$r_cc))

ord <- order(-abs(s$r_on))
for (n_top in c(5, 10, 20))
  say("Top %d gaps (%.1f%% of days) = %.1f%% of overnight variance",
      n_top, n_top / N * 100, sum(s$r_on[ord[1:n_top]]^2) / sum(s$r_on^2) * 100)
v_on_ex <- var(s$r_on[-ord[1:10]])
say("Excluding top-10 gaps: overnight vol %.1f%%  share %.1f%%", av(v_on_ex), v_on_ex / (v_on_ex + v_id) * 100)
say("Largest gaps: %s", paste(sprintf("%s %+.1f%%", s$date[ord[1:10]], s$r_on[ord[1:10]] * 100), collapse = ", "))
j <- which.max(abs(s$r_id))
say("Largest session move: %s %+.1f%%", s$date[j], s$r_id[j] * 100)

same <- sign(s$r_on) == sign(s$r_id)
big  <- abs(s$r_on) > quantile(abs(s$r_on), 0.8)
say("Session continues gap direction: all days %.1f%%  largest 20%% of gaps %.1f%%",
    mean(same) * 100, mean(same[big]) * 100)

s[, roll_on := frollapply(r_on, ROLL_W, var)]
s[, roll_id := frollapply(r_id, ROLL_W, var)]
s[, roll_cc := frollapply(r_cc, ROLL_W, var)]
s[, roll_share := roll_on / (roll_on + roll_id) * 100]
rl <- s[!is.na(roll_share)]
say("Rolling %d-session overnight share: min %.0f%%  max %.0f%%  latest %.0f%%  (latest vol: overnight %.1f%% session %.1f%% cc %.1f%%)",
    ROLL_W, min(rl$roll_share), max(rl$roll_share), rl[.N, roll_share],
    av(rl[.N, roll_on]), av(rl[.N, roll_id]), av(rl[.N, roll_cc]))

writeLines(out, file.path(OUT_DIR, "summary.txt"))

# ---- charts ------------------------------------------------------------------
COL <- c("True range" = "#2a78d6", "High - low" = "#1baf7a", "|Close-to-close|" = "#eb6834",
         "Overnight gap" = "#eb6834", "Session (open -> close)" = "#2a78d6", "Close-to-close" = "#52514e")
thm <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "top", legend.justification = "right", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 11), plot.title.position = "plot",
        plot.background = element_rect(fill = "#fcfcfb", colour = NA))

stack_png <- function(plots, file, heights, w = 10, h = 8.5) {
  png(file, width = w, height = h, units = "in", res = 150)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(length(plots), 1, heights = grid::unit(heights, "null"))))
  for (i in seq_along(plots)) print(plots[[i]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1))
  invisible(dev.off())
}

XMAX <- 3.5
long <- rbindlist(lapply(names(series), function(k)
  data.table(measure = series[[k]], x = pmin(s[[paste0("x_", k)]], XMAX - 1e-9))))
long[, measure := factor(measure, levels = unlist(series))]
meds <- long[, .(med = median(x)), by = measure]
p_hist <- ggplot(long, aes(x, colour = measure, fill = measure)) +
  geom_freqpoly(aes(y = after_stat(count / N * 100)), binwidth = 0.1, boundary = 0, linewidth = 0.9) +
  geom_vline(xintercept = 1, colour = "#52514e") +
  geom_vline(data = meds, aes(xintercept = med, colour = measure), linetype = "dotted") +
  scale_colour_manual(values = COL) + scale_fill_manual(values = COL, guide = "none") +
  scale_x_continuous(labels = function(v) paste0(v, "x")) + coord_cartesian(xlim = c(0, XMAX)) +
  labs(title = sprintf("%s daily move as a multiple of prior-day ATR(%d)", TICKER, N_ATR),
       subtitle = sprintf("%d sessions, %s to %s; dotted = medians; moves >= %.1fx pooled in last bin",
                          N, s[1, date], s[.N, date], XMAX),
       x = NULL, y = "% of days per 0.1x bin") + thm

xs <- seq(0, XMAX, by = 0.01)
ex_curve <- rbindlist(lapply(names(series), function(k) {
  x <- s[[paste0("x_", k)]]
  data.table(measure = series[[k]], x = xs, p = sapply(xs, function(m) mean(x > m) * 100))
}))
ex_curve[, measure := factor(measure, levels = unlist(series))]
at1 <- ex_curve[abs(x - 1) < 1e-9]
p_exc <- ggplot(ex_curve, aes(x, p, colour = measure)) +
  geom_vline(xintercept = 1, colour = "#52514e") +
  geom_line(linewidth = 0.9) +
  geom_point(data = at1, size = 2.5) +
  geom_text(data = at1, aes(label = sprintf("%.0f%%", p)), hjust = -0.3, vjust = -0.3, size = 3.2,
            colour = "#0b0b0b", show.legend = FALSE) +
  scale_colour_manual(values = COL, guide = "none") +
  scale_x_continuous(labels = function(v) paste0(v, "x")) + coord_cartesian(xlim = c(0, XMAX)) +
  labs(title = "Share of days with move > x * ATR", x = "Multiple of ATR (x)", y = "% of days") + thm
stack_png(list(p_hist, p_exc), file.path(OUT_DIR, "tr_vs_atr.png"), c(1, 1))

bars <- rbind(s[, .(date, r = r_on * 100, part = "Overnight gap")],
              s[, .(date, r = r_id * 100, part = "Session (open -> close)")])
p_bars <- ggplot(bars, aes(date, r, colour = part)) +
  geom_hline(yintercept = 0, colour = "#52514e") +
  geom_segment(aes(xend = date, yend = 0), linewidth = 0.5, alpha = 0.8) +
  scale_colour_manual(values = COL) +
  labs(title = sprintf("%s volatility: overnight gap vs regular session", TICKER),
       subtitle = sprintf("%d sessions, %s to %s; rolling window %d sessions", N, s[1, date], s[.N, date], ROLL_W),
       x = NULL, y = "Daily log return, %") + thm
rv <- rbind(rl[, .(date, v = av(roll_cc), part = "Close-to-close")],
            rl[, .(date, v = av(roll_id), part = "Session (open -> close)")],
            rl[, .(date, v = av(roll_on), part = "Overnight gap")])
p_roll <- ggplot(rv, aes(date, v, colour = part)) + geom_line(linewidth = 0.9) +
  scale_colour_manual(values = COL) + expand_limits(y = 0) +
  labs(title = sprintf("Rolling %d-session annualized volatility", ROLL_W), x = NULL, y = "%") + thm
p_share <- ggplot(rl, aes(date, roll_share)) +
  geom_area(fill = "#eb6834", alpha = 0.18) + geom_line(colour = "#eb6834", linewidth = 0.9) +
  geom_hline(yintercept = v_on / (v_on + v_id) * 100, linetype = "dashed", colour = "#52514e") +
  annotate("text", x = rl[1, date], y = v_on / (v_on + v_id) * 100 + 5, hjust = 0, size = 3.2, colour = "#52514e",
           label = sprintf("full-sample %.0f%%", v_on / (v_on + v_id) * 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Overnight gap share of daily variance: overnight / (overnight + session)", x = NULL, y = "% of variance") + thm
xr <- coord_cartesian(xlim = range(s$date))
p_bars <- p_bars + xr; p_roll <- p_roll + xr; p_share <- p_share + xr
stack_png(list(p_bars, p_roll, p_share), file.path(OUT_DIR, "gap_vs_session.png"), c(1, 1.1, 0.9), h = 10)

cat("\nWritten to", normalizePath(OUT_DIR), "\n")
