# bot_zones_breakout_trial.R — replay of the BOT_daily level engine with and
# without ZONE_DEFAULTS$bar_order / $breakout_n (TODO #82 B.7, BOT_TOOLS_DESIGN
# v3.42, 2026-10-26).
#
# Run from the RStudies project root:
#   Rscript bot_zones_breakout_trial.R [bot_daily xlsx]
# The universe, directions and em10 coefficients come from the xlsx Detail
# sheet. Each variant overrides the defaults explicitly, so `base` reproduces the
# engine before 2026-10-26 whatever ZONE_DEFAULTS now says.
#
# 1. Today: target / stop / asym per name under each variant.
# 2. Replay: every 10 sessions over ~2y, long and short, first touch of target
#    vs stop within 20 sessions (stop filled at the open on a gap, same-bar touch
#    counted as stop); paired R difference with a name-level bootstrap.
suppressMessages(library(Tdata)); SH <- "reports/shared"
for (f in c("indicators.R","weekly.R","zones.R","gates.R","market_calendar.R","name_attributes.R","bot_read.R")) source(file.path(SH, f))
args <- commandArgs(TRUE)
SP <- "output"; dir.create(SP, showWarnings = FALSE)
XLSX <- if (length(args)) args[1] else "C:/Users/aldoh/Documents/NewTrading/Reports/bot_daily_20261026_0952.xlsx"
x <- as.data.frame(readxl::read_excel(XLSX, "Detail"))
x <- x[!duplicated(x$yahoo), c("name","yahoo","direction","px","atr_pct","em10_hi","em10_lo","asym","target_source")]
num <- function(v) suppressWarnings(as.numeric(v))
x$k_hi <- num(x$em10_hi) / num(x$atr_pct); x$k_lo <- abs(num(x$em10_lo)) / num(x$atr_pct)
x$k_hi[!is.finite(x$k_hi)] <- median(x$k_hi, na.rm = TRUE); x$k_lo[!is.finite(x$k_lo)] <- median(x$k_lo, na.rm = TRUE)
V <- list(base = list(bar_order = FALSE, breakout_n = 0L), both2 = list(bar_order = TRUE, breakout_n = 2L), both3 = list(bar_order = TRUE, breakout_n = 3L),
          bar = list(bar_order = TRUE, breakout_n = 0L), brk2 = list(bar_order = FALSE, breakout_n = 2L))
cache <- file.path(SP, "bot_zones_breakout_data.rds")
D <- if (file.exists(cache)) readRDS(cache) else list()
for (y in x$yahoo) if (is.null(D[[y]])) D[[y]] <- tryCatch(bot_fetch_daily(y), error = function(e) NULL)
saveRDS(D, cache)
cat("names", nrow(x), "fetched", sum(!vapply(D, is.null, TRUE)), "\n")

lr_row <- function(zd, atr, em, dir, cfg) {
  lr <- tryCatch(level_read(zd, atr, em, cfg = cfg, direction = dir), error = function(e) NULL)
  if (is.null(lr)) return(NULL)
  data.frame(target = lr$target, src = lr$target_source, stop = lr$stop_px, ssrc = lr$stop_source, asym = lr$asym,
             in_res = isTRUE(lr$in_res_zone))
}
# ── 1. Today ──
today <- list()
for (i in seq_len(nrow(x))) {
  d <- D[[x$yahoo[i]]]; if (is.null(d)) next
  px <- tail(d$Close, 1); atr <- tail(d$atr14, 1); dir <- x$direction[i]
  em <- atr * (if (dir == "short") x$k_lo[i] else x$k_hi[i])
  zd <- d[as.Date(d$date) >= Sys.Date() - round(BOT_ZONE_YEARS * 365), ]
  for (v in names(V)) { r <- lr_row(zd, atr, em, dir, modifyList(ZONE_DEFAULTS, V[[v]]))
    if (!is.null(r)) today[[length(today) + 1]] <- cbind(name = x$name[i], dir = dir, v = v, px = px, atr = atr, r) }
}
today <- do.call(rbind, today); saveRDS(today, file.path(SP, "bot_zones_breakout_today.rds"))

# ── 2. Replay: every 5 sessions over the last ~2y, both directions, 20-session first touch ──
H <- 20; rep <- list()
for (y in names(D)) {
  d <- D[[y]]; if (is.null(d)) next; n <- nrow(d); xi <- x[x$yahoo == y, ][1, ]
  dt <- as.Date(d$date)
  for (t in seq(n - H, max(n - 520, 300), by = -10)) {
    atr <- d$atr14[t]; px <- d$Close[t]; if (!is.finite(atr) || atr <= 0) next
    zd <- d[dt >= dt[t] - round(BOT_ZONE_YEARS * 365) & seq_len(n) <= t, ]
    if (nrow(zd) < 130) next
    f <- d[(t + 1):(t + H), ]
    for (dir in c("long", "short")) {
      em <- atr * (if (dir == "short") xi$k_lo else xi$k_hi); s <- if (dir == "long") 1 else -1
      for (v in c("base", "both2", "both3")) {
        r <- lr_row(zd, atr, em, dir, modifyList(ZONE_DEFAULTS, V[[v]]))
        if (is.null(r) || !is.finite(r$asym) || !is.finite(r$stop)) next
        risk <- s * (px - r$stop)
        hit_t <- which(if (s > 0) f$High >= r$target else f$Low <= r$target)[1]
        hit_s <- which(if (s > 0) f$Low <= r$stop else f$High >= r$stop)[1]
        out <- if (!is.na(hit_s) && (is.na(hit_t) || hit_s <= hit_t)) "stop" else if (!is.na(hit_t)) "target" else "open"
        R <- switch(out,
          stop = { o <- f$Open[hit_s]; fill <- if (s > 0) min(o, r$stop) else max(o, r$stop); s * (fill - px) / risk },
          target = s * (r$target - px) / risk,
          open = s * (f$Close[H] - px) / risk)
        rep[[length(rep) + 1]] <- data.frame(y = y, t = t, date = d$date[t], dir = dir, v = v, asym = r$asym,
                                             src = r$src, tgt_atr = s * (r$target - px) / atr, out = out, R = R)
      }
    }
  }
}
rep <- do.call(rbind, rep); saveRDS(rep, file.path(SP, "bot_zones_breakout_replay.rds"))
cat("replay rows", nrow(rep), "\n")

# ── 3. Summary ──
td <- today; rp <- rep
cat("== TODAY: per variant ==\n")
s <- function(a) sprintf("n=%d  med asym %.2f  asym<1 %.0f%%  in_zone target %.0f%%  tgt<1ATR %.0f%%  em_cap %.0f%%",
  nrow(a), median(a$asym, na.rm=T), 100*mean(a$asym < 1, na.rm=T), 100*mean(a$src=="in_zone"),
  100*mean(abs(a$target-a$px)/a$atr < 1, na.rm=T), 100*mean(a$src=="em10_cap"))
for (v in unique(td$v)) cat(sprintf("%-6s %s\n", v, s(td[td$v==v,])))
b <- td[td$v=="base",]; n2 <- td[td$v=="both2",]; m <- merge(b, n2, by=c("name","dir"), suffixes=c(".b",".n"))
ch <- m[abs(m$target.b - m$target.n) > 1e-6 | abs(m$stop.b - m$stop.n) > 1e-6, ]
cat(sprintf("\nboth2 changed %d of %d names\n", nrow(ch), nrow(m)))
print(ch[order(-(ch$asym.n - ch$asym.b)), c("name","dir","px.b","target.b","src.b","stop.b","asym.b","target.n","src.n","stop.n","ssrc.n","asym.n")], row.names=F, digits=4)

cat("\n== REPLAY ==\n")
w <- reshape(rp[, c("y","t","dir","v","asym","src","tgt_atr","out","R")], idvar=c("y","t","dir"), timevar="v", direction="wide")
w <- w[complete.cases(w[, c("R.base","R.both2","R.both3")]), ]
agg <- function(a, v) { R <- a[[paste0("R.",v)]]; o <- a[[paste0("out.",v)]]
  sprintf("%-6s n=%5d  meanR %+.3f  medAsym %.2f  target %.0f%%  stop %.0f%%  open %.0f%%  medTgtATR %.2f", v, nrow(a), mean(R),
    median(a[[paste0("asym.",v)]]), 100*mean(o=="target"), 100*mean(o=="stop"), 100*mean(o=="open"), median(a[[paste0("tgt_atr.",v)]])) }
cat("-- all rows --\n"); for (v in c("base","both2","both3")) cat(agg(w, v), "\n")
for (v in c("both2","both3")) {
  dd <- w[abs(w$R.base - w[[paste0("R.",v)]]) > 1e-9 | w$asym.base != w[[paste0("asym.",v)]], ]
  cat(sprintf("-- rows where %s differs from base (%d, %.1f%%) --\n", v, nrow(dd), 100*nrow(dd)/nrow(w)))
  cat(agg(dd, "base"), "\n"); cat(agg(dd, v), "\n")
  dR <- dd[[paste0("R.",v)]] - dd$R.base
  # Names cluster in time; block bootstrap by name for the paired difference.
  ys <- unique(dd$y); set.seed(1)
  bs <- replicate(2000, { k <- sample(ys, replace=TRUE); mean(unlist(lapply(k, function(z) dR[dd$y==z]))) })
  cat(sprintf("   paired dR %+.3f  name-bootstrap 95%% CI [%+.3f, %+.3f]  names %d\n", mean(dR), quantile(bs,.025), quantile(bs,.975), length(ys)))
  for (dir in c("long","short")) { e <- dd$dir==dir; cat(sprintf("   %s: n=%d dR %+.3f\n", dir, sum(e), mean(dR[e]))) }
  # Selection view: does a BOT-style asym>=1 filter pick better trades?
  for (vv in c("base", v)) { k <- dd[[paste0("asym.",vv)]] >= 1
    cat(sprintf("   asym>=1 under %-6s: n=%d meanR %+.3f\n", vv, sum(k), mean(dd[[paste0("R.",vv)]][k]))) }
}
