# bot_zones_threshold_trial.R — TODO 88.6: is a finer ZigZag threshold worth it?
#
# A zone is useful if price REACTS at it. For each name, every STEP sessions
# over the last ~2 years, zones are built from the ZONE_WIN sessions ending at
# t, and the nearest zone wholly overhead is followed forward:
#   reached  : High reaches zone lo within HORIZON sessions
#   reacted  : after first contact, a Close >= REACT_ATR below lo comes before
#              any Close above hi, within REACT_WIN sessions
# A placebo level of the same width is placed at a distance (in ATR) drawn from
# the pooled distance distribution of the same config, on the same name/date,
# and scored identically. Lift = P(react | reach) zone - placebo.
# A finer threshold earns its extra zones only if their lift holds.
#
# Also reports, on the last session: zones per name, distance to nearest
# resistance, and how often spot stands inside a zone (the in-zone veto rate).
#
# Run from the RStudies project root:
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" bot_zones_threshold_trial.R [--cache PATH] [SYM ...]

suppressPackageStartupMessages({ library(Tdata); library(DBI) })
source(file.path("reports", "shared", "zones.R"))

ZONE_WIN  <- 504
STEP      <- 21
HORIZON   <- 20
REACT_WIN <- 10
REACT_ATR <- 1.0
set.seed(88)

CONFIGS <- list(
  base    = list(),
  zz150   = list(zz_atr = 1.50),
  zz125   = list(zz_atr = 1.25),
  ceil07  = list(zz_ceil = 0.07))
cfg_of <- function(o) modifyList(ZONE_DEFAULTS, o)

args  <- commandArgs(trailingOnly = TRUE)
ci    <- match("--cache", args)
cache <- if (!is.na(ci)) args[ci + 1] else "output/bot_zones_threshold_cache.rds"
syms  <- args[!grepl("^--", args) & !(seq_along(args) %in% (ci + 1))]

if (file.exists(cache)) {
  data <- readRDS(cache)
} else {
  conn <- dbConnect(RSQLite::SQLite(), Sys.getenv("R_DB_PATH"))
  tk <- dbGetQuery(conn, "SELECT Name, YahooName FROM Tickers
                          WHERE Type IN ('STK','ETF','FUT') AND COALESCE(ADV_Pass, 1) = 1")
  dbDisconnect(conn)
  if (length(syms)) tk <- tk[tk$Name %in% syms, , drop = FALSE]
  data <- list()
  for (i in seq_len(nrow(tk))) {
    yh <- if (!is.na(tk$YahooName[i]) && nzchar(tk$YahooName[i])) tk$YahooName[i] else tk$Name[i]
    d <- try(getSymIntervalDate(yh, Sys.Date() - 5 * 365, Sys.Date()), silent = TRUE)
    if (inherits(d, "try-error") || is.null(d)) next
    d <- d[!is.na(d$Close) & !is.na(d$High) & !is.na(d$Low), ]
    if (nrow(d) < ZONE_WIN + 100) next
    d$atr14 <- as.numeric(TTR::ATR(cbind(d$High, d$Low, d$Close), n = 14)[, "atr"])
    data[[tk$Name[i]]] <- d[, c("date", "High", "Low", "Close", "atr14")]
    if (i %% 25 == 0) message(i, "/", nrow(tk))
  }
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, cache)
}
if (length(syms)) data <- data[intersect(names(data), syms)]
message(length(data), " names")

zones_at <- function(d, t, cfg) {
  w <- d[(t - ZONE_WIN + 1):t, , drop = FALSE]
  atr <- w$atr14[nrow(w)]; px <- w$Close[nrow(w)]
  piv <- zigzag_pivots(w, zz_threshold(atr, px, cfg))
  list(h = build_zones(piv, "H", atr, cfg), l = build_zones(piv, "L", atr, cfg),
       atr = atr, px = px, n_piv = if (is.null(piv)) 0L else nrow(piv))
}

# Reach-then-react for a band [lo, hi] above spot, from session t.
follow <- function(d, t, lo, hi, atr) {
  end <- min(nrow(d), t + HORIZON)
  if (end <= t) return(c(reached = NA, reacted = NA))
  k <- which(d$High[(t + 1):end] >= lo)
  if (!length(k)) return(c(reached = 0, reacted = NA))
  k <- t + k[1]
  e2 <- min(nrow(d), k + REACT_WIN)
  cl <- d$Close[k:e2]
  down <- which(cl <= lo - REACT_ATR * atr); up <- which(cl > hi)
  reacted <- length(down) && (!length(up) || down[1] < up[1])
  c(reached = 1, reacted = as.numeric(reacted))
}

hist_rows <- list(); now_rows <- list()
for (nm in names(data)) {
  d <- data[[nm]]; n <- nrow(d)
  ts <- rev(seq(n - HORIZON - REACT_WIN, max(ZONE_WIN, n - 504), by = -STEP))
  for (cn in names(CONFIGS)) {
    cfg <- cfg_of(CONFIGS[[cn]])
    for (t in ts) {
      z <- zones_at(d, t, cfg)
      if (is.null(z$h) || !is.finite(z$atr)) next
      a <- z$h[z$h$lo > z$px, , drop = FALSE]
      if (!nrow(a)) next
      r <- a[which.min(a$lo), ]
      f <- follow(d, t, r$lo, r$hi, z$atr)
      hist_rows[[length(hist_rows) + 1]] <- data.frame(name = nm, cfg = cn, t = t,
        dist_atr = (r$lo - z$px) / z$atr, width_atr = (r$hi - r$lo) / z$atr,
        touches = r$touches, reached = f[["reached"]], reacted = f[["reacted"]],
        atr = z$atr, px = z$px)
    }
    z <- zones_at(d, n, cfg)
    inside <- function(zz) !is.null(zz) && any(zz$lo <= z$px & zz$hi >= z$px)
    a <- if (!is.null(z$h)) z$h[z$h$lo > z$px, , drop = FALSE] else NULL
    now_rows[[length(now_rows) + 1]] <- data.frame(name = nm, cfg = cn, n_piv = z$n_piv,
      n_res = if (is.null(z$h)) 0L else nrow(z$h), n_sup = if (is.null(z$l)) 0L else nrow(z$l),
      res_dist_atr = if (!is.null(a) && nrow(a)) min(a$lo - z$px) / z$atr else NA_real_,
      in_res = inside(z$h), in_sup = inside(z$l))
  }
}
H <- do.call(rbind, hist_rows); N <- do.call(rbind, now_rows)

# Placebo: same name/date/width, distance drawn from this config's pooled distances.
H$p_reached <- NA_real_; H$p_reacted <- NA_real_; H$p_dist <- NA_real_
for (cn in names(CONFIGS)) {
  ix <- which(H$cfg == cn); pool <- H$dist_atr[ix]
  for (j in ix) {
    dd <- sample(pool, 1); lo <- H$px[j] + dd * H$atr[j]; hi <- lo + H$width_atr[j] * H$atr[j]
    f <- follow(data[[H$name[j]]], H$t[j], lo, hi, H$atr[j])
    H$p_dist[j] <- dd; H$p_reached[j] <- f[["reached"]]; H$p_reacted[j] <- f[["reacted"]]
  }
}

se <- function(p, n) sqrt(p * (1 - p) / n)
cat("\n=== Reaction at the nearest overhead zone vs placebo (history) ===\n")
for (cn in names(CONFIGS)) {
  h <- H[H$cfg == cn, ]
  zr <- h[h$reached %in% 1, ]; pr <- h[h$p_reached %in% 1, ]
  pz <- mean(zr$reacted); pp <- mean(pr$p_reacted)
  cat(sprintf("%-7s samples %5d  med dist %.2f ATR  reach %.3f | react|reach zone %.3f (n %d)  placebo %.3f (n %d)  lift %+.3f (SE %.3f)\n",
              cn, nrow(h), median(h$dist_atr), mean(h$reached, na.rm = TRUE),
              pz, nrow(zr), pp, nrow(pr), pz - pp, sqrt(se(pz, nrow(zr))^2 + se(pp, nrow(pr))^2)))
}
cat("\n--- lift by distance band (zone vs placebo, reached only) ---\n")
H$band <- cut(H$dist_atr, c(0, 1, 2, 4, Inf), right = FALSE)
for (cn in names(CONFIGS)) for (b in levels(H$band)) {
  z <- H[H$cfg == cn & H$band %in% b & H$reached %in% 1, ]
  p <- H[H$cfg == cn & H$p_reached %in% 1, ]
  p <- p[cut(p$p_dist, c(0, 1, 2, 4, Inf), right = FALSE) %in% b, ]
  if (nrow(z) >= 20 && nrow(p) >= 20)
    cat(sprintf("%-7s %-8s zone %.3f (n %4d)  placebo %.3f (n %4d)  lift %+.3f\n", cn, b,
                mean(z$reacted), nrow(z), mean(p$p_reacted), nrow(p), mean(z$reacted) - mean(p$p_reacted)))
}
cat("\n--- base: reaction by touches (reached only) ---\n")
for (k in list(2, 3, 4, 5:99)) {
  z <- H[H$cfg == "base" & H$reached %in% 1 & H$touches %in% k, ]
  cat(sprintf("touches %-5s zone %.3f (n %4d)\n", if (length(k) > 1) ">=5" else k, mean(z$reacted), nrow(z)))
}

cat("\n=== Last session: structure and veto cost ===\n")
for (cn in names(CONFIGS)) {
  x <- N[N$cfg == cn, ]
  cat(sprintf("%-7s pivots med %3.0f  res zones med %2.0f  sup med %2.0f  res overhead %3d/%d  med dist %.2f ATR  >3 ATR %3d  in_res %3d  in_sup %3d  in_either %3d\n",
              cn, median(x$n_piv), median(x$n_res), median(x$n_sup), sum(!is.na(x$res_dist_atr)), nrow(x),
              median(x$res_dist_atr, na.rm = TRUE), sum(x$res_dist_atr > 3, na.rm = TRUE),
              sum(x$in_res), sum(x$in_sup), sum(x$in_res | x$in_sup)))
}
saveRDS(list(H = H, N = N), "output/bot_zones_threshold_result.rds")
cat("\ndone\n")
