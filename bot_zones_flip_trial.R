# bot_zones_flip_trial.R — TODO 95: do zones that changed role show a reaction
# edge that same-type zones (TODO 94) did not?
#
# Same design as bot_zones_reaction_trial.R part A, on dividend-adjusted OHLC
# (what the level engine now reads). Every STEP sessions over ~2 years, zones
# are built from the ZONE_WIN sessions ending at t, and on each side the
# nearest zone of each KIND is followed forward:
#   same : resistance built from pivot highs above spot, support from lows below
#   flip : resistance built from pivot LOWS above spot (a broken support),
#          support built from pivot HIGHS below spot (a broken resistance)
#   reached : price touches the near edge within HORIZON sessions
#   reacted : after first contact, a close >= k ATR back on the near side comes
#             before any close beyond the far edge, within W sessions
# A placebo band (same name/date/width, distance drawn from the pool of the
# same side and kind) is scored identically. Lift = zone - placebo, with
# name-clustered bootstrap SEs.
#
# Run from the RStudies project root:
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" bot_zones_flip_trial.R

suppressPackageStartupMessages({ library(Tdata); library(DBI) })
source(file.path("reports", "shared", "zones.R"))

CACHE     <- "output/bot_zones_adj_cache.rds"
ZONE_WIN  <- 504
STEP      <- 10
HORIZON   <- 20
KS        <- c(0.5, 1.0, 1.5)
WS        <- c(5L, 10L, 20L)
BOOT      <- 300
set.seed(95)
RCOLS <- c("reached", as.vector(outer(sprintf("%.1f", KS), WS,
  function(k, w) sprintf("r_k%s_w%d", k, w))))
full <- function(f) { x <- setNames(rep(NA_integer_, length(RCOLS)), RCOLS); x[names(f)] <- unlist(f); as.list(x) }

# ── Data: dividend-adjusted OHLC, as bot_fetch_daily() reads it ─────────────
if (file.exists(CACHE)) {
  data <- readRDS(CACHE)
} else {
  conn <- Tdata::safe_db_connect()
  tk <- dbGetQuery(conn, "SELECT Name, YahooName FROM Tickers
                          WHERE Type IN ('STK','ETF','FUT') AND COALESCE(ADV_Pass, 1) = 1")
  dbDisconnect(conn)
  data <- list()
  for (i in seq_len(nrow(tk))) {
    yh <- if (!is.na(tk$YahooName[i]) && nzchar(tk$YahooName[i])) tk$YahooName[i] else tk$Name[i]
    d <- try(getSymIntervalDate(yh, Sys.Date() - 5 * 365, Sys.Date()), silent = TRUE)
    if (inherits(d, "try-error") || is.null(d)) next
    d <- d[is.finite(d$Close) & is.finite(d$High) & is.finite(d$Low), ]
    if (nrow(d) < ZONE_WIN + 100) next
    f <- d$Adjusted / d$Close; f[!is.finite(f) | f <= 0] <- 1
    for (k in c("Open", "High", "Low", "Close")) d[[k]] <- d[[k]] * f
    d$atr14 <- as.numeric(TTR::ATR(cbind(d$High, d$Low, d$Close), n = 14)[, "atr"])
    data[[tk$Name[i]]] <- d[, c("date", "High", "Low", "Close", "atr14")]
    if (i %% 25 == 0) message(i, "/", nrow(tk))
  }
  dir.create(dirname(CACHE), showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, CACHE)
}
message(length(data), " names")

follow <- function(d, t, lo, hi, atr, side) {
  end <- min(nrow(d), t + HORIZON)
  if (end <= t) return(NULL)
  idx <- (t + 1):end
  hit <- if (side == "res") which(d$High[idx] >= lo) else which(d$Low[idx] <= hi)
  if (!length(hit)) return(list(reached = 0L))
  k0 <- t + hit[1]
  out <- list(reached = 1L)
  for (w in WS) {
    cl <- d$Close[k0:min(nrow(d), k0 + w)]
    brk <- if (side == "res") which(cl > hi) else which(cl < lo)
    for (k in KS) {
      back <- if (side == "res") which(cl <= lo - k * atr) else which(cl >= hi + k * atr)
      out[[sprintf("r_k%.1f_w%d", k, w)]] <-
        as.integer(length(back) > 0 && (!length(brk) || back[1] < brk[1]))
    }
  }
  out
}

A <- list()
for (nm in names(data)) {
  d <- data[[nm]]; if (is.null(d) || nrow(d) < ZONE_WIN + HORIZON + 1) next
  n <- nrow(d)
  for (t in seq(max(ZONE_WIN, n - 504 - HORIZON), n - HORIZON, by = STEP)) {
    w <- d[(t - ZONE_WIN + 1):t, , drop = FALSE]
    atr <- w$atr14[nrow(w)]; px <- w$Close[nrow(w)]
    if (!is.finite(atr) || atr <= 0) next
    piv <- zigzag_pivots(w, zz_threshold(atr, px))
    zh <- build_zones(piv, "H", atr); zl <- build_zones(piv, "L", atr)
    cand <- list(
      res_same = if (!is.null(zh)) zh[zh$lo > px, , drop = FALSE],
      res_flip = if (!is.null(zl)) zl[zl$lo > px, , drop = FALSE],
      sup_same = if (!is.null(zl)) zl[zl$hi < px, , drop = FALSE],
      sup_flip = if (!is.null(zh)) zh[zh$hi < px, , drop = FALSE])
    for (key in names(cand)) {
      a <- cand[[key]]; if (is.null(a) || !nrow(a)) next
      side <- substr(key, 1, 3); kind <- substr(key, 5, 8)
      r <- if (side == "res") a[which.min(a$lo), ] else a[which.max(a$hi), ]
      dist <- if (side == "res") (r$lo - px) / atr else (px - r$hi) / atr
      f <- follow(d, t, r$lo, r$hi, atr, side); if (is.null(f)) next
      A[[length(A) + 1]] <- data.frame(name = nm, t = t, side = side, kind = kind, src = "zone",
        dist = dist, width = (r$hi - r$lo) / atr, touches = r$touches, px = px, atr = atr,
        as.data.frame(full(f)))
    }
  }
}
A <- do.call(rbind, A)

P <- list()
for (i in seq_len(nrow(A))) {
  z <- A[i, ]; d <- data[[z$name]]
  pool <- A$dist[A$side == z$side & A$kind == z$kind]
  dd <- sample(pool, 1); wd <- z$width * z$atr
  if (z$side == "res") { lo <- z$px + dd * z$atr; hi <- lo + wd } else { hi <- z$px - dd * z$atr; lo <- hi - wd }
  f <- follow(d, z$t, lo, hi, z$atr, z$side); if (is.null(f)) next
  P[[length(P) + 1]] <- data.frame(name = z$name, t = z$t, side = z$side, kind = z$kind, src = "placebo",
    dist = dd, width = z$width, touches = NA, px = z$px, atr = z$atr, as.data.frame(full(f)))
}
AP <- rbind(A, do.call(rbind, P))
saveRDS(AP, "output/bot_zones_flip_result.rds")

boot_diff <- function(x1, g1, x2, g2) {
  ok1 <- !is.na(x1); ok2 <- !is.na(x2); x1 <- x1[ok1]; g1 <- g1[ok1]; x2 <- x2[ok2]; g2 <- g2[ok2]
  nms <- unique(c(g1, g2)); s1 <- split(x1, g1); s2 <- split(x2, g2)
  reps <- replicate(BOOT, { b <- sample(nms, replace = TRUE)
    mean(unlist(s1[b]), na.rm = TRUE) - mean(unlist(s2[b]), na.rm = TRUE) })
  c(diff = mean(x1) - mean(x2), se = stats::sd(reps, na.rm = TRUE), n1 = length(x1), n2 = length(x2))
}

cat("\n=== P(react | reached), zone vs placebo, by side and kind ===\n")
for (side in c("res", "sup")) for (kind in c("same", "flip")) {
  z0 <- AP[AP$side == side & AP$kind == kind & AP$src == "zone", ]
  p0 <- AP[AP$side == side & AP$kind == kind & AP$src == "placebo", ]
  cat(sprintf("\n-- %s / %s --  zones %d, reach zone %.3f placebo %.3f\n", side, kind, nrow(z0),
              mean(z0$reached), mean(p0$reached)))
  for (w in WS) for (k in KS) {
    col <- sprintf("r_k%.1f_w%d", k, w)
    z <- z0[z0$reached == 1, ]; p <- p0[p0$reached == 1, ]
    b <- boot_diff(z[[col]], z$name, p[[col]], p$name)
    cat(sprintf("  k %.1f w %2d : zone %.3f  placebo %.3f  lift %+.3f (SE %.3f)  n %d/%d\n",
                k, w, mean(z[[col]]), mean(p[[col]]), b[["diff"]], b[["se"]], b[["n1"]], b[["n2"]]))
  }
}

cat("\n=== Flip vs same, head to head (k 1.0, w 10, reached only) ===\n")
col <- "r_k1.0_w10"
for (side in c("res", "sup")) {
  f <- AP[AP$side == side & AP$kind == "flip" & AP$src == "zone" & AP$reached == 1, ]
  s <- AP[AP$side == side & AP$kind == "same" & AP$src == "zone" & AP$reached == 1, ]
  b <- boot_diff(f[[col]], f$name, s[[col]], s$name)
  cat(sprintf("  %s: flip %.3f (n %d)  same %.3f (n %d)  diff %+.3f (SE %.3f)\n",
              side, mean(f[[col]]), nrow(f), mean(s[[col]]), nrow(s), b[["diff"]], b[["se"]]))
}
cat("\ndone\n")
