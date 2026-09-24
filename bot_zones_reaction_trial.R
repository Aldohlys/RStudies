# bot_zones_reaction_trial.R — TODO 94: do BOT zones mark levels where price
# reacts, and does standing inside one make a long entry worse?
#
# Two questions, both on the production zone engine (reports/shared/zones.R,
# ZONE_DEFAULTS) and the name cache written by bot_zones_threshold_trial.R.
#
# A. REACTION AT A ZONE. Every STEP sessions, zones are built from the ZONE_WIN
#    sessions ending at t. For the nearest zone wholly above spot (resistance)
#    and wholly below (support), follow price HORIZON sessions forward:
#      reached : price touches the near edge
#      reacted : after first contact, a close >= k ATR back on the near side
#                comes before any close beyond the far edge, within W sessions
#    A placebo band of the same width, at a distance drawn from the pooled
#    distances of the same side, is scored identically on the same name/date.
#    Variants: k in {0.5, 1.0, 1.5} ATR, W in {5, 10, 20}; zones split by
#    touches (2 / >=3) and by recency of the last touch.
#
# B. THE IN-ZONE VETO. bot_daily vetoes an entry when spot stands inside a zone.
#    The claim is about the trade that follows, so it is tested on that: for a
#    long entered at the close of t, which comes first within HORIZON sessions,
#    +k ATR or -k ATR (k = 1.5), and the 10-session forward return in ATR.
#    Rows are split by spot inside a resistance zone / support zone / both /
#    neither. Name-clustered bootstrap SEs, because samples of one name overlap.
#
# Run from the RStudies project root:
#   "C:/Program Files/R/R-4.4.3/bin/Rscript.exe" bot_zones_reaction_trial.R

source(file.path("reports", "shared", "zones.R"))

CACHE     <- "output/bot_zones_threshold_cache.rds"
ZONE_WIN  <- 504
STEP      <- 10
HORIZON   <- 20
KS        <- c(0.5, 1.0, 1.5)
WS        <- c(5L, 10L, 20L)
FP_K      <- 1.5
RECENT    <- 126      # sessions: a zone last touched within this is "recent"
BOOT      <- 300
set.seed(94)
RCOLS <- c("reached", as.vector(outer(sprintf("%.1f", KS), WS,
  function(k, w) sprintf("r_k%s_w%d", k, w))))
full <- function(f) { x <- setNames(rep(NA_integer_, length(RCOLS)), RCOLS); x[names(f)] <- unlist(f); as.list(x) }

if (!file.exists(CACHE)) stop("Run bot_zones_threshold_trial.R first to build ", CACHE)
data <- readRDS(CACHE)
message(length(data), " names")

# First contact with the near edge, then which comes first within w sessions:
# a close k ATR back on the near side (reaction) or a close beyond the far edge.
# side "res": band above spot, contact from below. side "sup": mirror.
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

# Long entered at the close of t: +k ATR before -k ATR within HORIZON, and the
# 10-session forward return in ATR.
first_passage <- function(d, t, atr, k = FP_K) {
  end <- min(nrow(d), t + HORIZON)
  if (end <= t) return(c(win = NA, fwd10 = NA))
  px <- d$Close[t]; idx <- (t + 1):end
  up <- which(d$High[idx] >= px + k * atr); dn <- which(d$Low[idx] <= px - k * atr)
  win <- if (!length(up) && !length(dn)) NA_integer_
         else if (!length(dn)) 1L else if (!length(up)) 0L
         else if (up[1] < dn[1]) 1L else if (dn[1] < up[1]) 0L else NA_integer_
  f10 <- if (t + 10 <= nrow(d)) (d$Close[t + 10] - px) / atr else NA_real_
  c(win = win, fwd10 = f10)
}

A <- list(); B <- list()
for (nm in names(data)) {
  d <- data[[nm]]; if (is.null(d) || nrow(d) < ZONE_WIN + HORIZON + 1) next
  n <- nrow(d)
  ts <- seq(max(ZONE_WIN, n - 504 - HORIZON), n - HORIZON, by = STEP)
  for (t in ts) {
    w <- d[(t - ZONE_WIN + 1):t, , drop = FALSE]
    atr <- w$atr14[nrow(w)]; px <- w$Close[nrow(w)]
    if (!is.finite(atr) || atr <= 0) next
    piv <- zigzag_pivots(w, zz_threshold(atr, px))
    zh <- build_zones(piv, "H", atr); zl <- build_zones(piv, "L", atr)
    in_r <- !is.null(zh) && any(zh$lo <= px & zh$hi >= px)
    in_s <- !is.null(zl) && any(zl$lo <= px & zl$hi >= px)
    fp <- first_passage(d, t, atr)
    B[[length(B) + 1]] <- data.frame(name = nm, t = t, in_r = in_r, in_s = in_s,
                                     win = fp[["win"]], fwd10 = fp[["fwd10"]])
    for (side in c("res", "sup")) {
      z <- if (side == "res") zh else zl
      if (is.null(z)) next
      a <- if (side == "res") z[z$lo > px, , drop = FALSE] else z[z$hi < px, , drop = FALSE]
      if (!nrow(a)) next
      r <- if (side == "res") a[which.min(a$lo), ] else a[which.max(a$hi), ]
      dist <- if (side == "res") (r$lo - px) / atr else (px - r$hi) / atr
      f <- follow(d, t, r$lo, r$hi, atr, side)
      if (is.null(f)) next
      age <- t - match(as.character(r$last), as.character(d$date))
      A[[length(A) + 1]] <- data.frame(name = nm, t = t, side = side, kind = "zone",
        dist = dist, width = (r$hi - r$lo) / atr, touches = r$touches,
        age = age, px = px, atr = atr, as.data.frame(full(f)))
    }
  }
}
B <- do.call(rbind, B)
A <- do.call(rbind, A)

# Placebos: same name/date/width, distance drawn from the same side's pool.
P <- list()
for (i in seq_len(nrow(A))) {
  z <- A[i, ]; d <- data[[z$name]]
  pool <- A$dist[A$side == z$side]
  dd <- sample(pool, 1); wd <- z$width * z$atr
  if (z$side == "res") { lo <- z$px + dd * z$atr; hi <- lo + wd }
  else                 { hi <- z$px - dd * z$atr; lo <- hi - wd }
  f <- follow(d, z$t, lo, hi, z$atr, z$side)
  if (is.null(f)) next
  P[[length(P) + 1]] <- data.frame(name = z$name, t = z$t, side = z$side, kind = "placebo",
    dist = dd, width = z$width, touches = NA, age = NA, px = z$px, atr = z$atr, as.data.frame(full(f)))
}
P <- do.call(rbind, P)
AP <- rbind(A, P)
saveRDS(list(A = A, P = P, B = B), "output/bot_zones_reaction_result.rds")

# Name-clustered bootstrap of a difference in means.
boot_diff <- function(x1, g1, x2, g2) {
  ok1 <- !is.na(x1); ok2 <- !is.na(x2)
  x1 <- x1[ok1]; g1 <- g1[ok1]; x2 <- x2[ok2]; g2 <- g2[ok2]
  names_all <- unique(c(g1, g2))
  s1 <- split(x1, g1); s2 <- split(x2, g2)
  reps <- replicate(BOOT, {
    b <- sample(names_all, replace = TRUE)
    mean(unlist(s1[b]), na.rm = TRUE) - mean(unlist(s2[b]), na.rm = TRUE)
  })
  c(diff = mean(x1) - mean(x2), se = stats::sd(reps, na.rm = TRUE), n1 = length(x1), n2 = length(x2))
}

cat("\n=== A. Reaction at the nearest zone vs placebo (reached only) ===\n")
cat("cell = P(react | reached) zone / placebo, lift, name-clustered SE\n")
for (side in c("res", "sup")) {
  cat(sprintf("\n-- %s --  reach: zone %.3f  placebo %.3f\n", side,
      mean(AP$reached[AP$side == side & AP$kind == "zone"]),
      mean(AP$reached[AP$side == side & AP$kind == "placebo"])))
  for (w in WS) for (k in KS) {
    col <- sprintf("r_k%.1f_w%d", k, w)
    z <- AP[AP$side == side & AP$kind == "zone" & AP$reached == 1, ]
    p <- AP[AP$side == side & AP$kind == "placebo" & AP$reached == 1, ]
    b <- boot_diff(z[[col]], z$name, p[[col]], p$name)
    cat(sprintf("  k %.1f ATR  w %2d : zone %.3f  placebo %.3f  lift %+.3f (SE %.3f)  n %d/%d\n",
                k, w, mean(z[[col]]), mean(p[[col]]), b[["diff"]], b[["se"]], b[["n1"]], b[["n2"]]))
  }
}

cat("\n--- A by zone attribute (k 1.0, w 10; placebo = all placebos of the side) ---\n")
col <- "r_k1.0_w10"
for (side in c("res", "sup")) {
  p <- AP[AP$side == side & AP$kind == "placebo" & AP$reached == 1, ]
  z <- AP[AP$side == side & AP$kind == "zone" & AP$reached == 1, ]
  for (lab in c("touches 2", "touches >=3", "recent", "old", "dist <1 ATR", "dist 1-2 ATR", "dist >=2 ATR")) {
    sel <- switch(lab,
      "touches 2" = z$touches == 2, "touches >=3" = z$touches >= 3,
      "recent" = !is.na(z$age) & z$age <= RECENT, "old" = !is.na(z$age) & z$age > RECENT,
      "dist <1 ATR" = z$dist < 1, "dist 1-2 ATR" = z$dist >= 1 & z$dist < 2, "dist >=2 ATR" = z$dist >= 2)
    psel <- switch(lab, "dist <1 ATR" = p$dist < 1, "dist 1-2 ATR" = p$dist >= 1 & p$dist < 2,
                   "dist >=2 ATR" = p$dist >= 2, rep(TRUE, nrow(p)))
    zz <- z[sel, ]; pp <- p[psel, ]
    if (nrow(zz) < 30) next
    b <- boot_diff(zz[[col]], zz$name, pp[[col]], pp$name)
    cat(sprintf("  %-4s %-13s zone %.3f (n %4d)  placebo %.3f (n %4d)  lift %+.3f (SE %.3f)\n",
                side, lab, mean(zz[[col]]), nrow(zz), mean(pp[[col]]), nrow(pp), b[["diff"]], b[["se"]]))
  }
}

cat(sprintf("\n=== B. Long entered at t: +%.1f ATR before -%.1f ATR within %d sessions ===\n",
            FP_K, FP_K, HORIZON))
B$state <- ifelse(B$in_r & B$in_s, "in_both", ifelse(B$in_r, "in_resistance",
             ifelse(B$in_s, "in_support", "neither")))
base <- B[B$state == "neither", ]
cat(sprintf("  %-14s n %5d  P(win) %.3f  fwd10 %+.3f ATR\n", "neither", nrow(base),
            mean(base$win, na.rm = TRUE), mean(base$fwd10, na.rm = TRUE)))
for (s in c("in_resistance", "in_support", "in_both")) {
  x <- B[B$state == s, ]
  bw <- boot_diff(x$win, x$name, base$win, base$name)
  bf <- boot_diff(x$fwd10, x$name, base$fwd10, base$name)
  cat(sprintf("  %-14s n %5d  P(win) %.3f  (vs neither %+.3f, SE %.3f)  fwd10 %+.3f ATR (%+.3f, SE %.3f)\n",
              s, nrow(x), mean(x$win, na.rm = TRUE), bw[["diff"]], bw[["se"]],
              mean(x$fwd10, na.rm = TRUE), bf[["diff"]], bf[["se"]]))
}
x <- B[B$state != "neither", ]
bw <- boot_diff(x$win, x$name, base$win, base$name)
cat(sprintf("  %-14s n %5d  P(win) %.3f  (vs neither %+.3f, SE %.3f)   share of samples vetoed %.1f%%\n",
            "any zone", nrow(x), mean(x$win, na.rm = TRUE), bw[["diff"]], bw[["se"]],
            100 * nrow(x) / nrow(B)))
cat("\ndone\n")
