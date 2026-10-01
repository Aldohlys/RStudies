# S-8 (TODO 82), step 2: what BOT_daily would have shown at each real trade's entry, using
# only history up to the entry date. Output: target, sess_target_p75 / p90,
# the session the underlying first touched the target, and the date of the
# p75 session (the winning-interval exit).
suppressPackageStartupMessages(library(Tdata))
SH <- "reports/shared"
for (f in c("indicators.R", "zones.R", "name_attributes.R")) source(file.path(SH, f))
S <- commandArgs(trailingOnly = TRUE)[1]
tr <- read.csv(file.path(S, "s8_trades.csv"), stringsAsFactors = FALSE)

fetch_adj <- function(sym) {
  d <- tryCatch(getSymIntervalDate(sym, as.Date("2014-01-01"), Sys.Date()), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 300) return(NULL)
  d <- d[is.finite(d$Close) & is.finite(d$High) & is.finite(d$Low), , drop = FALSE]
  if ("Adjusted" %in% names(d)) {               # same adjustment as bot_fetch_daily()
    f <- d$Adjusted / d$Close; f[!is.finite(f) | f <= 0] <- 1
    for (k in c("Open", "High", "Low", "Close")) d[[k]] <- d[[k]] * f
  }
  calc_ind(d)
}
close_coef90 <- function(d, n = 10) {          # ATR_MoveCoefHi construction, on history to date
  cl <- d$Close; a <- d$atr14; len <- length(cl)
  mv <- c(cl[(n + 1):len] - cl[1:(len - n)], rep(NA, n))
  z <- mv / (a * sqrt(n)); z <- z[is.finite(z)]
  if (length(z) < 250) NA_real_ else unname(stats::quantile(z, 0.90))
}
close_coef10 <- function(d, n = 10) {
  cl <- d$Close; a <- d$atr14; len <- length(cl)
  mv <- c(cl[(n + 1):len] - cl[1:(len - n)], rep(NA, n))
  z <- mv / (a * sqrt(n)); z <- z[is.finite(z)]
  if (length(z) < 250) NA_real_ else unname(stats::quantile(z, 0.10))
}

cache <- list(); out <- list()
for (i in seq_len(nrow(tr))) {
  t <- tr[i, ]; sym <- t$yahoo
  if (is.null(cache[[sym]])) cache[[sym]] <- fetch_adj(sym) %||% NA
  full <- cache[[sym]]
  base <- data.frame(TradeNr = t$TradeNr, yahoo = sym, direction = t$direction, stringsAsFactors = FALSE)
  if (!is.data.frame(full)) { out[[i]] <- cbind(base, note = "no history"); next }
  ed <- as.Date(as.character(t$entry), "%Y%m%d")
  d <- full[as.Date(full$date) <= ed, , drop = FALSE]
  after <- full[as.Date(full$date) > ed, , drop = FALSE]
  if (nrow(d) < 500) { out[[i]] <- cbind(base, note = "short history at entry"); next }
  long <- t$direction == "long"
  px <- tail(d$Close, 1); atr <- tail(d$atr14, 1)
  cm <- if (long) close_coef90(d) else abs(close_coef10(d))
  em_abs <- cm * atr * sqrt(10)
  zd <- d[as.Date(d$date) >= ed - round(2 * 365), , drop = FALSE]
  lr <- tryCatch(level_read(if (nrow(zd) >= 130) zd else d, atr, em_abs, direction = t$direction),
                 error = function(e) NULL)
  if (is.null(lr) || !is.finite(lr$target)) { out[[i]] <- cbind(base, note = "no target"); next }
  tc <- touch_coefs(d, n = 10)
  c75 <- if (long) tc$up75 else tc$dn75; c90 <- if (long) tc$up90 else tc$dn90
  dist <- (if (long) 1 else -1) * (lr$target - px) / atr
  s75 <- (dist / c75)^2; s90 <- (dist / c90)^2
  hit <- if (long) which(after$High >= lr$target) else which(after$Low <= lr$target)
  T75 <- max(1L, as.integer(ceiling(s75)))
  out[[i]] <- cbind(base, note = "", px = px, atr = atr, target = lr$target, target_source = lr$target_source,
                    stop = lr$stop_px, dist_atr = round(dist, 3), sess_p90 = round(s90, 1), sess_p75 = round(s75, 1),
                    touch_session = if (length(hit)) hit[1] else NA_integer_,
                    touch_date = if (length(hit)) as.character(as.Date(after$date[hit[1]])) else NA_character_,
                    exit_date_p75 = if (nrow(after) >= T75) as.character(as.Date(after$date[T75])) else NA_character_)
}
res <- do.call(rbind, lapply(out, function(x) { for (k in setdiff(names(out[[which.max(sapply(out, ncol))]]), names(x))) x[[k]] <- NA; x }))
write.csv(res, file.path(S, "s8_targets.csv"), row.names = FALSE)
cat("trades", nrow(res), "| with a target", sum(res$note == ""), "| notes:", paste(names(table(res$note)), table(res$note), collapse = "; "), "\n")
