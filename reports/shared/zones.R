# reports/shared/zones.R — support/resistance zones, dynamic range position and
# Fibonacci levels for the BOT_daily level engine.
#
# Status: PROTOTYPE. Not yet wired into any pipeline — see
# docs/BOT_TOOLS_DESIGN.md WBS 2.2. Reproduce the published trial numbers with
# RStudies/bot_zones_trial.R.
#
# Design (docs/BOT_TOOLS_DESIGN.md §4):
#   - pivots come from a ZigZag on High/Low, NOT from a window extreme and NOT
#     from closes. The threshold is ATR-scaled: a fixed percentage over-pivots
#     high-ATR names (NVDA produced 180 pivots in 2y from a fixed 4%).
#   - a ZONE is a cluster of >= MIN_TOUCH pivots — "at least 2 attempts to
#     cross". Clustering is LEADER-based, most recent pivot first: a pivot joins
#     only if it is within cluster_k ATR of the zone SEED. Single-link
#     clustering chains transitively and produced 13-touch, 13%-wide zones.
#   - range position is measured between the live support and resistance zones,
#     so the lookback is whatever those zones need (33 to 467 sessions in the
#     trial), not a fixed window.
#   - Fibonacci is anchored on the swing pivot and returns LEVELS. Retracements
#     are support candidates, extensions are target candidates.
#
# ATR is supplied by the caller so this file cannot drift from calc_ind()'s
# `atr14` (TTR::ATR, n = 14) in indicators.R.

ZONE_DEFAULTS <- list(
  zone_k    = 0.35,   # zone padding each side, in ATR
  cluster_k = 0.60,   # join a zone if within this many ATR of the seed
  max_zone  = 1.50,   # hard cap on raw zone width, in ATR
  min_touch = 2L,     # "at least 2 attempts to cross"
  zz_atr    = 2.00,   # pivot needs a reversal of this many ATR
  zz_floor  = 0.025,  # ... but never less than 2.5%
  zz_ceil   = 0.100,  # ... and never more than 10%
  far_stop  = 3.00,   # beyond this many ATR a support zone is not a usable stop
  near_stop = 0.50    # nor is one closer than this: it sits inside the noise,
                      # and it collapses the denominator of asym (SNOW produced
                      # 18.5:1 from a .618 retracement a hair below spot)
)

#' ZigZag reversal threshold for a name, as a fraction.
#'
#' @param atr numeric ATR(14) in price
#' @param price numeric spot
#' @param cfg list of tuning constants, see ZONE_DEFAULTS
#' @return numeric fraction, e.g. 0.044
zz_threshold <- function(atr, price, cfg = ZONE_DEFAULTS) {
  if (!is.finite(atr) || !is.finite(price) || price <= 0) return(cfg$zz_floor)
  min(max(cfg$zz_atr * atr / price, cfg$zz_floor), cfg$zz_ceil)
}

#' ZigZag pivots on High/Low.
#'
#' A pivot is confirmed when price reverses by `th` from a running extreme. The
#' extreme itself is the pivot, so pivot prices are true intraday highs/lows.
#'
#' @param d data.frame with date, High, Low, Close (chronological)
#' @param th reversal threshold as a fraction, from zz_threshold()
#' @return data.frame(idx, price, type "H"/"L", date), or NULL
zigzag_pivots <- function(d, th) {
  n <- nrow(d)
  if (n < 5 || !is.finite(th) || th <= 0) return(NULL)
  piv <- list(); dir <- 0L; ext_i <- 1L; ext_p <- d$Close[1]
  for (i in 2:n) {
    if (dir >= 0) {
      if (d$High[i] >= ext_p) { ext_p <- d$High[i]; ext_i <- i }
      if (d$Low[i] <= ext_p * (1 - th)) {
        piv[[length(piv) + 1]] <- list(idx = ext_i, price = ext_p, type = "H")
        dir <- -1L; ext_p <- d$Low[i]; ext_i <- i
        next
      }
    }
    if (dir <= 0) {
      if (d$Low[i] <= ext_p) { ext_p <- d$Low[i]; ext_i <- i }
      if (d$High[i] >= ext_p * (1 + th)) {
        piv[[length(piv) + 1]] <- list(idx = ext_i, price = ext_p, type = "L")
        dir <- 1L; ext_p <- d$High[i]; ext_i <- i
      }
    }
  }
  if (!length(piv)) return(NULL)
  out <- do.call(rbind, lapply(piv, as.data.frame))
  out$date <- d$date[out$idx]
  out[!duplicated(out$idx), , drop = FALSE]
}

#' Cluster same-type pivots into zones (leader clustering, most recent first).
#'
#' @param piv data.frame from zigzag_pivots()
#' @param type "H" for resistance, "L" for support
#' @param atr numeric ATR(14) in price
#' @param cfg list of tuning constants
#' @return data.frame(lo, hi, mid, touches, first, last), or NULL
build_zones <- function(piv, type, atr, cfg = ZONE_DEFAULTS) {
  if (is.null(piv) || !is.finite(atr) || atr <= 0) return(NULL)
  p <- piv[piv$type == type, , drop = FALSE]
  if (!nrow(p)) return(NULL)
  p <- p[order(-p$idx), , drop = FALSE]
  used <- rep(FALSE, nrow(p)); zones <- list()
  for (i in seq_len(nrow(p))) {
    if (used[i]) next
    seed <- p$price[i]
    m <- !used &
      abs(p$price - seed) <= cfg$cluster_k * atr &
      abs(p$price - seed) <= cfg$max_zone * atr
    g <- p[m, , drop = FALSE]
    used[m] <- TRUE
    if (nrow(g) < cfg$min_touch) next
    zones[[length(zones) + 1]] <- data.frame(
      lo = min(g$price) - cfg$zone_k * atr,
      hi = max(g$price) + cfg$zone_k * atr,
      mid = mean(range(g$price)),
      touches = nrow(g),
      first = as.character(min(g$date)),
      last = as.character(max(g$date)),
      stringsAsFactors = FALSE)
  }
  if (!length(zones)) NULL else do.call(rbind, zones)
}

#' Nearest qualifying zone above (resistance) or below (support) spot.
#'
#' A zone that CONTAINS spot wins on both sides: it is the nearest level there
#' is, at distance zero. Selecting only on `lo > price` / `hi < price` dropped
#' it and reported no zone at all, which reads as "no overhead supply" when the
#' truth is the opposite — spot standing in the middle of the most-tested band
#' in the name's history (NTR, 5 touches, 2026-09-21).
#'
#' @param zones data.frame from build_zones(), or NULL
#' @param price numeric spot
#' @param side "res" or "sup"
#' @return one-row data.frame with an added `in_zone` flag, or NULL when there
#'   is no zone on that side and none containing spot
nearest_zone <- function(zones, price, side) {
  if (is.null(zones) || !nrow(zones)) return(NULL)

  beyond <- if (identical(side, "res")) {
    a <- zones[zones$lo > price, , drop = FALSE]
    if (nrow(a)) a[which.min(a$lo), , drop = FALSE] else NULL
  } else {
    b <- zones[zones$hi < price, , drop = FALSE]
    if (nrow(b)) b[which.max(b$hi), , drop = FALSE] else NULL
  }

  inside <- zones[zones$lo <= price & zones$hi >= price, , drop = FALSE]
  if (nrow(inside)) {
    # Overlapping zones are possible; take the most-tested, widest as tiebreak.
    o <- order(-inside$touches, -(inside$hi - inside$lo))
    cand <- inside[o[1], , drop = FALSE]
    # Distance zero does not make it the more informative level. KTOS stood in
    # a 2-touch band last touched 15 months earlier while a 4-touch wall sat
    # 5.2 ATR overhead; promoting the containing zone there collapsed asym from
    # 4.71 to 0.18. The containing zone wins only when it is at least as tested
    # as the one beyond spot.
    if (is.null(beyond) || cand$touches >= beyond$touches) {
      cand$in_zone <- TRUE
      return(cand)
    }
  }
  if (is.null(beyond)) return(NULL)
  beyond$in_zone <- FALSE
  beyond
}

#' The zone edge that is in play, given where spot stands.
#'
#' Resistance: the lower edge is first contact on the way up, but once spot is
#' inside the zone the level that matters is the upper edge — what price must
#' clear. Support mirrors it: the upper edge is first contact on the way down,
#' the lower edge is what must give way, and it is also where the stop already
#' went (below the whole zone), so the in-zone case changes nothing there.
#'
#' @param z one-row data.frame from nearest_zone(), or NULL
#' @param side "res" or "sup"
#' @return numeric level, or NA_real_ when z is NULL
zone_ref <- function(z, side) {
  if (is.null(z)) return(NA_real_)
  if (identical(side, "res")) {
    if (isTRUE(z$in_zone)) z$hi else z$lo
  } else {
    if (isTRUE(z$in_zone)) z$lo else z$hi
  }
}

#' Fibonacci retracements and extensions for the current leg.
#'
#' Anchor is the most recent confirmed swing LOW; the leg top is the highest
#' High since that pivot. Retracements are measured down from the leg high
#' (support candidates); extensions are projected up from the leg low, i.e. a
#' measured move (target candidates).
#'
#' @param d data.frame with High (chronological)
#' @param piv data.frame from zigzag_pivots()
#' @return list(leg_low, leg_high, leg, anchor_date, ret, ext), or NULL
fib_levels <- function(d, piv) {
  if (is.null(piv)) return(NULL)
  lows <- piv[piv$type == "L", , drop = FALSE]
  if (!nrow(lows)) return(NULL)
  a <- lows[which.max(lows$idx), , drop = FALSE]
  if (a$idx >= nrow(d)) return(NULL)
  leg_low <- a$price
  leg_high <- max(d$High[a$idx:nrow(d)], na.rm = TRUE)
  leg <- leg_high - leg_low
  if (!is.finite(leg) || leg <= 0) return(NULL)
  list(
    leg_low = leg_low, leg_high = leg_high, leg = leg,
    anchor_date = as.character(a$date),
    ret = c("0.382" = leg_high - leg * 0.382,
            "0.500" = leg_high - leg * 0.500,
            "0.618" = leg_high - leg * 0.618),
    ext = c("1.272" = leg_low + leg * 1.272,
            "1.618" = leg_low + leg * 1.618))
}

#' Full level read for one name.
#'
#' Two level systems, built differently and doing different jobs. They are kept
#' separate on purpose:
#'
#'   S/R ZONES  — clusters of pivots, width k*ATR, validated by REPETITION
#'                (>= min_touch attempts to cross). These say where price has
#'                actually struggled.
#'   FIBONACCI  — geometric, off the LOCAL min/max of the current leg, validated
#'                by nothing. Extensions are measured moves -> exit TARGET.
#'                Retracements -> exit STOP zone.
#'
#' Agreement between the two is the high-confidence case, so both are emitted
#' along with `target_agree` / `stop_agree` rather than being collapsed into one
#' number. The zone target is preferred when present because it is the validated
#' one; the Fib extension carries a name that has no overhead supply at all.
#'
#' @param d data.frame with date, High, Low, Close (chronological)
#' @param atr numeric ATR(14) in price, from calc_ind()'s atr14
#' @param em_upper numeric upper edge of the N-session expected move, in price
#'   (absolute, not percent); NA when unavailable
#' @param cfg list of tuning constants
#' @return list with both level systems, the chosen target/stop and the readings
level_read <- function(d, atr, em_upper = NA_real_, cfg = ZONE_DEFAULTS) {
  price <- tail(d$Close, 1)
  th <- zz_threshold(atr, price, cfg)
  piv <- zigzag_pivots(d, th)
  zones_h <- build_zones(piv, "H", atr, cfg)
  zones_l <- build_zones(piv, "L", atr, cfg)
  res <- nearest_zone(zones_h, price, "res")
  sup <- nearest_zone(zones_l, price, "sup")
  fb <- fib_levels(d, piv)

  # Containment is tested against EVERY zone, not the selected one. A zone that
  # contains spot can lose the touches test in nearest_zone() and never be
  # selected (KTOS, 2026-09-21), and price would still be sitting in it.
  in_res_zone <- !is.null(zones_h) && any(zones_h$lo <= price & zones_h$hi >= price)
  in_sup_zone <- !is.null(zones_l) && any(zones_l$lo <= price & zones_l$hi >= price)

  # ── Targets: repetition-validated zone, and the geometric measured move ──
  # With spot inside the zone the target is its upper edge — the level price
  # must clear — rather than the lower edge it is already past.
  res_in <- !is.null(res) && isTRUE(res$in_zone)
  target_zone <- zone_ref(res, "res")
  target_fib <- if (!is.null(fb)) unname(fb$ext[1]) else NA_real_
  target_fib_far <- if (!is.null(fb)) unname(fb$ext[2]) else NA_real_
  target <- if (is.finite(target_zone)) target_zone else target_fib
  target_source <- if (is.finite(target_zone)) (if (res_in) "in_zone" else "zone") else
    if (is.finite(target_fib)) "fib_ext" else "none"

  # ── Stops: support zone, and the retracement band of the current leg ──
  # The nearest repetition-validated support can sit so far below that no one
  # would place a stop there (MSFT: 6.6 ATR in the trial), so fall back in order
  # zone -> retracement -> plain ATR, and say which was used.
  # A usable stop is far enough below spot to sit outside the noise and near
  # enough to be worth placing.
  usable <- function(lvl) is.finite(lvl) && lvl < price &&
    (price - lvl) >= cfg$near_stop * atr && (price - lvl) <= cfg$far_stop * atr
  # The stop was always the zone's lower edge — below the whole band — so the
  # in-zone case changes the level not at all, only whether the zone is seen.
  # The stop goes below the whole band either way, so it is `lo` whether spot is
  # above the zone or inside it. zone_ref() is the DISTANCE reference (first
  # contact), which is a different level — do not wire the stop to it.
  sup_in <- !is.null(sup) && isTRUE(sup$in_zone)
  stop_zone <- if (!is.null(sup)) sup$lo else NA_real_
  zone_usable <- !is.null(sup) && usable(stop_zone)
  stop_fib <- if (!is.null(fb)) unname(fb$ret[3]) else NA_real_   # .618, deepest
  fib_usable <- usable(stop_fib)
  if (zone_usable) {
    stop_px <- stop_zone; stop_source <- if (sup_in) "in_zone_stop" else "zone_stop"
  } else if (fib_usable) {
    stop_px <- stop_fib; stop_source <- "fib_retr_stop"
  } else {
    stop_px <- price - cfg$far_stop * atr; stop_source <- "atr_stop"
  }

  asym <- if (is.finite(target) && is.finite(stop_px) && price > stop_px)
    (target - price) / (price - stop_px) else NA_real_
  asym_fib <- if (is.finite(target_fib) && is.finite(stop_fib) && price > stop_fib)
    (target_fib - price) / (price - stop_fib) else NA_real_

  span_hi <- if (!is.null(res)) res$mid else target
  rng_dyn <- if (!is.null(sup) && is.finite(span_hi) && span_hi > sup$mid)
    (price - sup$mid) / (span_hi - sup$mid) * 100 else NA_real_

  back <- suppressWarnings(min(as.Date(c(
    if (!is.null(res)) res$first else NA,
    if (!is.null(sup)) sup$first else NA)), na.rm = TRUE))

  list(
    price = price, atr = atr, zz_th = th, n_pivots = if (is.null(piv)) 0L else nrow(piv),
    res = res, sup = sup, fib = fb,
    in_res_zone = in_res_zone, in_sup_zone = in_sup_zone,
    target = target, target_source = target_source,
    target_zone = target_zone, target_fib = target_fib, target_fib_far = target_fib_far,
    stop_px = stop_px, stop_source = stop_source,
    stop_zone = stop_zone, stop_fib = stop_fib,
    asym = asym, asym_fib = asym_fib, rng_dyn = rng_dyn,
    zone_window_from = if (is.finite(back)) as.character(back) else NA_character_,
    zone_window_sessions = if (is.finite(back)) sum(d$date >= back) else NA_integer_,
    target_pct_of_em = if (is.finite(em_upper) && em_upper > 0 && is.finite(target))
      (target - price) / em_upper * 100 else NA_real_,
    fib_confirms_res = if (!is.null(res) && !is.null(fb))
      unname(fb$ext[1] >= res$lo && fb$ext[1] <= res$hi) else NA,
    fib_confirms_sup = if (!is.null(sup) && !is.null(fb))
      any(fb$ret >= sup$lo & fb$ret <= sup$hi) else NA)
}
