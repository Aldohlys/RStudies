# cheap_score.R — Phase C: Cheap screen (Steps C.1 – C.3)
#
# Runs only on Phase B survivors (~15-30 names). Reuses gate3 vol_profile
# infrastructure for IV30/IV90/IV180/RV30/IVP_2y/VRP. Adds skew vs own
# history (1 pt) and cross-sectional sector rank (1 pt). Anti-noise N.1
# applies: when computing IVP_2y / VRP / term-structure ratios, use the
# 5-day median of daily-close values, not the live close.

#' Score the Cheap component (Step C.2) and apply C.3 cutoff.
#'
#' @param vol_row Single-row data.frame from gate3 evaluate_gate3()
#'                with IV15/IV30/IV90/IV180/RV30/IVP/IVP_2y/VRP columns
#' @param skew_history data.frame of (cache_date, skew_25d) for this sym
#'                     across the last ~12 months. NULL → skew_pt = 0.
#' @param sector_iv_median numeric — median IVP_2y across rich-universe peers
#'                         in same sector (NA → cross-sectional_pt = 0)
#' @param flow_direction "up", "down", or "neutral"
#' @return list: cheap_score, cheap_side, passes, components
score_cheap <- function(vol_row, skew_history = NULL, sector_iv_median = NA,
                        flow_direction = "neutral") {
  if (is.null(vol_row) || nrow(vol_row) == 0) {
    return(list(cheap_score = 0L, cheap_side = "neutral",
                passes = FALSE, ivp_2y = NA, vrp = NA,
                term_pct = NA, skew_pt = 0L, sector_pt = 0L))
  }

  # IVP source: prefer IBKR-native 1y IVP (`ivp`); fall back to IVP_2y if 1y
  # is unavailable. The 1y IVP is what TWS returns directly via IBKR API; the
  # 2y is a secondary computation that often lags or is NA for most tickers.
  ivp_1y <- if ("IVP" %in% names(vol_row)) vol_row$IVP else NA_real_
  ivp_2y <- if ("IVP_2y" %in% names(vol_row)) vol_row$IVP_2y else NA_real_
  ivp_used <- if (!is.na(ivp_1y)) ivp_1y else ivp_2y
  vrp    <- vol_row$VRP
  iv30   <- vol_row$IV30
  iv90   <- vol_row$IV90
  iv180  <- vol_row$IV180

  # ── IVP (4 pts) — primary cheap signal, using 1y IBKR-native if available ──
  ivp_pt <- if (!is.na(ivp_used)) {
    if (ivp_used < 25) 4L
    else if (ivp_used < 40) 3L
    else if (ivp_used < 60) 2L
    else if (ivp_used < 75) 1L
    else 0L
  } else 0L

  # ── VRP (2 pts) — IV vs realized; small/negative = cheap ──────────────
  vrp_pt <- if (!is.na(vrp)) {
    if (vrp <= 0) 2L
    else if (vrp <= 10) 1L
    else 0L
  } else 0L

  # ── Term structure (2 pts) — front cheaper than back = contango ───────
  term_pct <- if (!is.na(iv30) && !is.na(iv90) && iv90 > 0)
    round((iv30 - iv90) / iv90 * 100, 1) else NA
  term_pt <- if (!is.na(term_pct)) {
    if (term_pct < -5) 2L
    else if (term_pct < 0) 1L
    else 0L
  } else 0L

  # ── Skew vs own history (1 pt) — call/put side bid up vs own median ────
  # Sign convention: skew_25d = call25_iv - put25_iv. Positive = call skew
  # heavy (calls bid). When flow_direction = up, we want call skew NOT
  # already bid — so prefer skew below own historical median.
  skew_pt <- 0L
  if (!is.null(skew_history) && nrow(skew_history) >= 30 &&
      "skew_25d" %in% names(skew_history)) {
    median_skew <- median(skew_history$skew_25d, na.rm = TRUE)
    current_skew <- if ("skew_25d" %in% names(vol_row)) vol_row$skew_25d else NA
    if (!is.na(current_skew) && !is.na(median_skew)) {
      if (flow_direction == "up" && current_skew < median_skew) skew_pt <- 1L
      else if (flow_direction == "down" && current_skew > median_skew) skew_pt <- 1L
    }
  }

  # ── Cross-sectional rank within sector (1 pt) ──────────────────────────
  sector_pt <- if (!is.na(ivp_used) && !is.na(sector_iv_median)) {
    if (ivp_used < sector_iv_median) 1L else 0L
  } else 0L

  cheap_score <- min(10L, as.integer(ivp_pt + vrp_pt + term_pt +
                                     skew_pt + sector_pt))

  # Cheap_Side: derived from skew direction — calls cheap (skew light or
  # negative) vs puts cheap (skew heavy on calls means puts relatively cheap).
  cheap_side <- "neutral"
  if (!is.null(skew_history) && "skew_25d" %in% names(vol_row) &&
      !is.na(vol_row$skew_25d)) {
    median_skew <- median(skew_history$skew_25d, na.rm = TRUE)
    if (!is.na(median_skew)) {
      if (vol_row$skew_25d < median_skew - 0.005) cheap_side <- "call-cheap"
      else if (vol_row$skew_25d > median_skew + 0.005) cheap_side <- "put-cheap"
    }
  }

  # ── C.3 Cutoff ────────────────────────────────────────────────────────
  side_aligns <- (flow_direction == "up" && cheap_side != "put-cheap") ||
                 (flow_direction == "down" && cheap_side != "call-cheap") ||
                 (flow_direction == "neutral")
  passes <- cheap_score >= 6 && side_aligns

  list(
    cheap_score = cheap_score,
    cheap_side  = cheap_side,
    passes      = passes,
    ivp_used    = ivp_used,
    ivp_1y      = ivp_1y,
    ivp_2y      = ivp_2y,
    vrp         = vrp,
    term_pct    = term_pct,
    ivp_pt      = ivp_pt,
    vrp_pt      = vrp_pt,
    term_pt     = term_pt,
    skew_pt     = skew_pt,
    sector_pt   = sector_pt,
    flags = paste0("IVP(", ifelse(!is.na(ivp_1y), "1y", "2y"), "):",
                   ivp_pt, "/4 VRP:", vrp_pt, "/2 Term:", term_pt,
                   "/2 Skew:", skew_pt, "/1 SecRank:", sector_pt, "/1")
  )
}
