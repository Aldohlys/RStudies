# reports/shared/vehicle_rule.R — shared vehicle-selection rule.
#
# Single source of truth for "which option vehicle is appropriate?" — used
# by swing_scanner (Phase D selection) and /analyze (conditional rendering
# of the structures table). Rules:
#
#   price < $10                     → stock          (single-leg, low absolute risk)
#   atm_bid_ask% > 8                → stock          (option spread too wide)
#   price < $150 AND cheap_score≥7  → call           (outright, vol is cheap enough)
#   otherwise                       → spread         (vertical, IV-cost-control)
#
# Default DTE bucket per stage: early=35, continuation=25, else 30.

#' Pick vehicle and target expiry.
#' @param price numeric — current spot
#' @param cheap_score integer 0..10 (NA-tolerant: treated as 0)
#' @param stage "early" | "continuation" | other
#' @param atm_bid_ask_pct numeric — ATM bid-ask %; NA = unknown, treat as OK
#' @param direction "long" or "short". Default "long" for backwards compatibility
#'   with swing_scanner callers. The outright-option vehicle is labelled "call"
#'   for long, "put" for short — same selection logic, direction-correct label.
#' @return list(vehicle, target_dte, reason)
pick_vehicle_expiry <- function(price, cheap_score, stage,
                                atm_bid_ask_pct = NA,
                                direction = "long") {
  cs <- suppressWarnings(as.numeric(cheap_score))
  if (is.na(cs)) cs <- 0

  if (is.na(price)) {
    return(list(vehicle = NA_character_, target_dte = NA_integer_,
                reason = "spot price unavailable"))
  }

  outright_label <- if (identical(direction, "short")) "put" else "call"

  vehicle <- if (price < 10) "stock"
             else if (!is.na(atm_bid_ask_pct) && atm_bid_ask_pct > 8) "stock"
             else if (price < 150 && cs >= 7) outright_label
             else "spread"

  reason <- if (vehicle == "stock") {
    if (price < 10) sprintf("price $%.2f < $10 (stock vehicle)", price)
    else sprintf("ATM bid-ask %.1f%% > 8%% (option spread too wide)",
                 atm_bid_ask_pct)
  } else if (vehicle %in% c("call", "put")) {
    sprintf("price $%.2f < $150 and cheap_score %d ≥ 7 (outright %s)",
            price, as.integer(cs), vehicle)
  } else {
    sprintf("price $%.2f%s (vertical spread for IV cost control)",
            price,
            if (cs < 7) sprintf(", cheap_score %d < 7", as.integer(cs)) else "")
  }

  target_dte <- if (identical(stage, "early")) 35
                else if (identical(stage, "continuation")) 25
                else 30

  list(vehicle = vehicle, target_dte = as.integer(target_dte), reason = reason)
}
