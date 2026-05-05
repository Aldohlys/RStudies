# classify.R — Phase E: Classification (Steps E.1 – E.2)
#
# Entry interval state already computed in setup_chain_rr.R::classify_entry_state.
# This module applies the TOP PICK / WATCH / SKIP gate (E.2) and Phase_Of_Drop
# tagging across all evaluated rows.

#' Classify each row into TOP PICK / WATCH / SKIP and tag phase of drop.
#'
#' @param df data.frame with all Phase A-D columns + entry_state
#' @param rr_min numeric — calibrated R:R_min threshold
#' @return df with rank, phase_of_drop columns added/updated
classify_final <- function(df, rr_min = 0.5) {
  df$rank <- "SKIP"
  df$phase_of_drop <- ""

  for (i in seq_len(nrow(df))) {
    # Phase A
    if (isTRUE(df$rich_pass[i]) == FALSE) {
      df$phase_of_drop[i] <- "A"
      next
    }
    # Phase B
    if (isTRUE(df$pull_pass[i]) == FALSE) {
      df$phase_of_drop[i] <- "B"
      next
    }
    # Phase C
    if (isTRUE(df$cheap_pass[i]) == FALSE) {
      df$phase_of_drop[i] <- "C"
      next
    }
    # Phase D individual sub-gate failures → WATCH (all A+B+C passed)
    targets_ok <- !is.na(df$targets_agreeing[i]) && df$targets_agreeing[i] >= 2
    rr_ok      <- !is.na(df$rr[i]) && df$rr[i] >= rr_min
    entry_ok   <- !is.na(df$entry_state[i]) && df$entry_state[i] == "IN BAND"
    chain_ok   <- !is.na(df$chain_state[i]) &&
                  (df$chain_state[i] != "chain-capped" ||
                   (df$chain_state[i] == "chain-capped" && rr_ok))

    n_d_pass <- sum(c(targets_ok, rr_ok, entry_ok, chain_ok))

    if (n_d_pass == 4) {
      df$rank[i] <- "TOP PICK"
      df$phase_of_drop[i] <- ""
    } else {
      df$rank[i] <- "WATCH"
      df$phase_of_drop[i] <- paste0("D[",
        paste(c(if(!targets_ok) "T", if(!rr_ok) "R", if(!entry_ok) "E",
                if(!chain_ok) "C"), collapse = ""), "]")
    }
  }

  # Sector cluster badge: ≥2 TOP PICK from same sector
  if (any(df$rank == "TOP PICK")) {
    top_pick_sectors <- df$sector[df$rank == "TOP PICK"]
    cluster_sectors <- names(table(top_pick_sectors))[table(top_pick_sectors) >= 2]
    df$sector_cluster <- df$sector %in% cluster_sectors & df$rank == "TOP PICK"
  } else {
    df$sector_cluster <- FALSE
  }

  df
}
