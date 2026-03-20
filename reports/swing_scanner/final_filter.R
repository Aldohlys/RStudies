# final_filter.R — 2-gate composite ranking
#
# A stock reaches TRADE only if it passes both gates AND has a top-3 composite score.
# No artificial threshold — just gate + rank + cut.
#
# The 2 gates:
#   1. TECHNICAL ANALYSIS — sector gate passed + stock score >= 5 (WATCH minimum)
#   2. OPTIONALITY — vol profile exists AND Optionality >= PARTIAL
#
# Composite score = technical_score + persistence_bonus
#   - technical_score: raw long/short score (0-9)
#   - persistence_bonus: +1 if WATCH on previous run
#
# Macro stress: VIX > 25 warning banner in macro_context report (informational only).
# Backtested 2026-03-19: regime signals have no predictive power for trade outcomes.

#' Main entry: 2-gate composite ranking
#' @param out data.frame with all scored + optionality-enriched stocks
#' @param macro data.frame of today's macro context
#' @param persistence Named integer vector: ticker -> persistence_bonus
#' @param max_trades Max TRADE signals (default 3)
#' @param max_watch Max WATCH signals to display (default 10)
#' @return data.frame with Rank, gates, composite columns
final_rank <- function(out, macro, scenario_scores = NULL, persistence = NULL,
                       max_trades = 3, max_watch = 10) {

  if (is.null(persistence)) persistence <- setNames(rep(0L, nrow(out)), out$Ticker)

  # Initialize new columns
  out$Composite     <- 0
  out$Gate_Tech     <- TRUE
  out$Gate_Opt      <- TRUE
  out$All_Gates     <- TRUE
  out$Rank          <- ""

  # Technical base score (max of long/short)
  tech_score <- pmax(
    ifelse(is.na(out$Long_Score), 0L, out$Long_Score),
    ifelse(is.na(out$Short_Score), 0L, out$Short_Score)
  )

  for (i in seq_len(nrow(out))) {
    # ── Gate 1: Technical analysis ─────────────────────────────────────
    out$Gate_Tech[i] <- tech_score[i] >= 5

    # ── Gate 2: Optionality ────────────────────────────────────────────
    opt <- if ("Optionality" %in% names(out)) out$Optionality[i] else "NO DATA"
    out$Gate_Opt[i] <- opt %in% c("PASS", "PARTIAL")

    # ── All gates ──────────────────────────────────────────────────────
    out$All_Gates[i] <- out$Gate_Tech[i] && out$Gate_Opt[i]

    # ── Composite score ────────────────────────────────────────────────
    persist_bonus <- if (out$Ticker[i] %in% names(persistence)) persistence[out$Ticker[i]] else 0L
    out$Composite[i] <- tech_score[i] + persist_bonus
  }

  # ── Classify signals ────────────────────────────────────────────────────
  # Stocks passing both gates: rank by composite, top N = TRADE
  all_pass_idx <- which(out$All_Gates)

  if (length(all_pass_idx) > 0) {
    pass_composites <- out$Composite[all_pass_idx]
    ranked <- all_pass_idx[order(-pass_composites)]
    trade_idx <- head(ranked, max_trades)
    watch_idx <- setdiff(ranked, trade_idx)

    for (idx in trade_idx) {
      if (out$Best_Signal[idx] %in% c("LONG TRADE", "LONG WATCH")) {
        out$Best_Signal[idx]  <- "LONG TRADE"
        out$Long_Signal[idx]  <- "TRADE"
      } else {
        out$Best_Signal[idx]  <- "SHORT TRADE"
        out$Short_Signal[idx] <- "TRADE"
      }
      out$Rank[idx] <- "TOP PICK"
    }

    for (idx in watch_idx) {
      if (out$Best_Signal[idx] %in% c("LONG TRADE", "LONG WATCH")) {
        out$Best_Signal[idx]  <- "LONG WATCH"
        out$Long_Signal[idx]  <- "WATCH"
      } else {
        out$Best_Signal[idx]  <- "SHORT WATCH"
        out$Short_Signal[idx] <- "WATCH"
      }
    }
  }

  # Stocks failing any gate: demote to WATCH or SKIP
  fail_idx <- which(!out$All_Gates)
  for (idx in fail_idx) {
    if (out$Gate_Tech[idx]) {
      if (out$Best_Signal[idx] %in% c("LONG TRADE", "LONG WATCH")) {
        out$Best_Signal[idx]  <- "LONG WATCH"
        out$Long_Signal[idx]  <- "WATCH"
      } else if (out$Best_Signal[idx] %in% c("SHORT TRADE", "SHORT WATCH")) {
        out$Best_Signal[idx]  <- "SHORT WATCH"
        out$Short_Signal[idx] <- "WATCH"
      }
    } else {
      out$Best_Signal[idx] <- "SKIP"
      if (!is.na(out$Long_Signal[idx]) && out$Long_Signal[idx] %in% c("TRADE", "WATCH"))
        out$Long_Signal[idx] <- "SKIP"
      if (!is.na(out$Short_Signal[idx]) && out$Short_Signal[idx] %in% c("TRADE", "WATCH"))
        out$Short_Signal[idx] <- "SKIP"
    }
  }

  # ── Cap WATCH display ──────────────────────────────────────────────────
  watch_rows <- which(out$Best_Signal %in% c("LONG WATCH", "SHORT WATCH"))
  if (length(watch_rows) > max_watch) {
    ranked_watch <- watch_rows[order(-out$Composite[watch_rows])]
    drop <- setdiff(ranked_watch, head(ranked_watch, max_watch))
    out$Best_Signal[drop] <- "SKIP"
  }

  # ── Sort: TOP PICK first, then TRADE, WATCH, SKIP ──────────────────
  out$Sort <- ifelse(out$Rank == "TOP PICK", 0,
    ifelse(out$Best_Signal == "LONG TRADE", 1,
    ifelse(out$Best_Signal == "SHORT TRADE", 2,
    ifelse(out$Best_Signal == "LONG WATCH", 3,
    ifelse(out$Best_Signal == "SHORT WATCH", 4, 5)))))
  out <- out[order(out$Sort, -out$Composite), ]
  out$Sort <- NULL

  out
}
