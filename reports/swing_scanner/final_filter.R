# final_filter.R — 2-gate composite ranking
#
# A stock reaches TRADE only if it passes both gates AND has a top-3 composite score.
# No artificial threshold — just gate + rank + cut.
#
# The 2 gates:
#   1. TECHNICAL ANALYSIS — stock score >= 5 (WATCH minimum) + sector gate passed
#      Note: scores are always computed regardless of sector gate.
#      Sector gate (ETF above MA20 + positive slope + ADX>20) is an additional filter.
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
    # Score must be >= 5 AND the sector gate must approve the direction
    score_ok <- tech_score[i] >= 5
    sect_ok  <- TRUE
    if ("Sector_Long_Gate" %in% names(out) && "Sector_Short_Gate" %in% names(out)) {
      long_viable  <- !is.na(out$Long_Score[i])  && out$Long_Score[i] >= 5  && isTRUE(out$Sector_Long_Gate[i])
      short_viable <- !is.na(out$Short_Score[i]) && out$Short_Score[i] >= 5 && isTRUE(out$Sector_Short_Gate[i])
      sect_ok <- long_viable || short_viable
    }
    out$Gate_Tech[i] <- score_ok && sect_ok

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

  # ── Trend-continuation rescue: promote SKIP → LONG WATCH when isTrendContinuation passes ──
  # These are confirmed stage-2 trends with an active pullback — high-quality 2nd/3rd-inning
  # setups that the raw tech score may have missed. Bypass the WATCH cap to always surface them.
  # Exclude if earnings within 10 days (option IV will be loaded with event premium, unfair entry).
  if ("Trend_Passes" %in% names(out)) {
    earnings_safe <- is.na(out$EarningsInDays) | out$EarningsInDays >= 10
    rescue_idx <- which(
      out$Best_Signal == "SKIP" &
      !is.na(out$Trend_Passes) & out$Trend_Passes == TRUE &
      earnings_safe
    )
    for (idx in rescue_idx) {
      out$Best_Signal[idx] <- "LONG WATCH"
      out$Long_Signal[idx] <- "WATCH"
      out$Rank[idx] <- "TREND CONT"   # marker — sorted between TOP PICK and regular WATCH
    }
    if (length(rescue_idx) > 0) {
      message(sprintf("Trend-continuation rescue: promoted %d SKIP → LONG WATCH (earnings-safe, trend-confirmed)",
                      length(rescue_idx)))
    }
    # Log tickers filtered out by earnings proximity for transparency
    earn_filtered <- which(
      out$Best_Signal == "SKIP" &
      !is.na(out$Trend_Passes) & out$Trend_Passes == TRUE &
      !is.na(out$EarningsInDays) & out$EarningsInDays < 10
    )
    if (length(earn_filtered) > 0) {
      message(sprintf("Trend-continuation: filtered out %d due to earnings <10d: %s",
                      length(earn_filtered),
                      paste(out$Ticker[earn_filtered], collapse = ", ")))
    }
  }

  # ── Sort: TOP PICK, TRADE, TREND CONT, WATCH, SKIP; group by sector within same tier ──────
  out$Sort <- ifelse(out$Rank == "TOP PICK", 0,
    ifelse(out$Best_Signal == "LONG TRADE", 1,
    ifelse(out$Best_Signal == "SHORT TRADE", 2,
    ifelse(out$Rank == "TREND CONT", 3,
    ifelse(out$Best_Signal == "LONG WATCH", 4,
    ifelse(out$Best_Signal == "SHORT WATCH", 5, 6))))))
  out <- out[order(out$Sort, out$Sector, -out$Composite), ]
  out$Sort <- NULL

  out
}
