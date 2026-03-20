# history.R — Scanner history, transitions, persistence tracking, alerts
#
# Tracks scanner results across runs for cross-report comparison.

library(DBI)

#' Save current scanner results to history table
#' @param out data.frame of current scanner results
#' @param conn DBI connection
save_scanner_results <- function(out, conn) {
  .today <- as.character(Sys.Date())
  export <- data.frame(
    scan_date   = .today,
    ticker      = out$Ticker,
    sector      = out$Sector,
    best_signal = out$Best_Signal,
    long_score  = ifelse(is.na(out$Long_Score), 0L, out$Long_Score),
    short_score = ifelse(is.na(out$Short_Score), 0L, out$Short_Score),
    price       = out$Price,
    rs_vs_etf   = ifelse(is.na(out$RS_vs_ETF), 0, out$RS_vs_ETF),
    macro_note  = out$Macro_Note,
    stringsAsFactors = FALSE
  )
  tryCatch(dbExecute(conn, "DELETE FROM scanner_history WHERE scan_date = ?",
                     params = list(.today)), error = function(e) NULL)
  dbWriteTable(conn, "scanner_history", export, append = TRUE)
  message(sprintf("Scanner history saved: %d rows", nrow(export)))
}

#' Load most recent prior scanner results
#' @param conn DBI connection
#' @return data.frame or NULL
load_previous_results <- function(conn) {
  prev <- tryCatch(
    dbGetQuery(conn, "SELECT * FROM scanner_history
                      WHERE scan_date = (SELECT MAX(scan_date) FROM scanner_history WHERE scan_date < ?)",
               params = list(as.character(Sys.Date()))),
    error = function(e) NULL
  )
  if (!is.null(prev) && nrow(prev) > 0) prev else NULL
}

#' Compute transitions between runs
#' @param current data.frame of current results
#' @param previous data.frame of previous results (or NULL)
#' @return data.frame of transitions
compute_transitions <- function(current, previous) {
  if (is.null(previous)) return(NULL)

  .today <- as.character(Sys.Date())
  cur_sigs  <- setNames(current$Best_Signal, current$Ticker)
  cur_scores <- setNames(
    pmax(ifelse(is.na(current$Long_Score), 0L, current$Long_Score),
         ifelse(is.na(current$Short_Score), 0L, current$Short_Score)),
    current$Ticker
  )
  prev_sigs  <- setNames(previous$best_signal, previous$ticker)
  prev_scores <- setNames(
    pmax(previous$long_score, previous$short_score),
    previous$ticker
  )

  all_tickers <- union(names(cur_sigs), names(prev_sigs))
  transitions <- list()

  for (tk in all_tickers) {
    cur_s  <- if (tk %in% names(cur_sigs))  cur_sigs[tk]  else "GONE"
    prev_s <- if (tk %in% names(prev_sigs)) prev_sigs[tk] else "NEW"
    cur_sc  <- if (tk %in% names(cur_scores))  cur_scores[tk]  else 0
    prev_sc <- if (tk %in% names(prev_scores)) prev_scores[tk] else 0

    # Classify transition
    if (prev_s == "NEW") {
      ttype <- "NEW_ENTRY"
    } else if (cur_s == "GONE") {
      ttype <- "DISAPPEARED"
    } else {
      rank_order <- c("SKIP" = 0, "LONG WATCH" = 1, "SHORT WATCH" = 1,
                       "LONG TRADE" = 2, "SHORT TRADE" = 2)
      cur_rank  <- ifelse(cur_s  %in% names(rank_order), rank_order[cur_s],  0)
      prev_rank <- ifelse(prev_s %in% names(rank_order), rank_order[prev_s], 0)

      if (cur_rank > prev_rank)      ttype <- "UPGRADED"
      else if (cur_rank < prev_rank) ttype <- "DOWNGRADED"
      else                           ttype <- "UNCHANGED"
    }

    # Only record meaningful transitions
    if (ttype != "UNCHANGED") {
      transitions[[tk]] <- data.frame(
        transition_date = .today,
        ticker          = tk,
        prev_signal     = prev_s,
        new_signal      = cur_s,
        score_delta     = as.integer(cur_sc - prev_sc),
        transition_type = ttype,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(transitions) == 0) return(NULL)
  result <- do.call(rbind, transitions)

  # Save to DB
  conn <- Tdata::safe_db_connect()
  tryCatch(dbExecute(conn, "DELETE FROM scanner_transitions WHERE transition_date = ?",
                     params = list(.today)), error = function(e) NULL)
  dbWriteTable(conn, "scanner_transitions", result, append = TRUE)
  dbDisconnect(conn)

  result
}

#' Compute persistence bonus for stocks appearing as WATCH for 2+ consecutive runs
#' @param current data.frame of current results
#' @param previous data.frame of previous results (or NULL)
#' @return Named integer vector: ticker -> persistence_bonus (0 or 1)
compute_persistence <- function(current, previous) {
  bonus <- setNames(rep(0L, nrow(current)), current$Ticker)
  if (is.null(previous)) return(bonus)

  prev_watch <- previous$ticker[grepl("WATCH", previous$best_signal)]
  cur_watch  <- current$Ticker[grepl("WATCH", current$Best_Signal) | grepl("TRADE", current$Best_Signal)]

  persistent <- intersect(cur_watch, prev_watch)
  bonus[persistent] <- 1L
  bonus
}

#' Load active alerts from Alerts table
#' @param conn DBI connection
#' @return data.frame of active alerts or NULL
load_active_alerts <- function(conn) {
  alerts <- tryCatch(
    dbGetQuery(conn, "SELECT * FROM Alerts WHERE Active = 1"),
    error = function(e) NULL
  )
  if (!is.null(alerts) && nrow(alerts) > 0) alerts else NULL
}
