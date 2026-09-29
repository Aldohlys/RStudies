# reports/shared/market_calendar.R — exchange sessions for the BOT tools.
#
# Holidays come from qlcal (the QuantLib calendars, packaged on their own);
# session hours are not in qlcal, so they are kept here, in the market's own
# time zone. Half-day sessions (day after Thanksgiving, Christmas Eve) are not
# modelled: the market reads as open for the full session.
#
# Used to skip option-data steps when the instrument's options market is closed
# (TODO 101: frozen or absent quotes, meaningless bid/ask) and to count missing
# sessions on the listing's own calendar rather than on weekdays.

MARKETS <- list(
  US       = list(cal = "UnitedStates/NYSE",      tz = "America/New_York", open = "09:30", close = "16:00"),
  EUREX    = list(cal = "Germany/Eurex",          tz = "Europe/Berlin",    open = "09:00", close = "17:30"),
  EURONEXT = list(cal = "France/Exchange",        tz = "Europe/Paris",     open = "09:00", close = "17:30"),
  XETRA    = list(cal = "Germany/Xetra",          tz = "Europe/Berlin",    open = "09:00", close = "17:30"),
  # qlcal's Switzerland is the settlement calendar: it lacks the SIX closures of
  # 24 and 31 December and 1 August.
  SIX      = list(cal = "Switzerland",            tz = "Europe/Zurich",    open = "09:00", close = "17:30"),
  LSE      = list(cal = "UnitedKingdom/Exchange", tz = "Europe/London",    open = "08:00", close = "16:30"),
  # CME Globex options: Sunday 17:00 to Friday 16:00 Chicago time, with a daily
  # 16:00-17:00 break. Holidays approximated by the NYSE calendar.
  CME      = list(cal = "UnitedStates/NYSE",      tz = "America/Chicago",  open = "17:00", close = "16:00",
                  overnight = TRUE)
)

.mkt_business_day <- function(market, date) {
  m <- MARKETS[[market]]
  if (is.null(m)) return(!format(as.Date(date), "%u") %in% c("6", "7"))
  if (!requireNamespace("qlcal", quietly = TRUE)) return(!format(as.Date(date), "%u") %in% c("6", "7"))
  qlcal::setCalendar(m$cal)
  qlcal::isBusinessDay(as.Date(date))
}

#' Is the market in session at a given instant?
#'
#' @param market key of MARKETS ("US", "EUREX", "EURONEXT", "XETRA", "SIX", "LSE", "CME")
#' @param at POSIXct instant (default now)
#' @return logical; TRUE for an unknown market, so an unmapped instrument is
#'   never blocked
market_is_open <- function(market, at = Sys.time()) {
  m <- MARKETS[[market]]
  if (is.null(m)) return(TRUE)
  local <- as.POSIXlt(at, tz = m$tz)
  d <- as.Date(format(local, "%Y-%m-%d"))
  hm <- as.numeric(format(local, "%H")) * 60 + as.numeric(format(local, "%M"))
  mins <- function(x) { p <- as.numeric(strsplit(x, ":")[[1]]); p[1] * 60 + p[2] }
  if (isTRUE(m$overnight)) {
    # The session opening at 17:00 belongs to the next business day's date.
    if (hm >= mins(m$close) && hm < mins(m$open)) return(FALSE)
    trade_day <- if (hm >= mins(m$open)) d + 1 else d
    return(isTRUE(.mkt_business_day(market, trade_day)))
  }
  isTRUE(.mkt_business_day(market, d)) && hm >= mins(m$open) && hm < mins(m$close)
}

#' Options market of an instrument, from its Tickers OptExchange and currency.
#'
#' @param opt_exchange Tickers.OptExchange ("SMART", "EUREX", "CME", "NYMEX"…)
#' @param currency Tickers.Currency
#' @return a MARKETS key
option_market_of <- function(opt_exchange, currency = "USD") {
  ox <- toupper(ifelse(is.na(opt_exchange), "", opt_exchange))
  cc <- toupper(ifelse(is.na(currency), "USD", currency))
  if (ox %in% c("EUREX", "DTB", "SOFFEX")) return("EUREX")
  if (ox %in% c("CME", "NYMEX", "COMEX", "CBOT", "GLOBEX", "ECBOT")) return("CME")
  if (ox %in% c("MONEP", "EURONEXT", "FTA", "BELFOX")) return("EURONEXT")
  if (ox %in% c("ICEEU", "LIFFE")) return("LSE")
  if (cc %in% c("EUR", "CHF")) return("EUREX")
  if (cc == "GBP") return("LSE")
  "US"
}

#' Listing market of a stock or ETF, from its Yahoo symbol suffix.
#'
#' @param yahoo Yahoo symbol ("TTE.PA", "UBSG.SW", "SIE.DE", "AAPL"…)
#' @return a MARKETS key
stock_market_of <- function(yahoo) {
  y <- toupper(ifelse(is.na(yahoo), "", yahoo))
  if (grepl("\\.(PA|AS|BR|LS|IR)$", y)) return("EURONEXT")
  if (grepl("\\.SW$", y)) return("SIX")
  if (grepl("\\.(DE|F)$", y)) return("XETRA")
  if (grepl("\\.L$", y)) return("LSE")
  "US"
}

#' Business days of a market strictly between two dates.
#'
#' @param market MARKETS key
#' @param from,to Dates; counts days d with from < d < to
#' @return integer
market_days_between <- function(market, from, to) {
  from <- as.Date(from); to <- as.Date(to)
  if (is.na(from) || is.na(to) || to - from <= 1) return(0L)
  days <- seq(from + 1, to - 1, by = "day")
  as.integer(sum(vapply(days, function(x) isTRUE(.mkt_business_day(market, x)), logical(1))))
}

#' Human-readable reason for a closed options market, for SKIPPED provenance.
options_closed_reason <- function(market, at = Sys.time()) {
  m <- MARKETS[[market]]
  local <- if (!is.null(m)) format(as.POSIXlt(at, tz = m$tz), "%a %H:%M %Z") else ""
  sprintf("options market closed (%s, local time %s)", market, local)
}
