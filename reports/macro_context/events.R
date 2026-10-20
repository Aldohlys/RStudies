# events.R — Fetch upcoming economic events from Equals Money calendar
#
# Replaces manual weekly editing with automated fetch + classification.
# Events are filtered to the upcoming 7 days from the report run date.
#
# Source: https://equalsmoney.com/economic-calendar/<month>

library(rvest)
library(httr)
library(logger)

# ── Impact classification by keyword ─────────────────────────────────────────

IMPACT_RULES <- list(
  CRITICAL = list(
    patterns = c(
      "Non-Farm Employment Change \\(USD\\)",
      "Interest Rate Decision.*\\(USD",
      "Interest Rate Decisions.*USD",
      "FOMC",
      "Core CPI m/m.*\\(USD\\)",
      "CPI m/m.*\\(USD\\)",
      "CPI y/y.*\\(USD\\)",
      "ISM Manufacturing PMI \\(USD\\)",
      "Advance GDP"
    ),
    action_map = list(
      "Non-Farm"   = "US payrolls and unemployment rate, 8:30 ET. Not tested.",
      "FOMC.*Minutes|Minutes.*FOMC" = "FOMC minutes: not tested (the event study covers decisions only).",
      "Interest Rate Decision" = "What followed historically: see the 'FOMC decision' row.",
      "Interest Rate Decisions" = "Several central-bank decisions on the same day. Not tested.",
      "FOMC"       = "What followed historically: see the 'FOMC decision' row.",
      "CPI"        = "US consumer prices, 8:30 ET. Not tested.",
      "ISM Manufacturing PMI" = "US manufacturing survey (ISM), first major release of the month; below 50 = contraction. Not tested.",
      "Advance GDP" = "First estimate of US quarterly GDP. Not tested."
    )
  ),
  HIGH = list(
    patterns = c(
      "ADP Non-Farm",
      "ISM Services PMI",
      "ISM Manufacturing Prices",
      "Core PCE",
      "Core PPI|PPI m/m",
      "Unemployment Rate.*\\(USD\\)",
      "Average Hourly Earnings",
      "CB Consumer Confidence",
      "Prelim UoM Consumer Sentiment",
      "Prelim UoM Inflation Expectations",
      "Empire State Manufacturing",
      "Flash Manufacturing PMI.*USD",
      "Flash Services PMI.*USD",
      "Final GDP",
      "GDP Price Index",
      "Retail Sales.*\\(USD\\)",
      "Durable Goods",
      "Interest Rate Decision(?!.*USD)",
      "Interest Rate Decisions(?!.*USD)",
      "German Prelim CPI",
      "Core CPI Flash Estimate|CPI Flash Estimate",
      "Eurozone.*CPI|Core CPI.*\\(EUR\\)",
      "ZEW Economic Sentiment",
      "Bank Holiday.*USD",
      "German Prelim GDP"
    ),
    action_map = list(
      "ADP"        = "Private payrolls (ADP), two days before the official payrolls. Not tested.",
      "ISM Services" = "US services survey (ISM); services weigh more in GDP than manufacturing. Not tested.",
      "PCE"        = "US personal consumption prices, the inflation measure the Fed targets. Not tested.",
      "PPI"        = "US producer prices, an upstream input to consumer prices. Not tested.",
      "Unemployment Rate" = "US unemployment rate. Not tested.",
      "Hourly Earnings" = "US average hourly earnings (wage growth). Not tested.",
      "Consumer Confidence" = "Conference Board consumer confidence survey. Not tested.",
      "Consumer Sentiment" = "University of Michigan consumer sentiment survey. Not tested.",
      "Inflation Expectations" = "University of Michigan household inflation expectations. Not tested.",
      "Empire State" = "New York Fed regional manufacturing survey. Not tested.",
      "Flash.*PMI"  = "Flash (first-estimate) purchasing managers' survey. Not tested.",
      "GDP"        = "GDP release (later estimate or price index). Not tested.",
      "Retail Sales" = "US retail sales; consumer spending is about 70% of GDP. Not tested.",
      "Durable Goods" = "US durable goods orders; the ex-transport figure is a proxy for business investment. Not tested.",
      "Interest Rate" = "Non-US central-bank rate decision. Not tested.",
      "German.*CPI" = "German preliminary consumer prices, released before the eurozone figure. Not tested.",
      "CPI Flash"  = "Eurozone flash consumer prices, an ECB input. Not tested.",
      "ZEW"        = "ZEW survey of German financial analysts. Not tested.",
      "Bank Holiday.*USD" = "US market holiday. Not tested.",
      "German Prelim GDP" = "German preliminary GDP. Not tested."
    )
  ),
  MODERATE = list(
    patterns = c(
      "Unemployment Claims",
      "Pending Home Sales|New Home Sales|Existing Home Sales",
      "Construction PMI",
      "Halifax HPI",
      "Final Manufacturing PMI",
      "Final Services PMI",
      "Claimant Count",
      "Average Earnings Index",
      "GDP m/m.*GBP",
      "Unemployment Rate.*\\(GBP\\)",
      "CPI.*\\(GBP\\)",
      "Core CPI.*\\(GBP\\)",
      "Retail Sales.*\\(GBP\\)",
      "Revised UoM",
      "Richmond Manufacturing",
      "Federal Budget Balance",
      "Employment Cost Index",
      "OPEC"
    ),
    action_map = list(
      "Unemployment Claims" = "US weekly initial jobless claims. Not tested.",
      "Home Sales"    = "US home sales. Not tested.",
      "Construction"  = "Construction purchasing managers' survey. Not tested.",
      "Halifax"       = "UK house prices (Halifax). Not tested.",
      "Final.*PMI"    = "Final purchasing managers' survey (revision of the flash figure). Not tested.",
      "Claimant Count" = "UK jobless claims. Not tested.",
      "Earnings Index" = "UK average earnings (wage growth). Not tested.",
      "GDP m/m"       = "UK monthly GDP. Not tested.",
      "CPI.*GBP"      = "UK consumer prices. Not tested.",
      "Retail Sales.*GBP" = "UK retail sales. Not tested.",
      "Revised UoM"   = "Revised University of Michigan sentiment and inflation expectations. Not tested.",
      "Richmond"      = "Richmond Fed regional manufacturing survey. Not tested.",
      "Federal Budget" = "US monthly federal budget balance. Not tested.",
      "Employment Cost" = "US employment cost index (wages and benefits). Not tested.",
      "OPEC"          = "OPEC meeting on production quotas. Not tested."
    )
  )
)

# Events to skip (no trading relevance)
SKIP_PATTERNS <- c(
  "^No relevant event",
  "^Bank Holiday(?!.*USD)",
  "^IMF Meetings"
)

# ── Scheduled political events (not in the economic calendar) ────────────────
# Shown from `lead` days before the date.
# boost = FALSE: listed in the table but no catalyst boost to the US regime scores.
SCHEDULED_EVENTS <- list(
  list(date = as.Date("2026-10-04"), event = "Brazil general election, first round (BRL)", impact = "HIGH",
       lead = 30, boost = FALSE,
       action = "Sunday vote; Bovespa, BRL and EWZ first trade on the result on Monday. Runoff Oct-25 if no candidate tops 50%. Not tested."),
  list(date = as.Date("2026-10-25"), event = "Brazil presidential runoff (BRL)", impact = "HIGH",
       lead = 30, boost = FALSE,
       action = "Sunday vote; Bovespa, BRL and EWZ first trade on the result on Monday. Not tested."),
  list(date = as.Date("2026-11-03"), event = "US midterm elections (USD)", impact = "CRITICAL",
       lead = 30, boost = TRUE,
       action = "Control of Congress sets the fiscal path (deficits, Treasury issuance, term premium). Not tested.")
)

#' Scheduled events whose lead window covers today
scheduled_events <- function(today = Sys.Date()) {
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)
  due <- Filter(function(ev) ev$date >= today && ev$date <= today + ev$lead, SCHEDULED_EVENTS)
  lapply(due, function(ev) list(date = format(ev$date, "%b-%d"), event = ev$event, impact = ev$impact,
                                action = ev$action, boost = ev$boost))
}

# ── Parsing functions ────────────────────────────────────────────────────────

#' Classify impact level for an event name
#' @param event_name Character string
#' @return List with impact and action
classify_event <- function(event_name) {
  for (level in c("CRITICAL", "HIGH", "MODERATE")) {
    rules <- IMPACT_RULES[[level]]
    for (pat in rules$patterns) {
      if (grepl(pat, event_name, perl = TRUE)) {
        # Find matching action
        action <- "Not tested."
        for (key in names(rules$action_map)) {
          if (grepl(key, event_name, perl = TRUE)) {
            action <- rules$action_map[[key]]
            break
          }
        }
        return(list(impact = level, action = action))
      }
    }
  }
  NULL
}

#' Parse events from an Equals Money calendar page
#' @param month_name Lowercase month name (e.g., "april")
#' @param year Numeric year
#' @return List of event lists (date, event, impact, action)
parse_calendar_page <- function(month_name, year) {
  # Force C locale for English month abbreviation parsing (e.g., "Apr")
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)
  url <- sprintf("https://equalsmoney.com/economic-calendar/%s", month_name)
  resp <- tryCatch(
    GET(url, user_agent("Mozilla/5.0"), timeout(15)),
    error = function(e) { log_warn("Failed to fetch {url}: {e$message}", namespace = "RStudies"); NULL }
  )
  if (is.null(resp) || status_code(resp) != 200) {
    log_warn("Could not fetch calendar for {month_name} (status: {if (!is.null(resp)) status_code(resp) else 'timeout'})",
             namespace = "RStudies")
    return(list())
  }

  page <- read_html(content(resp, as = "text", encoding = "UTF-8"))
  wrappers <- page |> html_elements("div.event-wrapper")
  if (length(wrappers) == 0) {
    log_warn("No event wrappers found for {month_name}", namespace = "RStudies")
    return(list())
  }

  events <- list()
  for (w in wrappers) {
    day_dates <- w |> html_elements("div.day-date") |> html_text(trim = TRUE)
    if (length(day_dates) < 2) next
    date_str <- day_dates[2]  # e.g., "01-Apr"

    # Parse individual event-info divs
    event_infos <- w |> html_elements("div.event-info")
    for (ei in event_infos) {
      event_name <- ei |> html_element("a.event-name") |> html_text(trim = TRUE)
      if (is.na(event_name) || event_name == "") next

      # Skip irrelevant events
      skip <- FALSE
      for (sp in SKIP_PATTERNS) {
        if (grepl(sp, event_name, perl = TRUE)) { skip <- TRUE; break }
      }
      if (skip) next

      # Handle combined events (e.g., "Core CPI m/m & CPI m/m (USD)")
      # Keep as single event — the classification handles combined names
      classification <- classify_event(event_name)
      if (is.null(classification)) next

      # Convert date to "Mon-DD" format for compatibility with compute_catalyst_boost
      parsed_date <- tryCatch({
        as.Date(paste0(date_str, "-", year), format = "%d-%b-%Y")
      }, error = function(e) NA)
      if (is.na(parsed_date)) next

      formatted_date <- format(parsed_date, "%b-%d")  # e.g., "Apr-01"

      events[[length(events) + 1]] <- list(
        date    = formatted_date,
        event   = event_name,
        impact  = classification$impact,
        action  = classification$action
      )
    }
  }
  events
}

# ── Main fetch function ──────────────────────────────────────────────────────

#' Fetch upcoming economic events for the next 7 days, plus the FOMC decision and Mag 7 earnings rows
#' (event_history.R, next EVENT_LOOKAHEAD days, boost = FALSE)
#' @param today Date object (default: Sys.Date())
#' @return List of events in same format as previous hardcoded EVENTS
fetch_events <- function(today = Sys.Date()) {
  year <- as.integer(format(today, "%Y"))
  end_date <- today + 7

  # English month names (locale-independent)
  EN_MONTHS <- c("january", "february", "march", "april", "may", "june",
                 "july", "august", "september", "october", "november", "december")

  # Determine which month(s) to fetch
  current_month <- EN_MONTHS[as.integer(format(today, "%m"))]
  end_month <- EN_MONTHS[as.integer(format(end_date, "%m"))]
  months_to_fetch <- unique(c(current_month, end_month))

  log_info("Fetching events for: {paste(months_to_fetch, collapse = ', ')}", namespace = "RStudies")

  all_events <- list()
  for (m in months_to_fetch) {
    page_events <- parse_calendar_page(m, year)
    all_events <- c(all_events, page_events)
  }

  if (length(all_events) == 0) {
    log_warn("No events fetched — events section shows scheduled events only", namespace = "RStudies")
    return(sort_events(c(scheduled_events(today), event_history_rows(today)), today))
  }

  # Filter to next 7 days (force C locale for English month parsing)
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)

  filtered <- Filter(function(ev) {
    ev_date <- tryCatch(
      as.Date(paste0(ev$date, "-", year), format = "%b-%d-%Y"),
      error = function(e) NA
    )
    !is.na(ev_date) && ev_date >= today && ev_date <= end_date
  }, all_events)

  log_info("Events: {length(all_events)} total parsed, {length(filtered)} in next 7 days", namespace = "RStudies")
  sort_events(c(filtered, scheduled_events(today), event_history_rows(today)), today)
}

#' Order events by date; rows carrying a Date `d` use it (the 28-day rows can cross the year end)
sort_events <- function(evs, today = Sys.Date()) {
  if (!length(evs)) return(evs)
  old_lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  on.exit(Sys.setlocale("LC_TIME", old_lc), add = TRUE)
  year <- format(today, "%Y")
  ev_dates <- vapply(evs, function(ev) if (!is.null(ev[["d"]])) as.numeric(ev[["d"]])
                     else as.numeric(as.Date(paste0(ev$date, "-", year), format = "%b-%d-%Y")), 0)
  evs[order(ev_dates)]
}

# ── Fetch events at source time ──────────────────────────────────────────────
EVENTS <- fetch_events()
