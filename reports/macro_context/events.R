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
      "Non-Farm"   = "No new positions before 8:30 ET. Expect IV crush post-release. Widen stops or flatten short-dated.",
      "Interest Rate Decision" = "Fed decision — expect vol spike. No new positions until statement + press conference digested.",
      "Interest Rate Decisions" = "Central bank decision day — expect vol spike across affected currencies.",
      "FOMC"       = "FOMC minutes can shift rate expectations — watch for hawkish/dovish surprises.",
      "CPI"        = "Key inflation print — large surprise moves broad market. No directional bets before release.",
      "ISM Manufacturing PMI" = "First hard data of the month — sub-50 = contraction risk, moves broad market.",
      "Advance GDP" = "First GDP estimate — large miss can reprice growth expectations and move rates."
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
      "ADP"        = "NFP preview — watch for large surprise, do not trade directionally on ADP alone.",
      "ISM Services" = "Services > Manufacturing for GDP weight — watch for divergence.",
      "PCE"        = "Fed's preferred inflation gauge — surprise moves rate expectations.",
      "PPI"        = "Producer prices feed into CPI — watch for upstream inflation pressure.",
      "Unemployment Rate" = "Labor market gauge — rising rate can shift Fed stance.",
      "Hourly Earnings" = "Wage inflation proxy — hot print = hawkish Fed expectations.",
      "Consumer Confidence" = "Sentiment gauge — large miss can move equities and USD.",
      "Consumer Sentiment" = "UoM sentiment — watch expectations component for inflation outlook.",
      "Inflation Expectations" = "Inflation expectations — Fed watches this closely, surprise moves rates.",
      "Empire State" = "Regional manufacturing gauge — first data point of the month.",
      "Flash.*PMI"  = "Flash PMI — early growth signal, moves on large surprise.",
      "GDP"        = "GDP release — watch for revisions vs consensus.",
      "Retail Sales" = "Consumer spending gauge — drives 70% of GDP.",
      "Durable Goods" = "Capex proxy — watch core (ex-transport) for business investment signal.",
      "Interest Rate" = "Central bank decision — expect vol in affected currency.",
      "German.*CPI" = "Sets tone for Eurozone CPI — directional for EUR.",
      "CPI Flash"  = "Eurozone flash CPI — key ECB input, watch core vs headline.",
      "ZEW"        = "German economic sentiment — forward-looking indicator for EUR.",
      "Bank Holiday.*USD" = "US market holiday — thin liquidity, wider spreads.",
      "German Prelim GDP" = "Eurozone growth signal — watch for contraction risk."
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
      "Unemployment Claims" = "Weekly labor pulse — watch deviation from 4-week trend.",
      "Home Sales"    = "Housing activity gauge — watch for trend change.",
      "Construction"  = "Construction activity — secondary indicator.",
      "Halifax"       = "UK house prices — secondary for FX.",
      "Final.*PMI"    = "Final PMI — rarely revised, low impact unless large change from flash.",
      "Claimant Count" = "UK labor data — watch for trend change.",
      "Earnings Index" = "UK wage growth — feeds into BoE rate expectations.",
      "GDP m/m"       = "UK monthly GDP — watch for contraction signal.",
      "CPI.*GBP"      = "UK inflation — BoE rate expectations driver.",
      "Retail Sales.*GBP" = "UK consumer spending — secondary for GBP.",
      "Revised UoM"   = "Revised reading — rarely moves market unless large revision.",
      "Richmond"      = "Regional manufacturing — secondary indicator.",
      "Federal Budget" = "Budget balance — minimal direct market impact.",
      "Employment Cost" = "Labor cost — feeds into inflation outlook.",
      "OPEC"          = "OPEC meeting — watch for production quota changes, moves oil."
    )
  )
)

# Events to skip (no trading relevance)
SKIP_PATTERNS <- c(
  "^No relevant event",
  "^Bank Holiday(?!.*USD)",
  "^IMF Meetings"
)

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
        action <- "Monitor for surprise — adjust positioning if needed."
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

#' Fetch upcoming economic events for the next 7 days
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
    log_warn("No events fetched — report will show empty events section", namespace = "RStudies")
    return(list())
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
  filtered
}

# ── Fetch events at source time ──────────────────────────────────────────────
EVENTS <- fetch_events()
