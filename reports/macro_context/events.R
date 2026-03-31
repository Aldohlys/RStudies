# events.R — Weekly events definition
#
# EDIT THIS FILE EVERY MONDAY with upcoming week's events.
# Each event has: date, event name, impact level, action recommendation.

# Source: https://www.forexfactory.com/calendar
EVENTS <- list(
  list(date = "Mar-30", event = "German Prelim CPI m/m (EUR)",
       impact = "HIGH",
       action = "Sets tone for Eurozone CPI next day — directional for EUR."),
  list(date = "Mar-31", event = "Eurozone Flash CPI y/y + Core CPI (EUR)",
       impact = "HIGH",
       action = "Key ECB input — watch core vs headline divergence, EUR positioning."),
  list(date = "Mar-31", event = "CB Consumer Confidence (USD)",
       impact = "HIGH",
       action = "Sentiment gauge — large miss can move equities and USD."),
  list(date = "Apr-01", event = "ISM Manufacturing PMI + Prices (USD)",
       impact = "CRITICAL",
       action = "First hard data for March — sub-50 = contraction risk, moves broad market."),
  list(date = "Apr-01", event = "ADP Non-Farm Employment (USD)",
       impact = "HIGH",
       action = "NFP preview — watch for large surprise, do not trade directionally on ADP alone."),
  list(date = "Apr-02", event = "US Unemployment Claims (USD)",
       impact = "MODERATE",
       action = "Weekly labor pulse — watch deviation from 4-week trend."),
  list(date = "Apr-03", event = "Non-Farm Payrolls + Unemployment Rate (USD)",
       impact = "CRITICAL",
       action = "No new positions before 8:30 ET. Expect IV crush post-release. Widen stops or flatten short-dated."),
  list(date = "Apr-03", event = "ISM Services PMI (USD)",
       impact = "HIGH",
       action = "Released same day as NFP — double volatility risk. Services > Manufacturing for GDP weight."),
  list(date = "Apr-03", event = "Good Friday — EU/UK/AU/NZ/CA/CH markets closed",
       impact = "HIGH",
       action = "Thin liquidity outside US — NFP moves amplified, wider spreads on FX crosses.")
)
