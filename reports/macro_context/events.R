# events.R — Weekly events definition
#
# EDIT THIS FILE EVERY MONDAY with upcoming week's events.
# Each event has: date, event name, impact level, action recommendation.

# Source: https://www.forexfactory.com/calendar
EVENTS <- list(
  list(date = "Mar-24", event = "Flash Manufacturing & Services PMI (EUR/GBP/USD)",
       impact = "HIGH",
       action = "Cluster of PMIs — watch for divergence across regions, directional for EUR/USD."),
  list(date = "Mar-25", event = "UK CPI y/y",               impact = "HIGH",
       action = "Forecast 3.0% — relevant for GBP pairs and rate expectations."),
  list(date = "Mar-26", event = "US Unemployment Claims",    impact = "MODERATE",
       action = "Forecast 211K — labor market pulse, watch deviation from trend."),
  list(date = "Mar-27", event = "UK Retail Sales m/m",       impact = "MODERATE",
       action = "Forecast -0.3% — GBP sentiment, consumer spending gauge.")
)
