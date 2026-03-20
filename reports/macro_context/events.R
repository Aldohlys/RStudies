# events.R — Weekly events definition
#
# EDIT THIS FILE EVERY MONDAY with upcoming week's events.
# Each event has: date, event name, impact level, action recommendation.

EVENTS <- list(
  list(date = "Mar-17", event = "FOMC Decision",           impact = "CRITICAL",
       action = "No new trades before announcement — prepare 2 scenarios."),
  list(date = "Mar-18", event = "Powell Press Conference",  impact = "CRITICAL",
       action = "Tone on cuts — binary for DXY/Gold/Energy."),
  list(date = "Mar-19", event = "CPI US (Feb)",             impact = "HIGH",
       action = "Reduce size by 50% the day before if position open."),
  list(date = "Mar-20", event = "Philadelphia Fed",         impact = "MODERATE",
       action = "Industrial activity — relevant for XLI short."),
  list(date = "Mar-21", event = "Options expiry (3rd Fri)", impact = "HIGH",
       action = "Elevated volatility — pinning around major strikes.")
)
