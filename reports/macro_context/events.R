# events.R — Weekly events definition
#
# EDIT THIS FILE EVERY MONDAY with upcoming week's events.
# Each event has: date, event name, impact level, action recommendation.

EVENTS <- list(
  list(date = "Mar-17", event = "FOMC Decision",           impact = "CRITIQUE",
       action = "Pas de nouveaux trades avant annonce \u2014 preparer 2 scenarios."),
  list(date = "Mar-18", event = "Conference Powell",        impact = "CRITIQUE",
       action = "Ton sur les cuts \u2014 binaire pour DXY/Gold/Energy."),
  list(date = "Mar-19", event = "CPI US (Feb)",             impact = "ELEVE",
       action = "Reduire taille de 50% la veille si position ouverte."),
  list(date = "Mar-20", event = "Philadelphia Fed",         impact = "MODERE",
       action = "Activite industrielle \u2014 pertinent pour XLI short."),
  list(date = "Mar-21", event = "Options expiry (3rd Fri)", impact = "ELEVE",
       action = "Volatilite accrue \u2014 pinning autour des strikes majeurs.")
)
