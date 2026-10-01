# archetypes.R — match today's cross-asset moves against known macro scenarios
#
# Each scenario is a fingerprint: the direction each asset typically takes when that
# "movie" is playing (signed weight; 2 = core tell, 1 = usual, 0.5 = secondary).
# Today's move per asset is a z-score: 21-day change / (daily volatility x sqrt(21)),
# so a 2% dollar move and a 6% oil move are compared on the same scale.
# Match = weighted agreement between fingerprint and today's z-scores, -100% to +100%.
# No scenario repeats exactly: the report lists the assets that contradict the fingerprint.

# Fingerprint assets. key -> list(symbol, sign, label). sign -1 inverts the quote
# (e.g. USD/JPY falling = yen strength). Bond-price proxies are inverted to read as yields.
FP_ASSETS <- list(
  USD     = list("DX-Y.NYB", 1, "US dollar"),
  EUR     = list("EURUSD=X", 1, "Euro"),
  GBP     = list("GBPUSD=X", 1, "Sterling"),
  JPY     = list("USDJPY=X", -1, "Yen"),
  CHF     = list("USDCHF=X", -1, "Swiss franc"),
  AUD     = list("AUDUSD=X", 1, "Australian dollar"),
  CAD     = list("USDCAD=X", -1, "Canadian dollar"),
  EMFX    = list(c("USDMXN=X", "USDKRW=X", "USDBRL=X"), -1, "EM currencies (MXN, KRW, BRL)"),
  CNY     = list("CNY=X", -1, "Yuan"),
  US10Y   = list("^TNX", 1, "US 10-year yield"),
  CURVE   = list("^TNX-^IRX", 1, "US curve (10y - 3m)"),
  BUND    = list("IS0L.DE", -1, "German long yields"),
  GILT    = list("GLTL.L", -1, "UK long yields"),
  MOVE    = list("^MOVE", 1, "Bond volatility (MOVE)"),
  CREDIT  = list("HYG/IEF", 1, "Credit (high yield vs Treasuries)"),
  OIL     = list("CL=F", 1, "Crude oil"),
  GOLD    = list("GC=F", 1, "Gold"),
  COPPER  = list("HG=F", 1, "Copper"),
  SPX     = list("^GSPC", 1, "S&P 500"),
  BREADTH = list("RSP/^GSPC", 1, "Equal weight vs S&P 500"),
  NDXREL  = list("^NDX/^GSPC", 1, "Nasdaq 100 vs S&P 500"),
  CONS    = list("XLY/XLP", 1, "Discretionary vs staples"),
  VIX     = list("^VIX", 1, "VIX"),
  EMEQ    = list("EEM", 1, "Emerging equities"),
  NIKKEI  = list("^N225", 1, "Nikkei"),
  BTC     = list("BTC-USD", 1, "Bitcoin")
)

ARCHETYPES <- list(
  list(
    id = "dash_for_cash", name = "Dash for cash (dollar liquidity squeeze)",
    fp = c(USD = 2, EMFX = -2, AUD = -1, CAD = -1, EUR = -1, JPY = 1, GOLD = -1, OIL = -1, COPPER = -1,
           CREDIT = -2, SPX = -2, VIX = 2, MOVE = 1, EMEQ = -2, BTC = -1),
    movie = "Everyone needs dollars at once. Leveraged holders sell what they can, not what they want: gold and even Treasuries are sold alongside equities, so the usual hedges fail. Credit spreads gap wider, EM and commodity currencies collapse, the dollar spikes.",
    analogs = "Q4 2008 (DXY +20% from July to November, gold -25% from March to October despite the crisis); March 2020 (DXY 94.6 to 102.8 in ten days, gold -12%, 10-year yield doubled from 0.54% as Treasuries were sold for cash).",
    after = "Both episodes ended with a policy backstop: unlimited Fed swap lines (October 2008, 15-19 March 2020) and QE. After the backstop the reversal was violent: gold and EM bottomed first, equities followed within days.",
    tells = "Gold falling together with equities; Treasuries failing as a hedge; VIX in backwardation; Fed swap-line usage; headlines on cross-currency basis or money-market funds.",
    invalid = "Gold rising with the dollar (that is fear, not funding stress), or credit spreads stable.",
    bot = list(long = character(0), short = c("EEM", "XME", "COPX", "KWEB", "FXI", "XLY", "KRE", "IAI"),
               note = "Shorts work until the backstop, then cover fast: time stop of days, not weeks. No new long breakouts until gold and EM currencies turn up.")
  ),
  list(
    id = "dollar_wrecking_ball", name = "Dollar wrecking ball (global tightening)",
    fp = c(USD = 2, EUR = -1, GBP = -1, JPY = -1, AUD = -1, EMFX = -1, CNY = -1, US10Y = 2, BUND = 1, GILT = 1,
           MOVE = 2, OIL = 0.5, GOLD = -1, COPPER = -1, SPX = -1, NDXREL = -1, CREDIT = -1, VIX = 1),
    movie = "US rates rise faster than the rest of the world, so capital flows into dollars. Every other currency weakens and has to defend itself; dollar debt and dollar-priced oil become more expensive for the rest of the world, which tightens global financial conditions. Bonds sell off everywhere, long-duration assets reprice lower.",
    analogs = "2022 (Fed +425 bp, DXY peaked at 114.8 on 28 September; the same week the Bank of England rescued the gilt market from the LDI pension crisis and Japan intervened for the yen for the first time since 1998); 1997-98 (strong dollar broke Asian currency pegs); 2014-15 (dollar +25%, oil and EM collapse).",
    after = "It lasts until something breaks or a central bank blinks: in 2022 the top came with the Bank of Japan and Bank of England interventions in late September and the first signs of the Fed slowing (smaller hikes from December). The turn in the dollar marked the low in gold, EM and equities in October-November 2022.",
    tells = "Interventions by other central banks (Japan, UK, China fixing); MOVE above 120; oil importers' currencies weakest; dollar strength against the yen.",
    invalid = "Dollar rising while US yields fall (that is risk-off flight, see dash for cash) or Bunds and gilts rallying while Treasuries sell.",
    bot = list(long = c("XOP", "OIH", "XLE", "PPH", "XLP"), short = c("EEM", "XME", "COPX", "LIT", "KWEB", "ITB", "VNQ", "IGV"),
               note = "In 2022 only energy, staples and healthcare made money. Long breakouts outside energy fail; duration (homebuilders, REITs, software) is the cleanest short.")
  ),
  list(
    id = "bond_rout", name = "Bond rout / term-premium shock",
    fp = c(US10Y = 2, CURVE = 1, BUND = 1.5, GILT = 1.5, MOVE = 1.5, USD = 1, GOLD = -1, SPX = -1, BREADTH = -1,
           EMFX = -1, CREDIT = -0.5),
    movie = "Bond investors demand more yield to hold long debt: fiscal deficits, heavy issuance or a central bank seen as behind. Long yields rise faster than short ones (bear steepening) across the US, Europe and the UK at the same time. Equities fall because the discount rate rises, not because growth weakens.",
    analogs = "1994 (Fed 3% to 6%, 10-year 5.6% to 8%, ended with Orange County and the Mexican peso crisis); 2013 taper tantrum (10-year 1.6% to 3.0% in four months, EM 'fragile five' and gold crushed); August-October 2023 (10-year to 5.0%, S&P -10%, ended when the Treasury cut long-bond issuance on 1 November).",
    after = "Bond routs end when the issuer or the central bank reacts (issuance mix, buybacks, verbal intervention) or when the yield rise itself causes a growth scare. The turn in yields is usually the turn in equities and in rate-sensitive sectors.",
    tells = "30-year leading the move; weak Treasury auctions (tails); rising term premium without rising inflation expectations; MOVE elevated.",
    invalid = "Yields rising with copper/gold and breadth (that is reflation, growth-driven, equity-friendly).",
    bot = list(long = character(0), short = c("ITB", "VNQ", "IGV", "KRE", "XLP"),
               note = "Few clean longs; the short side is duration. Watch Treasury refunding dates and auctions as event risk.")
  ),
  list(
    id = "stagflation", name = "Stagflation / oil supply shock",
    fp = c(OIL = 2, US10Y = 1, GOLD = 1, CURVE = -1, SPX = -1, BREADTH = -1, CONS = -1.5, VIX = 1, EMFX = -1,
           CREDIT = -1),
    movie = "Energy prices rise for supply reasons, not demand. Inflation goes up while growth goes down; the central bank cannot cut. The consumer is squeezed (discretionary underperforms staples), margins compress, and the curve flattens as policy stays tight.",
    analogs = "1973-74 (oil quadrupled, S&P -48%, gold up strongly); 1979-80 (Iran, gold to $850, Volcker); August 1990 (Iraq invades Kuwait, oil doubles, recession); H1 2008 (oil to $147 in July just before the crash); H1 2022 (Russia-Ukraine).",
    after = "Supply shocks end in demand destruction: oil peaks, then growth slows and the regime hands over to a growth scare or recession. In 1990 and 2008 the oil peak preceded the equity low by months.",
    tells = "Oil up while copper falls; inflation expectations (TIPS vs Treasuries) up; discretionary vs staples down; consumer sentiment down.",
    invalid = "Oil up with copper and breadth up (demand-driven reflation).",
    bot = list(long = c("XOP", "OIH", "FCG", "XLE", "GDX", "DBA", "MOO"), short = c("XLY", "XRT", "CARZ", "ITB", "IGV"),
               note = "Energy and real-asset breakouts work; consumer and long-duration shorts work. Exit energy longs on the first sign of demand destruction (oil down while copper down).")
  ),
  list(
    id = "reflation", name = "Reflation / global recovery",
    fp = c(USD = -1, AUD = 1, EMFX = 1, CNY = 1, COPPER = 2, OIL = 1, US10Y = 1, CURVE = 1, SPX = 1, BREADTH = 2,
           CREDIT = 1, VIX = -1, EMEQ = 1, CONS = 1),
    movie = "Growth is broadening across the world: commodity and EM currencies rise, copper leads, yields rise for good reasons, and the average stock beats the index. Cyclicals, small caps and value lead.",
    analogs = "2003-06 (weak dollar, commodity super-cycle); 2009-10; 2016-17 (China stimulus, then US election, copper surge in November 2016); November 2020-2021 (vaccine rotation, small caps +18% in November 2020).",
    after = "Reflation runs until yields rise enough to bite (rates up faster than growth) or the central bank tightens; then the leadership rotates back to quality.",
    tells = "Copper/gold up with yields; equal weight beating cap weight; AUD/JPY up; high yield beating Treasuries.",
    invalid = "Yields up with copper/gold down (term premium, not growth).",
    bot = list(long = c("XME", "COPX", "SLX", "XLB", "EEM", "FXI", "KRE", "OIH", "XOP"), short = character(0),
               note = "The best environment for long breakouts across many sectors. Shorts in defensives lag but rarely trend down cleanly; prefer longs.")
  ),
  list(
    id = "goldilocks", name = "Goldilocks / disinflationary growth",
    fp = c(SPX = 2, CREDIT = 1, VIX = -2, MOVE = -1, US10Y = -0.5, BREADTH = 1, CONS = 1, EMEQ = 1, OIL = -0.5),
    movie = "Inflation falls while growth holds: yields drift lower, volatility is compressed, credit is calm, equities grind higher with broad participation.",
    analogs = "1995-99 after the 1994 soft landing; 2017 (lowest-volatility year, S&P up every month); 2019 H2 after the Fed's insurance cuts; November 2023-2024.",
    after = "Usually ends with complacency (leverage, volatility selling) and an external shock, or with growth overheating into reflation.",
    tells = "VIX below 15 and term structure in steep contango; credit spreads tight; dips bought within days.",
    invalid = "Breadth falling while the index rises (narrow leadership, see below).",
    bot = list(long = c("SMH", "XLK", "IGV", "XLY", "ITB"), short = character(0),
               note = "Long breakouts work; option premium is cheap, so outright calls rather than spreads. Shorts are fighting the tide.")
  ),
  list(
    id = "growth_scare", name = "Growth scare / disinflationary slowdown",
    fp = c(US10Y = -2, JPY = 1, GOLD = 1, OIL = -1, COPPER = -2, SPX = -1, CONS = -1, BREADTH = -1, CREDIT = -1,
           VIX = 1, AUD = -1, EMEQ = -1),
    movie = "Markets start to price weaker growth: yields fall, copper and oil fall, the yen and gold are bought as havens, cyclicals and small caps underperform defensives.",
    analogs = "2001; summer 2011 (US downgrade and euro crisis, 10-year 3.5% to 2%, gold to $1,920 in September); 2015-16 (oil collapse, high-yield energy stress); 2019 (trade war, curve inversion, Fed cut in July); July-August 2024 (payroll miss triggered the Sahm rule on 2 August).",
    after = "Either the central bank cuts in time and it stays a scare (2016, 2019, 2024: equities recovered within weeks to months), or it becomes a recession (2001, 2008).",
    tells = "Copper/gold down with yields; cyclicals vs defensives down; credit spreads widening; the curve steepening because short rates fall (bull steepening).",
    invalid = "Yields falling while credit and breadth improve (goldilocks).",
    bot = list(long = c("GDX", "XLP", "PPH", "XLV", "VNQ"), short = c("XME", "COPX", "XOP", "OIH", "KRE", "XLY", "SLX"),
               note = "Defensive and gold breakouts work; cyclical breakdowns work. Watch for the policy response: a cut turns the short side quickly.")
  ),
  list(
    id = "carry_unwind", name = "Yen carry-trade unwind",
    fp = c(JPY = 2, AUD = -1.5, EMFX = -1, VIX = 2, NIKKEI = -2, SPX = -1, NDXREL = -1, US10Y = -1, BTC = -1, CHF = 1),
    movie = "Funding currencies (yen, franc) jump as leveraged carry positions are closed. Positions financed in yen are sold everywhere at once: Japanese equities, high-beta tech, crypto, high-yielding currencies. Fast and violent, driven by positioning more than by fundamentals.",
    analogs = "October 1998 (USD/JPY from 136 to 112 within days during LTCM); August 2007 (quant quake); 5 August 2024 (Nikkei -12.4% in one day, VIX intraday 65, after the Bank of Japan hike and a US payroll miss).",
    after = "Usually a one-to-three-week shock: in 2024 the S&P recovered its losses within about two weeks once positions were flushed. It becomes lasting only if it coincides with a real growth scare.",
    tells = "AUD/JPY falling fast; Nikkei leading the decline; VIX spike with term-structure inversion; Bank of Japan communication.",
    invalid = "Yen falling (carry still being added).",
    bot = list(long = character(0), short = c("SMH", "IGV", "IAI", "EEM"),
               note = "Do not chase breakdowns after the spike; do not open long breakouts until VIX term structure returns to contango.")
  ),
  list(
    id = "em_china_shock", name = "EM / China devaluation shock",
    fp = c(CNY = -2, EMFX = -2, AUD = -1, COPPER = -2, OIL = -1, EMEQ = -2, USD = 1, US10Y = -1, GOLD = 0.5, SPX = -1),
    movie = "China or the emerging world exports deflation: a weaker yuan, capital outflows, falling commodity demand. Commodity currencies and metals fall, EM equities break, developed-market yields fall as a haven.",
    analogs = "1997-98 (Asian crisis: Thai baht July 1997, Korean won halved, Russia default August 1998); 11 August 2015 (yuan devaluation, flash crash on 24 August); 2018 trade war (yuan 6.3 to 6.9, Turkey and Argentina crises, copper down).",
    after = "Ends with Chinese stimulus or Fed easing; the bottom in copper and AUD usually leads the bottom in EM equities.",
    tells = "Yuan fixing weaker; Hong Kong and KOSPI underperforming; copper down without a US growth signal; AUD weak.",
    invalid = "Yuan stable while EM falls (then it is the dollar, see wrecking ball).",
    bot = list(long = c("XLP", "PPH"), short = c("FXI", "KWEB", "EEM", "XME", "COPX", "LIT", "SLX", "OIH"),
               note = "Shorts in China and metals work; avoid long metals breakouts until copper and AUD stabilise.")
  ),
  list(
    id = "debasement", name = "Dollar debasement / fiscal dominance",
    fp = c(USD = -2, GOLD = 2, BTC = 1, US10Y = 1, CURVE = 1, COPPER = 1, EUR = 1, CHF = 1, SPX = 1, EMEQ = 1),
    movie = "Doubts about the dollar's purchasing power or the independence of the central bank: real assets and alternative stores of value (gold, bitcoin, franc) rise, the dollar falls, long yields rise on term premium while nominal equities still go up.",
    analogs = "The 1970s; H2 2020-2021 (DXY 103 to 89, gold record $2,075 in August 2020, bitcoin boom); H1 2025 (dollar's worst first half since 1973, gold through $3,000 in March).",
    after = "Persists while real rates stay low relative to inflation; ends with a credible tightening (Volcker 1979-81) or a growth shock that revives dollar demand.",
    tells = "Gold rising with yields (not with falling yields); gold rising in all currencies; central-bank gold buying; weak foreign demand at Treasury auctions.",
    invalid = "Gold rising only because yields fall (growth scare).",
    bot = list(long = c("GDX", "SIL", "COPX", "XME", "EEM", "IAI", "EWL"), short = character(0),
               note = "Long real-asset and non-US breakouts; USD-based shorts work poorly as everything is lifted in dollar terms.")
  ),
  list(
    id = "narrow_mania", name = "Narrow leadership / late-cycle mania",
    fp = c(SPX = 1, NDXREL = 2, BREADTH = -2, VIX = -0.5, CONS = -0.5, US10Y = 0.5, USD = 0.5),
    movie = "The index rises on a handful of leaders while the average stock falls. Capital concentrates in one theme; the index hides deteriorating internals.",
    analogs = "1999-2000 (breadth deteriorated for two years before the Nasdaq peaked on 10 March 2000, then -78%); 2021 (peak in speculative growth in February, index in December); 2023-24 (Magnificent Seven).",
    after = "Narrow markets can persist for a long time (1998-2000: about two years); they end when the leaders themselves break, often on rising rates. Divergence alone is not a timing signal.",
    tells = "Index new highs with fewer stocks above their 50-day average; leaders gapping on earnings; rising rates.",
    invalid = "Equal weight catching up (healthy broadening).",
    bot = list(long = c("SMH", "XLK"), short = c("ITB", "XRT", "KRE", "VNQ"),
               note = "Long only the leaders, small size; breakouts in the average stock fail more often. Shorts in laggards work while breadth falls.")
  )
)

#' z-score of the latest n-day move: change / (stdev of daily changes over 120 days x sqrt(n))
move_z <- function(d, n = 21, kind = "price") {
  if (is.null(d) || nrow(d) < n + 30) return(NA_real_)
  x <- d$Close
  dx <- if (kind == "yield") diff(x) else diff(log(x))
  sdv <- stats::sd(tail(dx, 120), na.rm = TRUE)
  if (is.na(sdv) || sdv == 0) return(NA_real_)
  m <- length(x)
  mv <- if (kind == "yield") x[m] - x[m - n] else log(x[m] / x[m - n])
  mv / (sdv * sqrt(n))
}

fp_series <- function(raw, sym) {
  if (grepl("-^", sym, fixed = TRUE)) {
    p <- strsplit(sym, "-", fixed = TRUE)[[1]]
    a <- get_close(raw, p[1]); b <- get_close(raw, p[2])
    if (is.null(a) || is.null(b)) return(NULL)
    m <- merge(a, b, by = "date")
    return(data.frame(date = m$date, Close = m$Close.x - m$Close.y))
  }
  get_close(raw, sym)
}

#' Current z-scores for every fingerprint asset, for horizon n days
asset_moves <- function(raw, n = 21) {
  vapply(names(FP_ASSETS), function(k) {
    a <- FP_ASSETS[[k]]
    zs <- vapply(a[[1]], function(s) {
      kind <- if (s %in% c("^TNX", "^TNX-^IRX")) "yield" else "price"
      move_z(fp_series(raw, s), n, kind)
    }, 0)
    a[[2]] * mean(zs, na.rm = TRUE)
  }, 0)
}

#' Match every archetype against today's z-scores
match_archetypes <- function(z, z3) {
  clip <- function(v) pmax(-1, pmin(1, v / 1.5))
  one <- function(a, zz) {
    keys <- intersect(names(a$fp), names(zz)[!is.na(zz)])
    w <- a$fp[keys]
    sum(w * clip(zz[keys])) / sum(abs(w))
  }
  rows <- lapply(ARCHETYPES, function(a) {
    keys <- intersect(names(a$fp), names(z)[!is.na(z)])
    agree <- keys[sign(a$fp[keys]) * z[keys] >= 0.5]
    against <- keys[sign(a$fp[keys]) * z[keys] <= -0.5]
    list(id = a$id, name = a$name, score = one(a, z), score3m = one(a, z3),
         agree = agree, against = against, a = a)
  })
  rows[order(-vapply(rows, `[[`, 0, "score"))]
}

fp_label <- function(k) FP_ASSETS[[k]][[3]]

describe_z <- function(k, zv) {
  word <- if (abs(zv) >= 2) "sharply" else if (abs(zv) >= 1) "clearly" else "slightly"
  dir <- if (zv > 0) "up" else "down"
  if (k %in% c("US10Y", "BUND", "GILT", "CURVE")) dir <- if (zv > 0) "higher" else "lower"
  sprintf("%s %s %s", fp_label(k), word, dir)
}

# ── Persistence: how long has each scenario been in place? ──────────────────
# One day does not make a trend. Scores are recomputed for the last HIST_DAYS trading days
# from price history (same fingerprints as today, so the series is consistent even when
# fingerprints change), then each scenario gets an age label.

SCEN_ACTIVE <- 0.40   # match score at which a scenario counts as "in place"
HIST_DAYS <- 60

#' Scenario scores for each of the last `days` trading days (rows = dates, cols = scenario ids)
scenario_history <- function(raw, days = HIST_DAYS) {
  series <- lapply(FP_ASSETS, function(a) lapply(a[[1]], function(s) fp_series(raw, s)))
  dates <- tail(sort(unique(get_close(raw, "^GSPC")$date)), days)
  clip <- function(v) pmax(-1, pmin(1, v / 1.5))
  out <- t(vapply(seq_along(dates), function(i) {
    d <- dates[i]
    z <- vapply(names(FP_ASSETS), function(k) {
      a <- FP_ASSETS[[k]]
      zs <- mapply(function(s, sym) {
        if (is.null(s)) return(NA_real_)
        kind <- if (sym %in% c("^TNX", "^TNX-^IRX")) "yield" else "price"
        move_z(s[s$date <= d, ], 21, kind)
      }, series[[k]], a[[1]])
      a[[2]] * mean(zs, na.rm = TRUE)
    }, 0)
    vapply(ARCHETYPES, function(a) {
      keys <- intersect(names(a$fp), names(z)[!is.na(z)])
      w <- a$fp[keys]
      sum(w * clip(z[keys])) / sum(abs(w))
    }, 0)
  }, numeric(length(ARCHETYPES))))
  if (length(ARCHETYPES) == 1) out <- t(out)
  colnames(out) <- vapply(ARCHETYPES, `[[`, "", "id")
  data.frame(date = dates, out, check.names = FALSE)
}

#' Age label for one scenario from its score history (oldest first, today last)
scenario_age <- function(s) {
  n <- length(s); s0 <- s[n]
  prev5 <- s[max(1, n - 5):(n - 1)]
  act <- s >= SCEN_ACTIVE
  run <- 0; for (i in n:1) if (act[i]) run <- run + 1 else break
  days_on <- sum(act)   # days in place within the window
  if (!act[n]) {
    if (any(prev5 >= SCEN_ACTIVE)) return(list(status = "FADED", run = 0,
      text = "In place in recent reports but no longer: the move that defined it has stopped."))
    return(list(status = "INACTIVE", run = 0, text = ""))
  }
  if (sum(prev5 >= SCEN_ACTIVE) <= 1) return(list(status = "NEW", run = run, text =
    "New: at most one of the previous five reports showed it. One day does not make a trend; wait for it to persist before acting on it."))
  if (run < 15) {
    if (s0 >= mean(prev5)) return(list(status = "BUILDING", run = run, text = sprintf(
      "Building: in place for %d trading days and strengthening. The early, most rewarding part of a scenario.", run)))
    return(list(status = "WAVERING", run = run, text = sprintf(
      "Wavering: in place for %d trading days but weaker than in recent reports.", run)))
  }
  if (run < 40) {
    if (s0 >= s[max(1, n - 5)]) return(list(status = "ESTABLISHED", run = run, text = sprintf(
      "Established and reinforcing: in place for %d trading days, score still rising.", run)))
    return(list(status = "ESTABLISHED", run = run, text = sprintf(
      "Established but losing momentum: in place for %d trading days, score lower than a week ago.", run)))
  }
  list(status = "MATURE", run = run, text = sprintf(
    "Mature: in place for %d trading days or more. Scenarios this old are nearer their end than their start; watch the signs that would rule it out.", run))
}

#' Attach history and age to each match
add_persistence <- function(matches, hist) {
  lapply(matches, function(m) {
    s <- hist[[m$id]]
    c(m, list(hist = s, age = scenario_age(s), prev5 = tail(head(s, -1), 5)))
  })
}
