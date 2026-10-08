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
  BTC     = list("BTC-USD", 1, "Bitcoin"),
  SMALL   = list("IWM/^GSPC", 1, "Russell 2000 vs S&P 500"),
  SHORT   = list("^IRX", 1, "US 3-month bill yield"),
  # Absolute breadth: % of S&P 500 stocks above their 50-day average. A level, not a move:
  # no price history on Yahoo, so it is read from the daily runs (macro_context_results.s5fi).
  ABS_BREADTH = list("S5FI", 1, "Stocks above their 50-day average")
)

YIELD_SYMBOLS <- c("^TNX", "^IRX", "^TNX-^IRX")   # moves in percentage points, not log returns

# ABS_BREADTH enters the match as (level - 50) / 15: 50% = 0, 27.5% or 72.5% = -/+1.5 (the cap).
# The 1-month value is the latest reading, the 3-month value the mean of the readings over 63 sessions.
ABS_BREADTH_CENTER <- 50
ABS_BREADTH_SCALE  <- 15

ARCHETYPES <- list(
  list(
    id = "dash_for_cash", name = "Dash for cash (dollar liquidity squeeze)",
    fp = c(USD = 2, EMFX = -2, AUD = -1, CAD = -1, EUR = -1, JPY = 1, GOLD = -1, OIL = -1, COPPER = -1,
           CREDIT = -2, SPX = -2, VIX = 2, MOVE = 1, EMEQ = -2, BTC = -1, ABS_BREADTH = -1),
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
           MOVE = 2, GOLD = -1, COPPER = -1, SPX = -1, NDXREL = -1, CREDIT = -1),   # OIL, VIX dropped (calibration 2026-10-08)
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
           EMFX = -1, ABS_BREADTH = -0.5),   # CREDIT dropped (calibration 2026-10-08)
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
    # Calibration 2026-10-08: US10Y 1 -> 1.5; GOLD, CURVE, BREADTH, VIX, EMFX, CREDIT dropped (about zero in 2008 and 2022)
    fp = c(OIL = 2, US10Y = 1.5, SPX = -1, CONS = -1.5, ABS_BREADTH = -0.5),
    movie = "Energy prices rise for supply reasons, not demand. Inflation goes up while growth goes down; the central bank cannot cut. The consumer is squeezed (discretionary underperforms staples), margins compress, and the curve flattens as policy stays tight.",
    analogs = "1973-74 (oil quadrupled, S&P -48%, gold up strongly); 1979-80 (Iran, gold to $850, Volcker); August 1990 (Iraq invades Kuwait, oil doubles, recession); H1 2008 (oil to $147 in July just before the crash); H1 2022 (Russia-Ukraine).",
    after = "Supply shocks end in demand destruction: oil peaks, then growth slows and the regime hands over to a growth scare or recession. In 1990 and 2008 the oil peak preceded the equity low by months.",
    tells = "Oil up while copper falls; inflation expectations (TIPS vs Treasuries) up; discretionary vs staples down; consumer sentiment down; Treasuries sold with the dollar up as oil importers raise dollars.",
    invalid = "Oil up with copper and breadth up (demand-driven reflation).",
    bot = list(long = c("XOP", "OIH", "FCG", "XLE", "GDX", "DBA", "MOO"), short = c("XLY", "XRT", "CARZ", "ITB", "IGV"),
               note = "Energy and real-asset breakouts work; consumer and long-duration shorts work. Exit energy longs on the first sign of demand destruction (oil down while copper down)."),
    # Conditional chain, shown under the scenario cards whatever this scenario's rank:
    # each step is a fingerprint asset that must be clearly up (z >= CHAIN_ON); the first one is the trigger.
    chain = list(
      name = "Oil to yields (petrodollar)",
      steps = c("OIL", "USD", "US10Y"),
      text = "Oil is paid in dollars. When it rises, importers need more dollars; their central banks and reserve holders raise them by selling US Treasuries, so long US yields rise with the dollar even without a change in inflation expectations. A bond market already under stress then reprices equities.")
  ),
  list(
    id = "reflation", name = "Reflation / global recovery",
    fp = c(USD = -1, AUD = 1, EMFX = 1, CNY = 1, COPPER = 2, OIL = 1, US10Y = 1, CURVE = 1, SPX = 1, BREADTH = 2,
           CREDIT = 1, VIX = -1, EMEQ = 1, CONS = 1, ABS_BREADTH = 1.5, SMALL = 1),
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
    # Calibration 2026-10-08: BREADTH (equal weight vs S&P) dropped, large caps led in 2017, H2 2019 and 2023-24
    fp = c(SPX = 2, CREDIT = 1, VIX = -2, MOVE = -1, US10Y = -0.5, CONS = 1, EMEQ = 1, OIL = -0.5, ABS_BREADTH = 1),
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
           VIX = 1, AUD = -1, EMEQ = -1, ABS_BREADTH = -1, SMALL = -1),
    movie = "Markets start to price weaker growth: yields fall, copper and oil fall, the yen and gold are bought as havens, cyclicals and small caps underperform defensives.",
    analogs = "2001; summer 2011 (US downgrade and euro crisis, 10-year 3.5% to 2%, gold to $1,920 in September); 2015-16 (oil collapse, high-yield energy stress); 2019 (trade war, curve inversion, Fed cut in July); July-August 2024 (payroll miss triggered the Sahm rule on 2 August).",
    after = "Either the central bank cuts in time and it stays a scare (2016, 2019, 2024: equities recovered within weeks to months), or it becomes a recession (2001, 2008).",
    tells = "Copper/gold down with yields; cyclicals vs defensives down; credit spreads widening; the curve steepening because short rates fall (bull steepening).",
    invalid = "Yields falling while credit and breadth improve (goldilocks).",
    bot = list(long = c("GDX", "XLP", "PPH", "XLV", "VNQ"), short = c("XME", "COPX", "XOP", "OIH", "KRE", "XLY", "SLX"),
               note = "Defensive and gold breakouts work; cyclical breakdowns work. Watch for the policy response: a cut turns the short side quickly.")
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
    # Calibration 2026-10-08: US10Y and CURVE dropped (about zero or wrong sign in 2020 and 2025)
    fp = c(USD = -2, GOLD = 2, BTC = 1, COPPER = 1, EUR = 1, CHF = 1, SPX = 1, EMEQ = 1),
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
    fp = c(SPX = 1, NDXREL = 2, BREADTH = -2, VIX = -0.5, CONS = -0.5, US10Y = 0.5, USD = 0.5, ABS_BREADTH = -1),
    movie = "The index rises on a handful of leaders while the average stock falls. Capital concentrates in one theme; the index hides deteriorating internals.",
    analogs = "1999-2000 (breadth deteriorated for two years before the Nasdaq peaked on 10 March 2000, then -78%); 2021 (peak in speculative growth in February, index in December); 2023-24 (Magnificent Seven).",
    after = "Narrow markets can persist for a long time (1998-2000: about two years); they end when the leaders themselves break, often on rising rates. Divergence alone is not a timing signal.",
    tells = "Index new highs with fewer stocks above their 50-day average; leaders gapping on earnings; rising rates.",
    invalid = "Equal weight catching up (healthy broadening).",
    bot = list(long = c("SMH", "XLK"), short = c("ITB", "XRT", "KRE", "VNQ"),
               note = "Long only the leaders, small size; breakouts in the average stock fail more often. Shorts in laggards work while breadth falls.")
  ),
  list(
    id = "leadership_unwind", name = "Leadership unwind / rotation out of the leaders",
    fp = c(NDXREL = -2, BREADTH = 1.5, SMALL = 1, SPX = -1, VIX = 1, BTC = -1),
    movie = "The leaders that carried the index are sold while the average stock holds up or rises: money rotates out of the crowded theme into laggards, value and small caps. The index falls because its heaviest weights fall, not because everything falls. It is the usual way a narrow market ends.",
    analogs = "March 2000-2002 (Nasdaq peaked on 10 March 2000 and fell 78%, while value and equal-weight stocks held up in 2000-01); February-March 2021 (speculative growth peaked, rotation into value and cyclicals); 2022 (Nasdaq 100 about -33% vs equal-weight S&P about -13%); July 2024 (after the 11 July CPI the Russell 2000 rose about 10% in five sessions while the Magnificent Seven were sold).",
    after = "Two outcomes. A rotation (2021, July 2024) where the index holds and leadership broadens, often reversing within weeks. Or a top (2000, 2022) where the leaders keep falling and drag the index down for months; the difference is whether breadth holds up in absolute terms.",
    tells = "Leaders falling on good earnings; Nasdaq 100 / S&P below its 50-day average; equal weight and the Russell beating the index while it falls; semis / S&P rolling over.",
    invalid = "Everything falling together with equal weight lagging as well (that is liquidity stress, see dash for cash), or the leaders back at new highs.",
    bot = list(long = c("IWM", "XLI", "XLF"), short = c("SMH", "XLK", "IGV"),
               note = "Exit or tighten long breakouts in the former leaders; their breakdowns are the cleanest shorts. Laggard breakouts start working, small size until absolute breadth confirms.")
  ),
  list(
    id = "policy_pivot", name = "Policy pivot / easing rally",
    # Calibration 2026-10-08: US10Y -0.5 -> -2, SHORT -2 -> -1, VIX -1 -> -2, CURVE dropped (the curve flattened in all three episodes)
    fp = c(SHORT = -1, US10Y = -2, USD = -1.5, GOLD = 1, JPY = 1, CREDIT = 1, BREADTH = 1, SMALL = 1.5,
           VIX = -2, MOVE = -1, EMFX = 1),
    movie = "The central bank signals or delivers cuts before a recession: long yields fall first (the 3-month bill waits for the first cut, so the curve flattens before it steepens), the dollar weakens, gold and the yen are bought, credit tightens and rate-sensitive laggards (small caps, regional banks, homebuilders) lead a broadening rally.",
    analogs = "1995 (first cut in July after the 1994 hikes, soft landing, S&P +34% for the year); autumn 1998 (three cuts after LTCM); 2019 (pivot in January, cuts from July, S&P +29%); November-December 2023 (Fed signalled the end of hikes, 10-year from 5.0% to about 3.9%, small caps about +20% in two months).",
    after = "If the pivot comes before a recession (1995, 1998, 2019) the rally broadens and lasts 6-12 months. If the central bank cuts into a recession (2001, 2007) short rates fall the same way but credit widens and equities fall: that is a growth scare or credit event, and the credit and small-cap legs of this fingerprint fail.",
    tells = "10-year and 2-year yields falling, then the 3-month bill once cuts are delivered (bull steepening comes later); dollar breaking down; gold and yen up together with equities; equal weight and the Russell beating the index; high yield beating Treasuries.",
    invalid = "Short rates falling while credit widens and small caps fall (growth scare: the central bank is cutting into weakness).",
    bot = list(long = c("IWM", "KRE", "ITB", "GDX", "EEM", "XME"), short = character(0),
               note = "Cover duration shorts (homebuilders, REITs, regional banks) left over from a bond rout; those groups lead the first leg. Long breakouts across the average stock start working.")
  ),
  list(
    id = "credit_event", name = "Credit event / recession bear market",
    fp = c(CREDIT = -2, SPX = -2, VIX = 2, US10Y = -1.5, SHORT = -1, SMALL = -1, ABS_BREADTH = -1, COPPER = -1, OIL = -1,
           MOVE = 1, USD = 0.5),
    movie = "Losses in credit force lenders and leveraged holders to cut risk: high-yield spreads widen, banks and small caps (the most credit-dependent) fall hardest, equities enter a bear market as earnings estimates are cut, and Treasuries rally as money seeks safety and the central bank is expected to cut. Slower and deeper than a growth scare; unlike a dash for cash, Treasuries work as a hedge.",
    analogs = "2001-02 (Enron and WorldCom, high-yield defaults near 10%, S&P -49% from the 2000 peak); July 2007-March 2009 (subprime, Bear Stearns March 2008, Lehman 15 September 2008, S&P -57%); December 2015-February 2016 (energy high-yield stress); February-March 2020 (spreads to about 10%, before the Fed bought corporate bonds); March 2023 (Silicon Valley Bank, regional-bank stress, contained in two weeks).",
    after = "Ends with a policy backstop aimed at credit itself (bank recapitalisation and TARP 2008, corporate-bond buying 23 March 2020, the bank term funding programme 12 March 2023). Equities usually bottom after spreads peak; in 2009 the S&P low (9 March) came about three months after high-yield spreads peaked.",
    tells = "High yield and leveraged loans falling while Treasuries rally; regional banks and small caps leading the decline; bank funding headlines (deposit flight, repo); earnings estimates being cut; the curve steepening as short rates fall.",
    invalid = "Credit stable while equities fall (growth scare or valuation-driven correction), or Treasuries sold together with everything (dash for cash).",
    bot = list(long = c("GLD", "XLP", "XLV"), short = c("KRE", "XLF", "IWM", "XHB", "XLY"),
               note = "No new long breakouts while high yield keeps underperforming Treasuries. Short the credit-dependent groups; take profits into backstop announcements, which reverse them violently.")
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

#' Absolute-breadth history: one reading per trading date.
#' Each morning run computes breadth from the previous close, so a reading stored on cache_date D
#' belongs to the last S&P session before D. `live_pct` (today's run) is appended for the latest session.
load_breadth_history <- function(raw, live_pct = NA_real_) {
  spx <- sort(unique(get_close(raw, "^GSPC")$date))
  h <- tryCatch({
    conn <- Tdata::safe_db_connect(); on.exit(DBI::dbDisconnect(conn), add = TRUE)
    DBI::dbGetQuery(conn, "SELECT cache_date, s5fi FROM macro_context_results WHERE s5fi IS NOT NULL")
  }, error = function(e) NULL)
  out <- data.frame(date = as.Date(character(0)), pct = numeric(0))
  if (!is.null(h) && nrow(h)) {
    i <- findInterval(as.numeric(as.Date(h$cache_date)) - 1, as.numeric(spx))   # last session strictly before D
    out <- data.frame(date = spx[pmax(i, 1)], pct = h$s5fi)[i > 0, ]
  }
  if (!is.na(live_pct) && length(spx)) out <- rbind(out, data.frame(date = max(spx), pct = live_pct))
  out <- out[!duplicated(out$date, fromLast = TRUE), ]
  out[order(out$date), ]
}

#' ABS_BREADTH value on date d: latest reading (n = 21) or mean over 63 sessions (n = 63), as (level - 50) / 15
abs_breadth_z <- function(bh, d, n = 21) {
  if (is.null(bh) || !nrow(bh)) return(NA_real_)
  b <- bh[bh$date <= d, ]
  if (!nrow(b) || as.numeric(d - max(b$date)) > 7) return(NA_real_)
  v <- if (n <= 21) tail(b$pct, 1) else {
    w <- b$pct[b$date > d - round(n * 365 / 252)]
    if (length(w) < 10) return(NA_real_)
    mean(w)
  }
  (v - ABS_BREADTH_CENTER) / ABS_BREADTH_SCALE
}

#' Current z-scores for every fingerprint asset, for horizon n days
asset_moves <- function(raw, n = 21, bh = NULL) {
  d_last <- max(get_close(raw, "^GSPC")$date)
  vapply(names(FP_ASSETS), function(k) {
    if (k == "ABS_BREADTH") return(abs_breadth_z(bh, d_last, n))
    a <- FP_ASSETS[[k]]
    zs <- vapply(a[[1]], function(s) {
      kind <- if (s %in% YIELD_SYMBOLS) "yield" else "price"
      move_z(fp_series(raw, s), n, kind)
    }, 0)
    a[[2]] * mean(zs, na.rm = TRUE)
  }, 0)
}

# ── State: where each asset stands in its 1-year range ──────────────────────
# state = 2 * (last - min) / (max - min) - 1 over the last STATE_WINDOW sessions (bottom -1, top +1), sign as in
# FP_ASSETS. Needs at least STATE_MIN_ROWS sessions. Absolute breadth is already a level: its state is its
# capped value, (level - 50) / 15 / 1.5. Chosen over the move for "in place" by compare_state_move.py (2026-10-08).
STATE_WINDOW   <- 252
STATE_MIN_ROWS <- 240
STATE_ACTIVE   <- 0.50   # state score at which a scenario is in place (mean false positives 9% on 2004-2026 episodes)

state_pos <- function(d, x) {
  if (is.null(d)) return(NA_real_)
  v <- d$Close[!is.na(d$Close)]
  if (length(v) < STATE_MIN_ROWS) return(NA_real_)
  v <- tail(v, STATE_WINDOW); lo <- min(v); hi <- max(v)
  if (hi <= lo) return(NA_real_)
  2 * (tail(v, 1) - lo) / (hi - lo) - 1
}

#' State of every fingerprint asset on date d (default: last S&P session)
asset_states <- function(raw, bh = NULL, d = NULL, series = NULL) {
  if (is.null(d)) d <- max(get_close(raw, "^GSPC")$date)
  vapply(names(FP_ASSETS), function(k) {
    if (k == "ABS_BREADTH") return(pmax(-1, pmin(1, abs_breadth_z(bh, d, 21) / 1.5)))
    a <- FP_ASSETS[[k]]
    ss <- if (is.null(series)) lapply(a[[1]], function(sym) fp_series(raw, sym)) else series[[k]]
    v <- vapply(ss, function(x) if (is.null(x)) NA_real_ else state_pos(x[x$date <= d, ]), 0)
    a[[2]] * mean(v, na.rm = TRUE)
  }, 0)
}

#' Weighted state score of one fingerprint (positions are already in [-1, 1], no cap)
state_score <- function(fp, st) {
  keys <- intersect(names(fp), names(st)[!is.na(st)])
  if (!length(keys)) return(NA_real_)
  sum(fp[keys] * st[keys]) / sum(abs(fp[keys]))
}

#' Match every archetype: state (where markets stand) and move (21- and 63-session moves)
match_archetypes <- function(z, z3, st = NULL) {
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
         state = if (is.null(st)) NA_real_ else state_score(a$fp, st),
         agree = agree, against = against, a = a)
  })
  key <- vapply(rows, function(r) if (is.na(r$state)) r$score else r$state, 0)   # rank by state, move as fallback
  rows[order(-key, -vapply(rows, `[[`, 0, "score"))]
}

fp_label <- function(k) FP_ASSETS[[k]][[3]]

describe_z <- function(k, zv) {
  word <- if (abs(zv) >= 2) "sharply" else if (abs(zv) >= 1) "clearly" else "slightly"
  dir <- if (zv > 0) "up" else "down"
  if (k == "ABS_BREADTH") return(sprintf("%s at %.0f%%", fp_label(k), ABS_BREADTH_CENTER + ABS_BREADTH_SCALE * zv))
  if (k %in% c("US10Y", "BUND", "GILT", "CURVE", "SHORT")) dir <- if (zv > 0) "higher" else "lower"
  sprintf("%s %s %s", fp_label(k), word, dir)
}

CHAIN_ON <- 1   # z-score at which a chain step counts as moving ("clearly up")

#' Status of an archetype's conditional chain against today's z-scores
chain_status <- function(ch, z) {
  zs <- z[ch$steps]
  on <- !is.na(zs) & zs >= CHAIN_ON
  rest <- paste(vapply(ch$steps[-1], fp_label, ""), collapse = " and ")
  status <- if (all(on)) "Firing: every link is moving."
    else if (on[1]) sprintf("Triggered: %s is up; %s are the next links.", tolower(fp_label(ch$steps[1])), rest)
    else if (all(on[-1])) sprintf("Not triggered: %s already up without %s, for another reason; a rally in %s would add to it.",
                                  rest, tolower(fp_label(ch$steps[1])), tolower(fp_label(ch$steps[1])))
    else "Not triggered."
  list(on = on, status = status,
       steps = vapply(seq_along(ch$steps), function(i) sprintf("%s (z %+.1f)", fp_label(ch$steps[i]), zs[i]), ""))
}

# ── Yen carry-trade unwind: short-window alert ──────────────────────────────
# An unwind plays out in one to three sessions, so 21-day scenario scores catch it late and
# diluted. It is checked on 3-session z-scores instead (same z definition, n = 3).
# Fired 2019-2026 on: Aug 2019, Feb-Mar 2020, Nov 2021, Dec 2022 (Bank of Japan yield-cap change),
# 25 Jul - 7 Aug 2024, Apr 2025. Status is reported for the last CARRY_LOOKBACK sessions.
CARRY_ALERT <- list(
  window = 3, yen_fire = 2, yen_watch = 1.5, confirm_z = 2,
  name = "Yen carry-trade unwind",
  movie = "Funding currencies (yen, franc) jump as leveraged carry positions are closed. Positions financed in yen are sold everywhere at once: Japanese equities, high-beta tech, crypto, high-yielding currencies. Fast and violent, driven by positioning more than by fundamentals.",
  analogs = "October 1998 (USD/JPY from 136 to 112 within days during LTCM); August 2007 (quant quake); 5 August 2024 (Nikkei -12.4% in one day, VIX intraday 65, after the Bank of Japan hike and a US payroll miss).",
  after = "Usually a one-to-three-week shock: in 2024 the S&P recovered its losses within about two weeks once positions were flushed. It becomes lasting only if it coincides with a real growth scare.",
  bot = "Do not chase breakdowns after the spike; do not open long breakouts until VIX term structure returns to contango."
)
CARRY_LOOKBACK <- 5

#' Carry-unwind status for each of the last CARRY_LOOKBACK sessions (S&P calendar, FX joined as-of)
carry_alert <- function(raw) {
  ca <- CARRY_ALERT
  dates <- tail(sort(unique(get_close(raw, "^GSPC")$date)), CARRY_LOOKBACK)
  s <- list(usdjpy = get_close(raw, "USDJPY=X"), audjpy = get_close(raw, "AUDJPY=X"), nikkei = get_close(raw, "^N225"),
            btc = get_close(raw, "BTC-USD"), vix = get_close(raw, "^VIX"), vix3m = get_close(raw, "^VIX3M"))
  asof <- function(d, x) if (is.null(x)) NULL else x[x$date <= d, ]
  last_val <- function(d, x) { y <- asof(d, x); if (is.null(y) || !nrow(y)) NA_real_ else tail(y$Close, 1) }
  rows <- lapply(dates, function(d) {
    z <- function(k) move_z(asof(d, s[[k]]), ca$window, "price")
    yen <- -z("usdjpy"); aj <- z("audjpy"); nk <- z("nikkei"); bt <- z("btc")
    vinv <- isTRUE(last_val(d, s$vix) >= last_val(d, s$vix3m))
    conf <- c(audjpy = isTRUE(aj <= -ca$confirm_z), nikkei = isTRUE(nk <= -ca$confirm_z),
              btc = isTRUE(bt <= -ca$confirm_z), vix_inverted = vinv)
    n <- sum(conf)
    status <- if (isTRUE(yen >= ca$yen_fire) && n >= 2) "FIRING"
              else if (isTRUE(yen >= ca$yen_watch) && n >= 1) "WATCH" else "QUIET"
    list(date = d, yen = yen, audjpy = aj, nikkei = nk, btc = bt, vix_inverted = vinv, n_conf = n, status = status)
  })
  today <- rows[[length(rows)]]
  fired <- Filter(function(r) r$status == "FIRING", rows)
  list(today = today, rows = rows, last_fired = if (length(fired)) fired[[length(fired)]]$date else NULL)
}

# ── Persistence: state decides "in place", move gives the direction ──────────
# One day does not make a trend. State and move scores are recomputed for the last HIST_DAYS trading days
# from price history (same fingerprints as today), then each scenario gets a status.

SCEN_ACTIVE <- 0.40   # move score at which a scenario counts as moving strongly toward it (calibrated 2026-10-08)
HIST_DAYS <- 60

#' State and move scores for each of the last `days` trading days: list(state, move), rows = dates, cols = ids
scenario_history <- function(raw, days = HIST_DAYS, bh = NULL) {
  series <- lapply(FP_ASSETS, function(a) lapply(a[[1]], function(s) fp_series(raw, s)))
  dates <- tail(sort(unique(get_close(raw, "^GSPC")$date)), days)
  clip <- function(v) pmax(-1, pmin(1, v / 1.5))
  ids <- vapply(ARCHETYPES, `[[`, "", "id")
  per_day <- lapply(seq_along(dates), function(i) {
    d <- dates[i]
    z <- vapply(names(FP_ASSETS), function(k) {
      if (k == "ABS_BREADTH") return(abs_breadth_z(bh, d, 21))
      a <- FP_ASSETS[[k]]
      zs <- mapply(function(s, sym) {
        if (is.null(s)) return(NA_real_)
        kind <- if (sym %in% YIELD_SYMBOLS) "yield" else "price"
        move_z(s[s$date <= d, ], 21, kind)
      }, series[[k]], a[[1]])
      a[[2]] * mean(zs, na.rm = TRUE)
    }, 0)
    st <- asset_states(raw, bh, d, series)
    rbind(move = vapply(ARCHETYPES, function(a) {
      keys <- intersect(names(a$fp), names(z)[!is.na(z)])
      w <- a$fp[keys]
      sum(w * clip(z[keys])) / sum(abs(w))
    }, 0), state = vapply(ARCHETYPES, function(a) state_score(a$fp, st), 0))
  })
  mk <- function(row) {
    m <- do.call(rbind, lapply(per_day, function(x) x[row, ]))
    colnames(m) <- ids
    data.frame(date = dates, m, check.names = FALSE)
  }
  list(move = mk("move"), state = mk("state"))
}

#' Status from the state history (in place) and today's move (direction); oldest first, today last
scenario_age <- function(st, mv) {
  n <- length(st); s0 <- st[n]; m0 <- mv[n]
  act <- !is.na(st) & st >= STATE_ACTIVE
  prev5 <- act[max(1, n - 5):(n - 1)]
  run <- 0; for (i in n:1) if (act[i]) run <- run + 1 else break
  if (!act[n]) {
    if (isTRUE(m0 >= SCEN_ACTIVE)) return(list(status = "EMERGING", run = 0, text = sprintf(
      "Emerging: not in place yet (state %+.0f%%, needs %+.0f%%), but the last month's moves point strongly toward it. The early part of a scenario; watch whether the state follows.",
      100 * s0, 100 * STATE_ACTIVE)))
    if (any(prev5)) return(list(status = "FADED", run = 0,
      text = "In place in recent reports but no longer: markets have moved away from it."))
    return(list(status = "INACTIVE", run = 0, text = ""))
  }
  if (isTRUE(m0 < 0)) return(list(status = "FADING", run = run, text = sprintf(
    "Fading: in place for %d trading days, but over the last month its assets moved against it (move %+.0f%%). Often the start of its end.",
    run, 100 * m0)))
  if (isTRUE(m0 >= SCEN_ACTIVE)) return(list(status = "BUILDING", run = run, text = sprintf(
    "Building: in place for %d trading days and still strengthening (move %+.0f%%).", run, 100 * m0)))
  if (run < 40) return(list(status = "ESTABLISHED", run = run, text = sprintf(
    "Established: in place for %d trading days; the last month's moves no longer add much (move %+.0f%%).", run, 100 * m0)))
  list(status = "MATURE", run = run, text = sprintf(
    "Mature: in place for %d trading days or more, moves flat (%+.0f%%). Scenarios this old are nearer their end than their start; watch the signs that would rule it out.",
    run, 100 * m0))
}

#' Attach history and status to each match
add_persistence <- function(matches, hist) {
  lapply(matches, function(m) {
    st <- hist$state[[m$id]]; mv <- hist$move[[m$id]]
    c(m, list(hist = st, hist_move = mv, age = scenario_age(st, mv), prev5 = tail(head(st, -1), 5)))
  })
}
