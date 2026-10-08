# intermarket_config.R — Intermarket panels: sections, ratios, BOT sector-map drivers
#
# Section order follows the macro-outlook reading order:
#   stocks -> US sectors -> currencies -> precious metals -> oil -> other commodities -> rates -> world markets
# kind: "price" (returns in %), "yield" (changes in basis points), "fx" (returns in %)

SECTIONS <- list(
  list(
    id = "stocks", title = "Stock markets",
    instruments = list(
      c("^GSPC", "S&P 500", "price"),
      c("^NDX", "Nasdaq 100", "price"),
      c("RSP", "S&P 500 equal weight", "price"),
      c("IWM", "Russell 2000", "price"),
      c("SMH", "Semiconductors", "price"),
      c("^VIX", "VIX", "price"),
      c("^VIX3M", "VIX 3-month", "price")
    ),
    ratios = list(
      c("^NDX", "^GSPC", "Nasdaq 100 / S&P 500", "Up = mega-cap growth leads"),
      c("RSP", "^GSPC", "Equal weight / S&P 500", "Up = broad participation; down = narrow, index carried by a few names"),
      c("IWM", "^GSPC", "Russell 2000 / S&P 500", "Up = domestic, rate-sensitive risk appetite"),
      c("XLY", "XLP", "Discretionary / Staples", "Up = risk-on consumer; down = defensive rotation"),
      c("SMH", "^GSPC", "Semis / S&P 500", "Up = AI/semis leadership intact")
    )
  ),
  list(
    # Parent sectors: context for the BOT sector map, which is read per correlation group
    id = "sectors", title = "US sectors (SPDR)",
    instruments = list(
      c("XLK", "Technology", "price"),
      c("XLC", "Communication services", "price"),
      c("XLY", "Consumer discretionary", "price"),
      c("XLF", "Financials", "price"),
      c("XLI", "Industrials", "price"),
      c("XLB", "Materials", "price"),
      c("XLE", "Energy", "price"),
      c("XLV", "Health care", "price"),
      c("XLP", "Consumer staples", "price"),
      c("XLU", "Utilities", "price"),
      c("XLRE", "Real estate", "price")
    ),
    ratios = list(
      c("XLK", "^GSPC", "Technology / S&P 500", "Up = technology leads the index"),
      c("XLC", "^GSPC", "Communication services / S&P 500", "Up = communication services leads the index"),
      c("XLY", "^GSPC", "Consumer discretionary / S&P 500", "Up = consumer discretionary leads the index"),
      c("XLF", "^GSPC", "Financials / S&P 500", "Up = financials leads the index"),
      c("XLI", "^GSPC", "Industrials / S&P 500", "Up = industrials leads the index"),
      c("XLB", "^GSPC", "Materials / S&P 500", "Up = materials leads the index"),
      c("XLE", "^GSPC", "Energy / S&P 500", "Up = energy leads the index"),
      c("XLV", "^GSPC", "Health care / S&P 500", "Up = health care leads the index"),
      c("XLP", "^GSPC", "Consumer staples / S&P 500", "Up = consumer staples leads the index"),
      c("XLU", "^GSPC", "Utilities / S&P 500", "Up = utilities leads the index"),
      c("XLRE", "^GSPC", "Real estate / S&P 500", "Up = real estate leads the index")
    )
  ),
  list(
    id = "fx", title = "Currencies",
    instruments = list(
      c("DX-Y.NYB", "Dollar index (DXY)", "fx"),
      c("EURUSD=X", "EUR/USD", "fx"),
      c("GBPUSD=X", "GBP/USD", "fx"),
      c("AUDUSD=X", "AUD/USD", "fx"),
      c("USDCAD=X", "USD/CAD", "fx"),
      c("USDJPY=X", "USD/JPY", "fx"),
      c("USDCHF=X", "USD/CHF", "fx"),
      c("AUDJPY=X", "AUD/JPY (risk barometer)", "fx"),
      c("CNY=X", "USD/CNY (onshore yuan)", "fx"),
      c("USDMXN=X", "USD/MXN", "fx"),
      c("USDBRL=X", "USD/BRL", "fx"),
      c("USDKRW=X", "USD/KRW", "fx"),
      c("BTC-USD", "Bitcoin", "price")
    ),
    ratios = list()
  ),
  list(
    id = "metals", title = "Precious metals",
    instruments = list(
      c("GC=F", "Gold", "price"),
      c("SI=F", "Silver", "price"),
      c("PL=F", "Platinum", "price"),
      c("PA=F", "Palladium", "price"),
      c("GDX", "Gold miners", "price")
    ),
    ratios = list(
      c("GC=F", "SI=F", "Gold / Silver", "Up = defensive metal demand; down = reflation, silver leads"),
      c("GDX", "GC=F", "Miners / Gold", "Up = equity investors confirm the gold move"),
      c("GC=F", "^GSPC", "Gold / S&P 500", "Up = gold outperforms equities (currency debasement / risk-off)"),
      c("GC=F", "EURUSD=X", "Gold in euros", "Gold priced in EUR; a fall smaller than in USD = the dollar, not gold, is moving"),
      c("GC=F", "CHFUSD=X", "Gold in Swiss francs", "Gold priced in CHF (book currency)")
    )
  ),
  list(
    id = "oil", title = "Oil and energy",
    instruments = list(
      c("CL=F", "WTI crude", "price"),
      c("BZ=F", "Brent crude", "price"),
      c("RB=F", "RBOB gasoline", "price"),
      c("HO=F", "Heating oil / diesel (ULSD)", "price"),
      c("NG=F", "Natural gas", "price"),
      c("XLE", "Energy stocks", "price")
    ),
    ratios = list(
      c("XLE", "CL=F", "Energy stocks / WTI", "Up = equities price a durable oil level"),
      c("XLE", "^GSPC", "Energy / S&P 500", "Up = energy sector leadership"),
      c("OIH", "CL=F", "Oil services / WTI", "Up = equity investors confirm the oil move"),
      c("FCG", "NG=F", "Gas producers / natural gas", "Up = equity investors confirm the gas move"),
      c("HO=F", "CL=F", "Diesel / crude", "Up = distillate cracks widening (diesel tighter than crude)")
    )
  ),
  list(
    id = "commod", title = "Other commodities",
    instruments = list(
      c("HG=F", "Copper", "price"),
      c("ZW=F", "Wheat", "price"),
      c("ZC=F", "Corn", "price"),
      c("CC=F", "Cocoa", "price"),
      c("KC=F", "Coffee", "price"),
      c("SB=F", "Sugar", "price"),
      c("LBR=F", "Lumber", "price"),
      c("SRUUF", "Uranium (Sprott physical trust)", "price"),
      c("URA", "Uranium miners", "price"),
      c("DBC", "Broad commodities", "price")
    ),
    ratios = list(
      c("HG=F", "GC=F", "Copper / Gold", "Up = growth expectations rising; tracks the 10-year yield"),
      c("URA", "SRUUF", "Uranium miners / uranium", "Up = equity investors confirm the uranium move"),
      c("COPX", "HG=F", "Copper miners / copper", "Up = equity investors confirm the copper move"),
      c("MOO", "DBA", "Agribusiness / ag futures", "Up = equity investors confirm the ag move"),
      c("DBC", "^GSPC", "Commodities / S&P 500", "Up = real assets beat financial assets (inflation regime)")
    )
  ),
  list(
    id = "rates", title = "Rates and credit",
    instruments = list(
      c("^IRX", "3-month bill", "yield"),
      c("^FVX", "5-year yield", "yield"),
      c("^TNX", "10-year yield", "yield"),
      c("^TYX", "30-year yield", "yield"),
      c("IS0L.DE", "German Bunds 10y+ (price; down = yields up)", "price"),
      c("GLTL.L", "UK Gilts 15y+ (price; down = yields up)", "price"),
      c("^MOVE", "MOVE (bond volatility)", "price"),
      c("TLT", "Long Treasuries (TLT)", "price")
    ),
    ratios = list(
      c("HYG", "IEF", "High yield / Treasuries", "Up = credit risk appetite; down = credit stress"),
      c("LQD", "IEF", "Investment grade / Treasuries", "Up = IG spreads tightening; down = stress reaching quality credit"),
      c("TIP", "IEF", "TIPS / Treasuries", "Up = inflation expectations rising")
    ),
    spreads = list(
      c("^TNX", "^IRX", "10-year minus 3-month", "Rising = steepening"),
      c("^TYX", "^FVX", "30-year minus 5-year", "Rising = steepening, term premium building")
    )
  ),
  list(
    id = "world", title = "World markets",
    instruments = list(
      c("^STOXX50E", "Euro Stoxx 50", "price"),
      c("^GDAXI", "DAX", "price"),
      c("^SSMI", "SMI", "price"),
      c("^FCHI", "CAC 40", "price"),
      c("^IBEX", "IBEX 35 (Spain)", "price"),
      c("FTSEMIB.MI", "FTSE MIB (Italy)", "price"),
      c("^BVSP", "Bovespa (Brazil)", "price"),
      c("^MXX", "IPC (Mexico)", "price"),
      c("^N225", "Nikkei 225", "price"),
      c("^KS11", "KOSPI", "price"),
      c("^HSCE", "HSCEI (China offshore, HK-listed)", "price"),
      c("000001.SS", "Shanghai Composite (China onshore)", "price"),
      c("EEM", "Emerging markets (USD)", "price"),
      c("FXI", "China large caps (USD)", "price"),
      c("EWL", "Switzerland (USD)", "price")
    ),
    ratios = list(
      c("EEM", "^GSPC", "Emerging / S&P 500", "Up = money leaves the US for EM (usually with a weaker dollar)")
    )
  )
)

# BOT sector map rows = correlation groups (ScannerUniverse.Cluster, hand-reviewed
# 2026-10-02) that hold at least one Tickers.BOT_Eligible name; see group_map().
# Drivers per group: symbol = sensitivity sign (+1 = group rises with the driver,
# -1 = falls). Judgement values, untested (TODO 105). Every mapped group must have
# an entry: a renamed or new group after a cluster review stops the map until
# its drivers are written here.
GROUP_DRIVERS <- list(
  "Agriculture - Crop inputs"                     = c("ZC=F" = 1, "ZW=F" = 1),
  "Agriculture - Fertilizers & chemicals"         = c("ZC=F" = 1, "ZW=F" = 1),
  "Agriculture - Grain processors"                = c("ZC=F" = 1, "DX-Y.NYB" = -1),
  "AI infra - Data-centre REITs"                  = c("^NDX" = 1, "^TNX" = -1),
  "AI infra - Semis equipment"                    = c("^NDX" = 1, "SMH/^GSPC" = 1, "^KS11" = 1),
  "AI infra - Semis hardware"                     = c("^NDX" = 1, "SMH/^GSPC" = 1, "^KS11" = 1),
  "China - Internet offshore"                     = c("^HSCE" = 1, "CNY=X" = -1),
  "Consumer - Autos"                              = c("XLY/XLP" = 1, "^TNX" = -1),
  "Consumer - Beverages & alcohol"                = c("XLY/XLP" = -1, "^TNX" = -1),
  "Consumer - Food & household staples"           = c("XLY/XLP" = -1, "^TNX" = -1),
  "Consumer - Leisure & travel"                   = c("XLY/XLP" = 1, "CL=F" = -1),
  "Consumer - Tobacco"                            = c("XLY/XLP" = -1, "^TNX" = -1),
  "Defence - European & growth defence"           = c("^STOXX50E" = 1, "EURUSD=X" = 1),
  "Defence - Government IT services"              = c("XLY/XLP" = -1, "^TNX" = -1),
  "Defence - Primes"                              = c("XLY/XLP" = -1),
  "Energy - Canadian integrated oil"              = c("CL=F" = 1, "USDCAD=X" = -1),
  "Energy - Integrated European oil"              = c("BZ=F" = 1, "^STOXX50E" = 1),
  "Energy - Integrated oil & E&P"                 = c("CL=F" = 1, "BZ=F" = 1),
  "Energy - Nuclear & critical minerals"          = c("SRUUF" = 1, "000001.SS" = 1, "DX-Y.NYB" = -1),
  "Energy - Oil services & drilling"              = c("CL=F" = 1),
  "Energy - Refiners"                             = c("HO=F/CL=F" = 1),
  "Energy - Tankers"                              = c("BZ=F" = 1),
  "Energy - US natural gas"                       = c("NG=F" = 1),
  "Europe - European cyclicals"                   = c("^STOXX50E" = 1, "EURUSD=X" = 1, "HG=F" = 1),
  "Europe - European Financials"                  = c("^STOXX50E" = 1, "EURUSD=X" = 1, "HYG/IEF" = 1),
  "Financials - Insurers"                         = c("^TNX" = 1, "HYG/IEF" = 1),
  "Financials - Payments & Berkshire"             = c("XLY/XLP" = 1),
  "Financials - US money-centre banks"            = c("^TNX" = 1, "HYG/IEF" = 1),
  "Financials - US regional banks & card issuers" = c("^TNX" = 1, "HYG/IEF" = 1, "IWM/^GSPC" = 1),
  "Healthcare - Big pharma"                       = c("XLY/XLP" = -1, "^TNX" = -1),
  "Healthcare - Bio pharma"                       = c("^TNX" = -1, "IWM/^GSPC" = 1),
  "Healthcare - Life-science tools"               = c("^TNX" = -1),
  "Healthcare - Managed care"                     = c("XLY/XLP" = -1),
  "Healthcare - Medtech"                          = c("XLY/XLP" = -1, "^TNX" = -1),
  "Housing - Homebuilders & building"             = c("^TNX" = -1, "IWM/^GSPC" = 1),
  "Industrials - Automation & analog"             = c("HG=F" = 1, "^NDX" = 1),
  "Industrials - Commercial aerospace"            = c("CL=F" = -1, "XLY/XLP" = 1),
  "Industrials - Machinery & diversified"         = c("HG=F" = 1, "IWM/^GSPC" = 1),
  "Industrials - Transports"                      = c("CL=F" = -1, "IWM/^GSPC" = 1),
  "Materials - Industrial gases"                  = c("HG=F" = 1),
  "Metals - Bullion ETFs"                         = c("GC=F" = 1, "SI=F" = 1, "DX-Y.NYB" = -1),
  "Metals - Copper & base metals"                 = c("HG=F" = 1, "HG=F/GC=F" = 1, "DX-Y.NYB" = -1, "^HSCE" = 1),
  "Metals - Gold miners"                          = c("GC=F" = 1, "DX-Y.NYB" = -1, "TIP/IEF" = 1),
  "Metals - Silver & precious-metal miners"       = c("SI=F" = 1, "GC=F/SI=F" = -1, "DX-Y.NYB" = -1),
  "Metals - Steel"                                = c("HG=F" = 1, "000001.SS" = 1),
  "Mixed - Business services"                     = c("^TNX" = -1),
  "Platforms - E-commerce growth"                 = c("XLY/XLP" = 1, "^NDX" = 1),
  "Real estate - REITs"                           = c("^TNX" = -1, "HYG/IEF" = 1),
  "Software - Cloud & cybersecurity"              = c("^NDX" = 1, "^TNX" = -1),
  "Software - Enterprise software"                = c("^NDX" = 1, "^TNX" = -1),
  "Software - Speculative growth & crypto"        = c("BTC-USD" = 1, "^NDX" = 1, "HYG/IEF" = 1),
  "Tech - Networking & hardware"                  = c("^NDX" = 1),
  "Telecom - Telecom & defensive retail"          = c("XLY/XLP" = -1, "^TNX" = -1),
  "Utilities - Power generation"                  = c("^TNX" = -1, "SMH/^GSPC" = 1),
  "Utilities - Regulated utilities"               = c("^TNX" = -1)
)

BENCHMARK <- "^GSPC"
# Distribution-paying bond/credit ETFs: use dividend-adjusted closes, otherwise each
# monthly ex-date reads as a price fall (HYG ~0.5%) and shows up as false credit stress.
ADJUSTED_SYMBOLS <- c("HYG", "LQD", "IEF", "TIP", "TLT")
HISTORY_DAYS <- 520   # calendar days: 200-day average, 1-year chart, and 252-session state ranges for 60 days of history
