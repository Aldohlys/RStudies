# intermarket_config.R — Intermarket panels: sections, ratios, BOT sector groups
#
# Section order follows the macro-outlook reading order:
#   stocks -> currencies -> precious metals -> oil -> other commodities -> rates -> world markets
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
      c("^FTSE", "FTSE 100", "price"),
      c("^BVSP", "Bovespa (Brazil)", "price"),
      c("^MXX", "IPC (Mexico)", "price"),
      c("^N225", "Nikkei 225", "price"),
      c("^KS11", "KOSPI", "price"),
      c("^HSI", "Hang Seng", "price"),
      c("000001.SS", "Shanghai Composite", "price"),
      c("EEM", "Emerging markets (USD)", "price")
    ),
    ratios = list(
      c("EEM", "^GSPC", "Emerging / S&P 500", "Up = money leaves the US for EM (usually with a weaker dollar)")
    )
  )
)

# BOT universe sector groups (Strategies/tradable_universe_*.csv, column `bench`).
# drivers: symbol = sensitivity sign (+1 = group rises with the driver, -1 = falls).
SECTOR_GROUPS <- list(
  list(group = "Integrated energy", bench = "XLE", drivers = c("CL=F" = 1, "BZ=F" = 1)),
  list(group = "Energy E&P", bench = "XOP", drivers = c("CL=F" = 1, "BZ=F" = 1)),
  list(group = "Oil services", bench = "OIH", drivers = c("CL=F" = 1)),
  list(group = "Gas producers", bench = "FCG", drivers = c("NG=F" = 1)),
  list(group = "US banks", bench = "KRE", drivers = c("^TNX" = 1, "HYG/IEF" = 1, "IWM/^GSPC" = 1)),
  list(group = "EU banks", bench = "EUFN", drivers = c("^STOXX50E" = 1, "EURUSD=X" = 1, "HYG/IEF" = 1)),
  list(group = "Pharma", bench = "PPH", drivers = c("XLY/XLP" = -1, "^TNX" = -1)),
  list(group = "Homebuilders", bench = "ITB", drivers = c("^TNX" = -1, "IWM/^GSPC" = 1)),
  list(group = "REITs", bench = "VNQ", drivers = c("^TNX" = -1, "HYG/IEF" = 1)),
  list(group = "Metals & mining", bench = "XME", drivers = c("HG=F" = 1, "DX-Y.NYB" = -1, "^HSI" = 1)),
  list(group = "Copper miners", bench = "COPX", drivers = c("HG=F" = 1, "HG=F/GC=F" = 1, "DX-Y.NYB" = -1)),
  list(group = "Steel", bench = "SLX", drivers = c("HG=F" = 1, "000001.SS" = 1)),
  list(group = "Gold miners", bench = "GDX", drivers = c("GC=F" = 1, "DX-Y.NYB" = -1, "TIP/IEF" = 1)),
  list(group = "Silver miners", bench = "SIL", drivers = c("SI=F" = 1, "GC=F/SI=F" = -1, "DX-Y.NYB" = -1)),
  list(group = "Lithium / battery", bench = "LIT", drivers = c("000001.SS" = 1, "CNY=X" = -1)),
  list(group = "Rare earths", bench = "REMX", drivers = c("000001.SS" = 1, "DX-Y.NYB" = -1)),
  list(group = "Uranium", bench = "URA", drivers = c("SRUUF" = 1)),
  list(group = "Agriculture", bench = "MOO", drivers = c("ZC=F" = 1, "ZW=F" = 1)),
  list(group = "Ag futures", bench = "DBA", drivers = c("ZC=F" = 1, "ZW=F" = 1, "DX-Y.NYB" = -1)),
  list(group = "Staples / tobacco / alcohol", bench = "XLP", drivers = c("XLY/XLP" = -1, "^TNX" = -1)),
  list(group = "China internet", bench = "KWEB", drivers = c("^HSI" = 1, "CNY=X" = -1)),
  list(group = "China large caps", bench = "FXI", drivers = c("^HSI" = 1, "CNY=X" = -1)),
  list(group = "Autos", bench = "CARZ", drivers = c("XLY/XLP" = 1, "^TNX" = -1)),
  list(group = "Retail", bench = "XRT", drivers = c("XLY/XLP" = 1, "IWM/^GSPC" = 1)),
  list(group = "Semis / AI", bench = "SMH", drivers = c("^NDX" = 1, "SMH/^GSPC" = 1, "^KS11" = 1)),
  list(group = "Software", bench = "IGV", drivers = c("^NDX" = 1, "^TNX" = -1)),
  list(group = "Brokers / crypto", bench = "IAI", drivers = c("BTC-USD" = 1, "HYG/IEF" = 1)),
  list(group = "Tech", bench = "XLK", drivers = c("^NDX" = 1, "^TNX" = -1)),
  list(group = "Healthcare", bench = "XLV", drivers = c("XLY/XLP" = -1)),
  list(group = "Materials", bench = "XLB", drivers = c("HG=F" = 1, "DX-Y.NYB" = -1)),
  list(group = "Consumer discretionary", bench = "XLY", drivers = c("XLY/XLP" = 1, "^TNX" = -1)),
  list(group = "Emerging markets", bench = "EEM", drivers = c("DX-Y.NYB" = -1, "^KS11" = 1, "HYG/IEF" = 1)),
  list(group = "Swiss stocks", bench = "EWL", drivers = c("^SSMI" = 1, "^STOXX50E" = 1))
)

BENCHMARK <- "^GSPC"
# Distribution-paying bond/credit ETFs: use dividend-adjusted closes, otherwise each
# monthly ex-date reads as a price fall (HYG ~0.5%) and shows up as false credit stress.
ADJUSTED_SYMBOLS <- c("HYG", "LQD", "IEF", "TIP", "TLT")
HISTORY_DAYS <- 450   # calendar days: covers the 200-day average plus a 1-year chart
