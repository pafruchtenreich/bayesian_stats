library(data.table)
library(zoo)
library(seasonal)
library(tseries)

variables_name <- c(
  "Date", # "...1"
  "Real_Personal_Income", # "RPI"
  "Industrial_Production_Index", # "INDPRO"
  "Civilian_Employment", # "CE16OV"
  "Unemployment_Rate", # "UNRATE"
  "Total_Nonfarm_Employees", # "PAYEMS"
  "Goods_Producing_Employees", # "USGOOD"
  "Trade_Transportation_Utilities", # "USTPU"
  "Housing_Starts", # "HOUST"
  "Private_Housing_Permits", # "PERMIT"
  "Real_Personal_Consumption", # "DPCERA3M086SBEA"
  "Manufacturers_Total_Inventory", # "AMTMTI"
  "New_Orders_Total_Manufacturing", # "AMTMNO"
  "New_Orders_Consumer_Goods", # "ACOGNO"
  "Unfilled_Orders_Durable_Goods", # "AMDMUO"
  "Total_Business_Inventories", # "BUSINV"
  "Inventory_to_Sales_Ratio", # "ISRATIO"
  "M1_Money_Stock", # "M1SL"
  "M2_Money_Stock", # "M2SL"
  "Total_Reserves", # "TOTRESNS"
  "Commercial_Industrial_Loans", # "BUSLOANS"
  "Real_Estate_Loans", # "REALLN"
  "Consumer_Loans", # "DTCTHFNM"
  "Effective_Federal_Funds_Rate", # "FEDFUNDS"
  "Three_Month_Treasury_Bill", # "TB3MS"
  "Six_Month_Treasury_Bill", # "TB6MS"
  "One_Year_Treasury_Rate", # "GS1"
  "Five_Year_Treasury_Rate", # "GS5"
  "Ten_Year_Treasury_Rate", # "GS10"
  "Moody_AAA_Corp_Bond_Yield", # "AAA"
  "Three_Month_Treasury_Spread", # "TB3SMFFM"
  "One_Year_Treasury_Spread", # "T1YFFM"
  "Moody_AAA_Funds_Spread", # "AAAFFM"
  "USD_Swiss_Exchange_Rate", # "EXSZUS"
  "USD_Canada_Exchange_Rate", # "EXCAUS"
  "USD_UK_Exchange_Rate", # "EXUSUK"
  "PPI_Finished_Goods", # "WPSFD49207"
  "PPI_Intermediate_Materials", # "WPSID61"
  "CPI_All_Items", # "CPIAUCSL"
  "SP500_Index", # "SP500"
  "NASDAQ_Index", # "NASDAQ"
  "Gold_Bar", # "GOLDBAR"
  "Regime", # "Regime"
  "Price_Earnings_Ratio", # "P/E"
  "Dividend_Yield" # "Dividend Yield"
)

seasonal_adjustment_and_stationarity <- function(data, date_col = "Date") {
  vars <- setdiff(names(data), date_col)

  # Track how many times each variable is differenced
  diffCount <- setNames(rep(0, length(vars)), vars)

  # Track base values needed to invert differencing later
  baseVal <- vector("list", length = length(vars))
  names(baseVal) <- vars

  for (v in vars) {
    # Build a zoo object for variable 'v'
    z <- zoo(data[[v]], order.by = data[[date_col]])
    z <- na.trim(z)

    # Seasonal adjustment (Convert zoo->ts->seas->zoo)
    start_year <- as.numeric(format(start(z), "%Y"))
    start_month <- as.numeric(format(start(z), "%m"))
    z_ts <- ts(coredata(z), frequency = 12, start = c(start_year, start_month))

    fit <- seas(z_ts)
    z_sa <- zoo(as.numeric(final(fit)), order.by = index(z))

    # ADF test; difference if p>0.05
    p_val_1 <- adf.test(coredata(z_sa))$p.value
    if (p_val_1 > 0.05) {
      message(sprintf("Variable '%s' p=%.4f > 0.05. Differencing once...", v, p_val_1))
      z_sa <- diff(z_sa)
      z_sa <- na.trim(z_sa)
      diffCount[v] <- diffCount[v] + 1

      p_val_2 <- adf.test(coredata(z_sa))$p.value
      if (p_val_2 > 0.05) {
        message(sprintf("Variable '%s' still p=%.4f > 0.05. Differencing twice...", v, p_val_2))
        z_sa <- diff(z_sa)
        z_sa <- na.trim(z_sa)
        diffCount[v] <- diffCount[v] + 1
      }
    }

    # Merge back to 'data' in place
    z_final <- merge(z, z_sa, all = TRUE)
    data[[v]] <- as.numeric(z_final[, 2])
  }

  # Final stationarity check
  message("\nFinal stationarity check:")
  for (v in vars) {
    z_check <- zoo(data[[v]], order.by = data[[date_col]])
    z_check <- na.omit(z_check)
    if (length(z_check) < 2) {
      message(sprintf("  %s => Not enough data. (differenced %d time[s])", v, diffCount[v]))
      next
    }
    p_val_final <- adf.test(coredata(z_check))$p.value
    message(sprintf("  %s => p=%.4f, differenced %d time[s]", v, p_val_final, diffCount[v]))
  }

  # Return final data plus differencing counts
  return(list(
    data      = data,
    diffCount = diffCount
  ))
}
