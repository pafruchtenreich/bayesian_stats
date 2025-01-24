library(data.table)
library(forecast)
library(seasonal)
library(tseries)
library(lubridate)

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

  for (v in vars) {
    # Convert the column to a time-series object for seasonal adjustment
    tsVar <- ts(
      data[[v]],
      frequency = 12,
      start = c(
        year(data[[date_col]][1]),
        month(data[[date_col]][1])
      )
    )

    # Seasonal adjustment
    fitSeas <- seas(tsVar)
    seasadj_series <- final(fitSeas)
    data[[v]] <- as.numeric(seasadj_series)

    # Check stationarity with ADF test on the seasonally adjusted data
    adf_result <- adf.test(data[[v]])

    # If non-stationary, difference once
    if (adf_result$p.value > 0.05) {
      message(sprintf(
        "Variable %s is non-stationary (p-value = %.4f). Differencing once...",
        v, adf_result$p.value
      ))

      data[[v]] <- c(NA, diff(data[[v]]))
      data <- na.omit(data)

      # Re-check stationarity after first difference
      adf_result_2 <- adf.test(data[[v]])
      if (adf_result_2$p.value > 0.05) {
        message(sprintf(
          "Variable %s is still non-stationary (p-value = %.4f). Differencing twice...",
          v, adf_result_2$p.value
        ))

        data[[v]] <- c(NA, diff(data[[v]]))
        data <- na.omit(data)
      }
    }
  }

  # Final stationarity report
  message("Final stationarity check (ADF p-values):")
  for (v in vars) {
    if (v %in% names(data)) {
      adf_res_final <- adf.test(data[[v]])
      message(sprintf("  %s: %.4f", v, adf_res_final$p.value))
    } else {
      message(sprintf("  %s was removed (possibly due to NA omission).", v))
    }
  }

  return(data)
}
