############################
##### LOADING PACKAGES #####
############################

# Load and manage project environment
library(renv)
renv::activate() # Activate the renv environment for this project
renv::restore() # Restore the renv environment from the lockfile

##########################################
##### IMPORTING & PREPROCESSING DATA #####
##########################################

source("src/data_preprocess.R")

# Import data
data <- fread("data/raw_data.csv")

# Change variable names
names(data) <- variables_name

# Remove regime column
data <- data[, .SD, .SDcols = !("Regime")]

# Adjust date format
data$Date <- as.Date(paste0(data$Date, "-01"), format = "%Y-%m-%d")
data <- data[order(Date)]

# Exclude 2019 and 2020 (Covid)
data <- data[!(year(Date) %in% c(2019, 2020))]

##################################
##### DESCRIPTIVE STATISTICS #####
##################################

source("src/descriptive_statistics.R")

# Mean, Std, Min, Max
summary_statistics(data, latex = FALSE)

# Correlation matrix
correlation_matrix(data)

#####################
##### MODELLING #####
#####################

source("src/predictions.R")

# Save base values of data for reconstruction
base_val <- data[order(Date)][1:2]

# Remove seasonality and stationarize
differentiated_data <- seasonal_adjustment_and_stationarity(data, date_col = "Date")
data <- differentiated_data$data
diffCount <- differentiated_data$diffCount

# Zoo data format
zoo_data <- zoo(data[, .SD, .SDcols = !("Date")], order.by = data$Date)

# Sort by Date
data <- data[order(Date)]

# Exclude the last 12 observations
training_data <- data[1:(.N - 12)]

######################
# Large Bayesian VAR #
######################
model_large_bvar <- lbvar::lbvar(
  training_data[, .SD, .SDcols = !("Date")],
  p = 8,
  delta = 0,
  lambda = 0.05,
  xreg = NULL,
  ps = FALSE,
  tau = 10 * 0.05
)

predictions_large_bvar <- data.table(predict(model_large_bvar, h = 12))
results_large_bvar <- forecast_plot(zoo_data, predictions_large_bvar, variable = "Unemployment_Rate", diffCount = diffCount, base_values = base_val, horizon = 12)
compute_mse(results_large_bvar)


################
# Bayesian VAR #
################
model_bvar <- BVAR::bvar(training_data[, .SD, .SDcols = c("Unemployment_Rate", "Industrial_Production_Index", "Real_Personal_Consumption")][3:length(training_data)], lags = 1)

predictions_bvar <- predict(test, horizon = 12)
predictions_bvar <- process_predictions(predictions_bvar, c("Unemployment_Rate", "Industrial_Production_Index", "Real_Personal_Consumption"))
results_bvar <- forecast_plot(zoo_data, dt_50pct, variable = "Unemployment_Rate", diffCount = diffCount, base_values = base_val, horizon = 12)

compute_mse(results_bvar)
