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
summary_statistics(data, latex = TRUE)

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

first_model_variables <- c("Unemployment_Rate", "Effective_Federal_Funds_Rate", "CPI_All_Items")

second_model_variables <- c(
  first_model_variables,
  "PPI_Intermediate_Materials",
  "Total_Reserves",
  "M2_Money_Stock"
)

third_model_variables <- c(
  second_model_variables,
  "Real_Personal_Income",
  "Real_Personal_Consumption",
  "Industrial_Production_Index",
  "Housing_Starts",
  "PPI_Finished_Goods",
  "M1_Money_Stock",
  "SP500_Index",
  "Ten_Year_Treasury_Rate",
  "USD_Swiss_Exchange_Rate",
  "USD_Canada_Exchange_Rate",
  "USD_UK_Exchange_Rate"
)

all_model_vars <- list(
  first_model_variables = first_model_variables,
  second_model_variables = second_model_variables,
  third_model_variables = third_model_variables,
  all_variables_model = setdiff(names(training_data), "Date")
)


lag_candidates <- 1:12

################
# Bayesian VAR #
################
mse_bvar <- evaluate_bvar_models(
  all_model_vars,
  training_data,
  zoo_data,
  lag_candidates,
  diffCount,
  base_val
)

mse_bvar$first_model_variables$variables <- first_model_variables
mse_bvar$second_model_variables$variables <- second_model_variables
mse_bvar$third_model_variables$variables <- third_model_variables
mse_bvar$all_variables_model$variables <- setdiff(names(training_data), "Date")

######################
# Large Bayesian VAR #
######################
mse_large_bvar <- evaluate_large_bvar_models(
  training_data = training_data,
  model_parameters = mse_bvar,
  zoo_data = zoo_data,
  diffCount = diffCount,
  base_val = base_val,
  horizon = 12
)
