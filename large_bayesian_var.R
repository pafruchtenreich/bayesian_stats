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

# Remove seasonality and stationarize
data <- seasonal_adjustment_and_stationarity(data, date_col = "Date")

arrow::write_parquet(data, "data/processed_data.parquet")


test <- lbvar::lbvar(
  data[, .SD, .SDcols = !("Date")],
  p = 1,
  delta = 0,
  lambda = 0.05,
  xreg = NULL,
  ps = FALSE,
  tau = 10 * 0.05
)
