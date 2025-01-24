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
data <- data.table(read_csv("macrodata.csv"))
names(data) <- variables_name

##################################
##### DESCRIPTIVE STATISTICS #####
##################################

source("src/descriptive_statistics.R")
correlation_matrix(data)
