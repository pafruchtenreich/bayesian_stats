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

data <- data.table(read_csv("macrodata.csv"))
