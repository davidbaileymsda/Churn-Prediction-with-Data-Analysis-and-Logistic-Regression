# Install Packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)

# Set the Working Directory for the session
setwd("C:/Users/david/Google Drive/Data/GitHub/Data-Mining-Project-with-R")

# Load the Telco Customer Churn data set
customer.churn <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header = TRUE)

# View the structure of the data frame
str(customer.churn)

# View summary statistics for the uncleaned data set and check for missing values
summary(customer.churn)

# Create new data.frame for 11 records with missing TotalCharges value
missing.values <- data.frame(subset(customer.churn,is.na(TotalCharges)))

?plot









### Clean Up ###

# Clear Packages
p_unload(all)

# Clear Plots, if there are any
dev.off()

# Clear global environment
rm(list = ls())

# Clear Console CTRL+L
cat("\014")
