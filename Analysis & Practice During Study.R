# Install Packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)

# Set the Working Directory for the session
setwd("C:/Users/david/Desktop/Data Mining & Analytics II Performance Assessment")

# Load the Telco Customer Churn dataset
customer.churn <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header = TRUE)

# View the structure of the data frame
str(customer.churn)

# View summary statistics for the uncleaned dataset and check for missing values
summary(customer.churn)

# Create new data.frame for 11 records with missing TotalCharges value
missing.values <- data.frame(subset(customer.churn,is.na(TotalCharges)))

?plot









### Clean Up ###

# Clear Packages
detach("package:ggplot2", unload = TRUE)

# Clear Plots, if there are any
dev.off()

# Clear global environment
rm(list = ls())

# Clear Console CTRL+L
cat("\014")
