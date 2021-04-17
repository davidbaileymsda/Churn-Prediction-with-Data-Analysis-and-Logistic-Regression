# Install Packages
pacman::p_load(pacman, dplyr, plyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr)

# Set the Working Directory for the session
setwd("C:/Users/david/Google Drive/Data/GitHub/Data-Mining-Project-with-R")

# Load the Telco Customer Churn data set
customer.churn <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header = TRUE)

# View the structure of the data frame
str(customer.churn)

# View summary statistics for the uncleaned data set and check for missing values
summary(customer.churn)

# Remove 11 records with missing values in TotalCharges variable
customer.churn <- customer.churn[complete.cases(customer.churn),]
summary(customer.churn)

# Remove Customer ID variable
customer.churn$customerID <- NULL

# Review each categorical variable for cleaning/wrangling
unique(customer.churn$gender)
unique(customer.churn$SeniorCitizen)
unique(customer.churn$Partner)
unique(customer.churn$Dependents)
unique(customer.churn$PhoneService)
unique(customer.churn$MultipleLines)
unique(customer.churn$InternetService)
unique(customer.churn$OnlineSecurity)
unique(customer.churn$OnlineBackup)
unique(customer.churn$DeviceProtection)
unique(customer.churn$TechSupport)
unique(customer.churn$StreamingTV)
unique(customer.churn$StreamingMovies)
unique(customer.churn$Contract)
unique(customer.churn$PaperlessBilling)
unique(customer.churn$PaymentMethod)
unique(customer.churn$Churn)

# Clean the needed categorical values
customer.churn$SeniorCitizen <- as.factor(mapvalues(customer.churn$SeniorCitizen, from = c("1", "0"), to = c("Yes", "No")))

customer.churn$MultipleLines <- as.factor(mapvalues(customer.churn$MultipleLines, from = c("No phone service"), to = c("No")))

customer.churn$OnlineSecurity <- as.factor(mapvalues(customer.churn$OnlineSecurity, from = c("No internet service"), to = c("No")))
customer.churn$OnlineBackup <- as.factor(mapvalues(customer.churn$OnlineBackup, from = c("No internet service"), to = c("No")))
customer.churn$DeviceProtection <- as.factor(mapvalues(customer.churn$DeviceProtection, from = c("No internet service"), to = c("No")))
customer.churn$TechSupport <- as.factor(mapvalues(customer.churn$TechSupport, from = c("No internet service"), to = c("No")))
customer.churn$StreamingTV <- as.factor(mapvalues(customer.churn$StreamingTV, from = c("No internet service"), to = c("No")))
customer.churn$StreamingMovies <- as.factor(mapvalues(customer.churn$StreamingMovies, from = c("No internet service"), to = c("No")))

unique(customer.churn$SeniorCitizen)
unique(customer.churn$MultipleLines)
unique(customer.churn$OnlineSecurity)
unique(customer.churn$OnlineBackup)
unique(customer.churn$DeviceProtection)
unique(customer.churn$TechSupport)
unique(customer.churn$StreamingTV)
unique(customer.churn$StreamingMovies)

# Convert tenure variable into years factor
Tenure.Years <- function(tenure) {
  if (tenure >= 0 & tenure <= 12) 
    return("First Year")
  else if (tenure > 12 & tenure <= 24)
    return("Second Year")
  else if (tenure > 24 & tenure <= 36)
    return("Third Year")
  else if (tenure > 36 & tenure <= 48)
    return("Fourth Year")
  else if (tenure > 48 & tenure <= 60)
    return("Fifth Year")
  else (tenure >60)
    return("Sixth Year or More")
}

# Apply function to new variable and delete old tenure variable
customer.churn$Tenure.Years <- sapply(customer.churn$tenure, Tenure.Years)
customer.churn$Tenure.Years <- as.factor(customer.churn$Tenure.Years)
unique(customer.churn$Tenure.Years)

# Remove old tenure variable
customer.churn$tenure <- NULL

View(customer.churn)


# Initial Distribution Charts



# Principal Component Analysis to select most important features

# Correlation Matrix

# Chart to select the number of principal components

# Create new data set to be used in Logistic Regression





### Clean Up ###

# Clear Packages
p_unload(all)

# Clear Plots, if there are any
dev.off()

# Clear global environment
rm(list = ls())

# Clear Console CTRL+L
cat("\014")
