#Analysis of Regression
# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(mediation)
library(psych)
library(car)
library(readr)
library(lmtest)

#Import Clean Data
df_loneliness_clean <- read_csv("data_clean/eu_loneliness_clean.csv")
View(eu_loneliness_clean)

#Analysis
results_regression <- lm(loneliness_total ~ worklife_balance_num, data = df_loneliness_clean)

#Show Results
summary(results_regression)