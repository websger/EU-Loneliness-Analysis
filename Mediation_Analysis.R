#Analysis of Mediation
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
results_mediation <- mediation::mediate(model.m = lm(relationship_need_num ~ gender_num, data = df_loneliness_clean),
                                        model.y = lm(loneliness_total ~ gender_num + relationship_need_num, data = df_loneliness_clean),
                                        treat = "gender_num",
                                        mediator = "relationship_need_num", 
                                        sims = 1000)

#Show Results
summary(results_mediation)