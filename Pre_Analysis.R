#Pre-Analysis of Regression
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

#Viewing Variables
hist(df_loneliness_clean$gender_num)
hist(df_loneliness_clean$relationship_need_num)
hist(df_loneliness_clean$loneliness_total)

#Checking Linearity of Regression
model_regression <- lm(loneliness_total ~ worklife_balance_num, data = df_loneliness_clean)

plot_linearity_1 <- crPlots(model_regression, pch=NA) #linearity visually appropriate

#Checking normal distribution
residuals_regression <- residuals(model_regression)
histogram_residuals_regression <- hist(residuals_regression)
qqnorm_residuals_regression <- qqnorm(residuals_regression)
qqline_residuals_regression <- qqline(residuals_regression) #Normal distribution visually appropriate

#homoskedasticity
homoskedasticity_regression <- bptest(model_regression)
homoskedasticity_regression

df_res <- data.frame(fitted = fitted(model_regression),
                     resid  = resid(model_regression))

residuals_jitter <- ggplot(df_res, aes(fitted, resid)) +
  geom_jitter(alpha = 0.1, width = 0.1, height = 0.1) +
  geom_hline(yintercept = 0, color = "blue")
residuals_jitter #cloud of data seems appropriate

residuals_density <- ggplot(df_res, aes(fitted, resid)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal()
residuals_density #cloud of data seems appropriate

#Pre-Analysis of Mediation
#Viewing Variables
hist(df_loneliness_clean$worklife_balance_num)

#Checking multicollinearity
correlation_mediation <- cor(df_loneliness_clean$gender_num, df_loneliness_clean$relationship_need_num, use = "complete.obs")
correlation_mediation #correlation < .7

model_y_mediation <- lm(loneliness_total ~ gender_num + relationship_need_num, data = df_loneliness_clean)
vif(model_y_mediation) #vif < 5
