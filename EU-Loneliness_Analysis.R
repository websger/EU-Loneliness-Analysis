#Code by Philip Weber

#Set Seed
set.seed(2025)

# Packages needed: tidyverse, mediation, psych, car, lmtest 

# Libraries
library(tidyverse)
library(mediation)
library(psych)
library(car)
library(lmtest)

# Load Data
df_loneliness <- read_csv("data_raw/eu_loneliness_survey_eu27_labels.csv")

#Cleaning Data

#Gender as numeric (Male = 0, Female = 1)
df_loneliness_clean <- df_loneliness %>%
  filter(gender %in% c("Male", "Female")) %>%
  mutate(gender_num = ifelse(gender == "Female", 1, 0))

#Drop NAs
df_loneliness_clean <- df_loneliness_clean %>%
  drop_na(gender, relationship_need, worklife_balance)

#Drop "Prefer not to say"
unique(df_loneliness_clean$relationship_need)

df_loneliness_clean <- df_loneliness_clean %>%
  filter(relationship_need != "Prefer not to say")

#relationship need as numeric - higher values means more need for relationships
df_loneliness_clean <- df_loneliness_clean %>%
  dplyr::mutate(relationship_need_num = dplyr::recode(relationship_need,
                                                      "Never" = 1,
                                                      "Every two months or less frequently" = 2,
                                                      "Once a month" = 3,
                                                      "Every two weeks" = 4,
                                                      "Every week" = 5,
                                                      "More than once a week" = 6,
                                                      "Daily" = 7))

#Drop "Prefer not to say
df_loneliness_clean <- df_loneliness_clean %>%
  filter(loneliness_djg_a != "Prefer not to say", 
         loneliness_djg_b != "Prefer not to say",
         loneliness_djg_c != "Prefer not to say", 
         loneliness_djg_d != "Prefer not to say",
         loneliness_djg_e != "Prefer not to say",
         loneliness_djg_f != "Prefer not to say")

unique(df_loneliness_clean$loneliness_djg_a)

#Recode so higher values equal more loneliness and as numeric 1 to 3
df_loneliness_clean <- df_loneliness_clean %>%
  mutate(loneliness_djg_a_num = case_when(loneliness_djg_a == "Yes" ~ 3,
                                          loneliness_djg_a == "More or less" ~ 2,
                                          loneliness_djg_a == "No" ~ 1), 
         loneliness_djg_b_num = case_when(loneliness_djg_b == "Yes" ~ 3,
                                          loneliness_djg_b == "More or less" ~ 2,
                                          loneliness_djg_b == "No" ~ 1),
         loneliness_djg_c_num = case_when(loneliness_djg_c == "Yes" ~ 3,
                                          loneliness_djg_c == "More or less" ~ 2,
                                          loneliness_djg_c == "No" ~ 1),
         loneliness_djg_d_num = case_when(loneliness_djg_d == "Yes" ~ 1,
                                          loneliness_djg_d == "More or less" ~ 2,
                                          loneliness_djg_d == "No" ~ 3),
         loneliness_djg_e_num = case_when(loneliness_djg_e == "Yes" ~ 1,
                                          loneliness_djg_e == "More or less" ~ 2,
                                          loneliness_djg_e == "No" ~ 3),
         loneliness_djg_f_num = case_when(loneliness_djg_f == "Yes" ~ 1,
                                          loneliness_djg_f == "More or less" ~ 2,
                                          loneliness_djg_f == "No" ~ 3))

#Calculate total mean of loneliness score out of all 6 questions
df_loneliness_clean <- df_loneliness_clean %>%
  dplyr::mutate(loneliness_total = rowMeans(dplyr::select(df_loneliness_clean, loneliness_djg_a_num:loneliness_djg_f_num), 
                                            na.rm = TRUE))

#Work-Life-Balance as numeric - higher values means better Work-Life-Balance
df_loneliness_clean <- df_loneliness_clean %>%
  dplyr::mutate(worklife_balance_num = dplyr::recode(worklife_balance,
                                                     "1 Do not fit at all" = 1,
                                                     "2" = 2,
                                                     "3" = 3, 
                                                     "4" = 4, 
                                                     "5" = 5,
                                                     "6" = 6,
                                                     "7" = 7,
                                                     "8" = 8,
                                                     "9" = 9,
                                                     "10 Fit extremely well" = 10))

unique(df_loneliness_clean$worklife_balance_num)

#Get rid of all unnecessary columns/variables
df_loneliness_clean <- df_loneliness_clean %>%
  dplyr::transmute(gender_num,relationship_need_num, worklife_balance_num, loneliness_total)

head(df_loneliness_clean)

#Save for further analysis
write_csv(df_loneliness_clean, "data_clean/eu_loneliness_clean.csv")

#Pre-Analysis of Regression

#Viewing Variables
table(df_loneliness_clean$gender_num)
hist(df_loneliness_clean$relationship_need_num)
ggplot(df_loneliness_clean, aes(x = loneliness_total)) +
  geom_freqpoly(binwidth = 0.5)+
  theme_minimal()

#Checking Linearity of Regression
model_regression <- lm(loneliness_total ~ worklife_balance_num, data = df_loneliness_clean)

plot_linearity <- crPlots(model_regression, pch=NA) #linearity visually appropriate
plot_linearity

#Checking normal distribution
residuals_regression <- residuals(model_regression)
histogram_residuals_regression <- hist(residuals_regression)
qqnorm_residuals_regression <- qqnorm(residuals_regression)
qqline_residuals_regression <- qqline(residuals_regression) #Normal distribution visually appropriate

#homoskedasticity
df_res <- data.frame(fitted = fitted(model_regression),
                     resid  = resid(model_regression))

residuals_jitter <- ggplot(df_res, aes(fitted, resid)) +
  geom_jitter(alpha = 0.1, width = 0.1, height = 0.1) +
  geom_hline(yintercept = 0, color = "blue")+
  theme_minimal()
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

#Analysis of Mediation

#Analysis
results_mediation <- mediation::mediate(model.m = lm(relationship_need_num ~ gender_num, data = df_loneliness_clean),
                                        model.y = lm(loneliness_total ~ gender_num + relationship_need_num, data = df_loneliness_clean),
                                        treat = "gender_num",
                                        mediator = "relationship_need_num", 
                                        sims = 1000)

#Show Results
summary(results_mediation)

#Analysis of Regression

#Analysis
results_regression <- lm(loneliness_total ~ worklife_balance_num, data = df_loneliness_clean)

#Show Results
summary(results_regression)

#Graphs and illustrations for the hypotheses

#Regression plot
plot_regression <- ggplot(df_loneliness_clean, aes(x = worklife_balance_num, y = loneliness_total)) +
  geom_jitter(alpha = 0.05, width = 0.2, height = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(limits = c(1, 3), breaks = seq(1, 3, by = 0.5)) +
  scale_x_continuous(limits = c(1, 10), breaks = seq(1, 10, by = 1)) +
  theme_minimal()

plot_regression


#Mediation plots
# path_a: gender -> relationship need
path_a_summary <- df_loneliness_clean %>%
  group_by(gender_num) %>%
  summarise(mean_rel = mean(relationship_need_num, na.rm = TRUE),
            se = sd(relationship_need_num, na.rm = TRUE) / sqrt(n()),
            ci_lower = mean_rel - 1.96 * se,
            ci_upper = mean_rel + 1.96 * se)

path_a <- ggplot(path_a_summary, aes(x = factor(gender_num, labels=c("Male", "Female")), y = mean_rel)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  labs(x = "Gender", y = "Relationship Need (Mean + 95% CI)") +
  theme_minimal()

path_a

# path_b: relationship need -> loneliness
path_b <- ggplot(df_loneliness_clean, aes(x = relationship_need_num, y = loneliness_total)) +
  geom_jitter(alpha = 0.05, width = 0.2, height = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 3), breaks = 1:3) +
  theme_minimal() +
  labs(x = "Relationship Need", y = "Loneliness")

path_b

# path_C: gender -> loneliness
path_c_summary <- df_loneliness_clean %>%
  group_by(gender_num) %>%
  summarise(mean_lon = mean(loneliness_total, na.rm = TRUE),
            se = sd(loneliness_total, na.rm = TRUE) / sqrt(n()),
            ci_lower = mean_lon - 1.96 * se,
            ci_upper = mean_lon + 1.96 * se)

path_c <- ggplot(path_c_summary, aes(x = factor(gender_num, labels=c("Male", "Female")), y = mean_lon)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +
  labs(x = "Gender", y = "Loneliness (Mean + 95% CI)") +
  theme_minimal()

path_c
