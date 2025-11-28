#Graphs and illustrations for the hypotheses
# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(mediation)
library(psych)
library(car)
library(readr)
library(lmtest)

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

path_a <- ggplot(path_a_summary, aes(x = factor(gender_num), y = mean_rel)) +
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
