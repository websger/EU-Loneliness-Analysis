# Install Packages
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("mediation")
install.packages("psych")
install.packages("car")
install.packages("readr")

# Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(mediation)
library(psych)
library(car)
library(readr)

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

#Speichern für weitere Analyse
write_csv(df_loneliness_clean, "data_clean/eu_loneliness_clean.csv")
