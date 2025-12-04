Analysis Protocol:
Prior to analysis, the dataset will be cleaned and prepared. Cases with missing values on relevant
variables will be handled via listwise deletion. Multi-item scales will be reverse-coded where
necessary to ensure consistent directionality, and scale reliability may be checked using
Cronbach’s alpha. Descriptive statistics will be computed for all study variables, including age
(range, mean, median, standard deviation), and frequencies for categorical variables such as
gender and work-life balance (WLB). For the mediation analysis, only participants identifying as
male or female will be included; non-binary or other responses will be excluded. Outliers
exceeding ±3 standard deviations from the mean will be inspected and addressed if necessary.
For the mediation analysis, gender will serve as the predictor, relationship_need as the mediator,
and loneliness as the outcome. For the linear regression analysis, WLB will be the predictor and
loneliness the outcome. Before conducting analyses, assumptions will be checked, including
linearity between predictors and outcomes, approximate normality of residuals, and potential
multicollinearity between predictor and mediator for the mediation model. Homoskedasticity will
be checked for the regression but is not critical for the mediation analysis using bootstrapped
indirect eﬀects.
All analyses will be conducted in R, using the packages mediation, dplyr, ggplot2, psych, car, and
tidyr.
