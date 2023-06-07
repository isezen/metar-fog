library(ggplot2)
library(dplyr)
library(caret)
# NOTE: First run "01-prepare_data.R" file.

# Here, I will try to establish a logistic regression model between predictors
# and fog events. 68307 observations from 2019 to end of 2022
# (3 years).

df <- readRDS("data/metar.rds") # read data
# Binomial representation of real fog events
df$fog <- ifelse(df$fog_type == "FG", 1, 0)
# df$fog <- ifelse(df$is_foggy, 1, 0)

# BINOMIAL LOGISTIC REGRESSION
# -----------------------------
# Logistic regression is used to predict the class (or category) of
# individuals based on one or multiple predictor variables. Following is the
# best binomial logistic regression model that I could accomplish with the
# predictors (spread, wind speed, wind direction and month). Also log of spread
# and wind speed are used as predictors.

real <- factor(df$fog,
               levels = c(1, 0),
               labels = c("FOGGY", "NOT-FOGGY"))
fit <- glm(fog ~ log10(spread + 1) + log10(ws + 1) + wd + month,
           data = df, family = binomial)
pred <- factor(ifelse(fit$fitted.values > 0.5, 1, 0),
               levels = c(1, 0),
               labels = c("FOGGY", "NOT-FOGGY"))
(confusionMatrix(pred, real))

# Confusion matrix shows that ~15% of the fog events are determined correctly
# and 29 observations are determined as foggy even if there is not a fog. This
# model is worse than 'vis_spread_0' linear regression model studied in
# 'reg_linear.R' file.

#
# To prepare data:
# SEE: "01-prepare_data.R"
#
# To understand what is confusion matrix,
# SEE: "confusion_matrix.R"
#
# To have an idea of overview of the data,
# SEE: "03-eda.R"
#
# If you want to see why Linear model is not suitable for prediction,
# SEE: "04-reg_linear.R"
#
# If you want to see results of binomial logistic regression and compare
# with linear regression:
# SEE: '05-reg_logistic.R'
#
# If you want to have an idea how CART models behave for this data set,
# SEE: "06-CART.R"
#
