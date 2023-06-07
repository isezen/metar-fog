library(caret)
library(dplyr)
library(ggplot2)
# NOTE: First run "01-prepare_data.R" file.

myplot <- function(f, xvar) {
  coef <- coefficients(f)
  if (missing(xvar)) {
    xvar <- if (length(fit$vis_pres_0$terms[[3]]) == 1)
      fit$vis_pres_0$terms[[3]]
    else
      f$terms[[3]][[2]]
  }
  xvar <- as.character(xvar)
  x_labels <- c('spread' = 'Spread (C)', 'pres' = 'Pressure (hPa',
                'ws' = 'Wind Speed (m/sec)')
  xlab <- x_labels[xvar]
  slope <- coef[xvar]
  intercept <- if (is.na(coef['(Intercept)'])) 0 else coef['(Intercept)']
  df <- f$data
  if (is.null(df)) {
    df <- get(f$call$data)
  }
  yvar <- as.character(f$call$formula[[2]])
  ggplot(df %>% arrange(desc(fog_type)),
         aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[['fog_type']],
             shape = .data[['fog_type']])) +
    geom_abline(intercept = intercept, slope = slope, color = "red",
                linetype = "dashed", linewidth = 1, alpha = 0.5) +
    geom_point(alpha = 0.8) +
    labs(x = xlab, y = 'Visibility (m)') +
    theme_bw()
}

# Here, I will try to establish a linear model between predictors
# and visibility and try to predict FOG event. 68307 observations from 2019
# to end of 2022 (3 years).

df <- readRDS("data/metar.rds") # load data

df_fog <- df[df$is_foggy,] # Just foggy reports
# There is an mistaken observation at vis = 9999.
# I will remove it.
df_fog <- df_fog[df_fog$vis < 9000,]

model <- list(
  'vis_spread_0' = list('formula' = vis ~ spread + 0, 'data' = df),
  'vis_spread_ws_0' = list('formula' = vis ~ spread + ws + 0, 'data' = df),
  'vis_spread_wd' = list('formula' = vis ~ spread + wd, 'data' = df),
  'vis_spread_month_0' = list('formula' = vis ~ spread + month + 0, 'data' = df),
  'vis_spread_pres_ws_0' = list('formula' = vis ~ spread + pres + ws + 0,
                                'data' = df),
  'vis_spread_pres_ws_wd_0' = list('formula' = vis ~ spread + pres + ws + wd + 0,
                                   'data' = df),
  'vis_spread_ws_wd_0' = list('formula' = vis ~ spread + ws + wd + 0, 'data' = df),
  'vis_pres_0' = list('formula' = vis ~ pres + 0, 'data' = df),
  'vis_ws_0' = list('formula' = vis ~ ws + 0, 'data' = df),

  'vis_spread_0_foggy' = list('formula' = vis ~ spread + 0, 'data' = df_fog),
  'vis_spread_ws_0_foggy' = list('formula' = vis ~ spread + ws + 0, 'data' = df_fog),
  'vis_spread_wd_0_foggy' = list('formula' = vis ~ spread + wd + 0, 'data' = df_fog),
  'vis_pres_0_foggy' = list('formula' = vis ~ pres + 0, 'data' = df_fog),
  'vis_ws_0_foggy' = list('formula' = vis ~ ws + 0, 'data' = df_fog),
  'vis_pres_ws_0_foggy' = list('formula' = vis ~ pres + ws + 0, 'data' = df_fog)
)
# Run all models
fit <- lapply(model, function(x) {
  f <- lm(x$formula, x$data)
  f$call$formula <- x$formula
  f$data <- x$data
  f
})

# SUMMARY - WHY CAN'T I USE LINEAR MODEL?
# -----------------------------
# It is not a good idea to use a linear model to predict visibility since
# visibility is reported subjectively and rounding to a nearest integer by
# observer. Also, METAR coding has strict rules how to report visibility i.e.
# with 100m intervals below 1000m or with 1000m intervals above 5000m. These
# biases originated from METAR rules or subjective observing results in
# accumulation of values at specific numbers such as at 4000, 6000, 7000,
# 8000 and 9999 meters as shown peaks in the figure below.

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

plot(density(df$vis))

# RELATIONSHIP BETWEEN VIS and SPREAD
# -------------------------------------
# The effect of rounding and METAR coding rules for visibility and spread also
# can be seen in the following figure (i.e. wider spacing above 5000m). Also,
# If spread approaches zero, visibility tends to decrease; however, there are
# higher visibility values greater than 1000m near zero spread. Zero spread can
# also happen under rainy or snowy conditions. Because of this, A linear model
# with a non-zero intercept, regression line do not cross the area below 1000m
# and cannot predict fog. That's why, we need to force the intercept to be
# zero at the expense of false predictions for low spread but rainy/snowy
# conditions where visibility is relatively high. The following figure suggests
# that visibility tends to decrease approximately below 9.0C spread. Linear
# model adjusted R-squared value is 0.72 which means that the model can explain
# the 72% of the variability. To emphasize again, This model make false
# predictions where actually spread is near zero but visibility is high such as
# rainy/snowy conditions.

# visibility vs. spread for all data (R^2 = 0.72)
summary(fit$vis_spread_0) # vis = 1292 * spread
myplot(fit$vis_spread_0)

# Let's try to predict visibility by spread in only foggy conditions
# by linear model. There are 683 foggy observations. Just for foggy conditions,
# (including BCFG and/or FG) slope coefficient of spread and total p-value of
# the model is not significant. And also adjusted R-squared is very close to
# zero. If we force the intercept to be zero, p-value of spread coefficient and
# model is significant but adjusted R-squared value is still very low (0.19).

# visibility vs. spread for foggy conditions (R^2 = 0.19)
summary(fit$vis_spread_0_foggy) # vis = 596 * spread
myplot(fit$vis_spread_0_foggy)

# Those inappropriate models above are mainly originated from heteroscedasticity
# and not normally distribution of the values around regression line which also
# means error terms are not normally distributed. We will see the confusion
# matrix results of the models at the end of this section.


# RELATIONSHIP BETWEEN VIS and PRES
# -------------------------------------
# There is not any accountable relationship between pressure and visibility as
# seen in the figure below although p-value is significant. Adjusted R-squared
# value shows that error values are not high but this is originated from high
# visibility values in the data set. This model is far from detecting any fog
# event. That's why, vis-pres relation will not be taken into account.

# visibility vs. pressure for all Data (R^2 = 0.97)
summary(fit$vis_pres_0) # vis = 9.4 * pres
myplot(fit$vis_pres_0)

# RELATIONSHIP BETWEEN VIS and WIND SPEED
# -------------------------------------
# Most interesting thing from the figure below is that we see fog events even
# at high wind speeds (greater than 5 m/sec). We physically expect that fog
# event for calm conditions, however, with a non-zero intercept for all values
# of visibility and wind speed, adjusted R-squared is almost zero and
# regression line never pass through the area below 1000m visibility. That's why,
# we need to force the intercept to be zero. This makes adjusted R-squared value
# 0.78 with a significant wind speed coefficient. But this time, model cannot
# predict fog events when spread is greater than ~2C. I wonder, should we
# establish a linear model only using foggy events at the expense of false
# predictions for low wind speed but high visibility? See next paragraph.

# visibility vs. Wind Speed for all Data (R^2 = 0.78)
summary(fit$vis_ws_0) # vis = 1274.26 * ws
myplot(fit$vis_ws_0)

# Linear model using only foggy events.
# There are 683 foggy observations. This model looks like better than
# previous linear model which uses all data. Because, regression line passes
# through foggy events (red markers) and it is obvious that it will catch up
# true positive events.

# visibility vs. Wind Speed for foggy conditions (R^2 = 0.46)
summary(fit$vis_ws_0_foggy) # vis = 159 * ws
myplot(fit$vis_ws_0_foggy)

# MULTIPLE LINEAR REGRESSION
# ---------------------
# For multiple linear regression (MLR), coefficients of spread and pressure are
# not significant. Also model p-value is not significant with a zero adjusted
# R-squared value. Even if we tried to force the intercept to be zero,
# coefficient of spread is still zero and it is useless. That's why, we can
# remove spread variable from the model.

# Spread is not significant (R^2 = 0.61)
vis_spread_pres_ws_0 <- lm(vis ~ spread + pres + ws + 0, df_fog)
summary(vis_spread_pres_ws_0) # vis = 59 * spread + 0.699 * pres + 26.7 * ws

# Remove spread and re-establish model (R^2: 0.61)
summary(fit$vis_pres_ws_0_foggy) # vis = 0.73 * pres + 26.2 * ws

# Remove pressure and re-establish model (R^2: 0.49)
summary(fit$vis_spread_ws_0_foggy) # vis = 253.5 * spread + 140.2 * ws

# For all data (R^2: 0.98).
summary(fit$vis_spread_pres_ws_0)

# For all data and for each wind direction (R^2: 0.98).
summary(fit$vis_spread_pres_ws_wd_0)

# After removing of spread, adjusted R-squared value is still 0.60 and p-value
# of the model is very close to zero. However, plot of visibility vs. pressure
# shows that very low relationship between pressure and visibility even
# coefficient of pressure is significant. This was studied in the section
# "RELATIONSHIP BETWEEN VIS and PRES".

# CONFUSION MATRICES
# -------------------
# We will create confusion matrices and accuracy/kappa scores for each model
# above. We have 7 models and we will use them to predict fog for all data
# (68307 observation). Real data is obtained from reprted fog events and
# predictions are calculated if predicted visibility is lower than 1000m.

# real <- factor(df$fog_type == "FG",
#                levels = c("TRUE", "FALSE"),
#                labels = c("FOGGY", "NOT-FOGGY"))

real <- factor(df$is_foggy,
               levels = c("TRUE", "FALSE"),
               labels = c("FOGGY", "NOT-FOGGY"))

# And we create confusion matrices for each model.
cf <- lapply(fit, function(x) {
  pred <- factor(predict(x, df) < 1000,
                 levels = c("TRUE", "FALSE"),
                 labels = c("FOGGY", "NOT-FOGGY"))
  confusionMatrix(pred, real)
})

# Get accuracy and Kappa values
ac <- as.data.frame(
  t(sapply(cf, function(x) c(x$overall[1:2], x$byClass[c(1:4, 7, 9, 11)]))))

# Sort the values such as best will be on top.
(ac[with(ac, order(-Kappa, -Accuracy)),])

#                           Accuracy       Kappa Sensitivity Specificity Pos Pred Value Neg Pred Value         F1 Detection Rate Balanced Accuracy
# vis_spread_0            0.98420182 0.441367493  0.64181287   0.9876655     0.34485467      0.9963447 0.44864589   0.0064276197         0.8147392
# vis_spread_0_foggy      0.90440563 0.151818877  0.95906433   0.9038527     0.09165852      0.9995420 0.16732560   0.0096048258         0.9314585
# vis_spread_ws_0_foggy   0.90696789 0.136632996  0.83479532   0.9076980     0.08382267      0.9981622 0.15234792   0.0083602981         0.8712467
# vis_spread_ws_0         0.98945812 0.101792430  0.06140351   0.9988464     0.35000000      0.9905836 0.10447761   0.0006149431         0.5301250
# vis_ws_0                0.98696906 0.071360924  0.05409357   0.9964061     0.13214286      0.9904880 0.07676349   0.0005417356         0.5252498
# vis_spread_wd_0_foggy   0.70308496 0.044312542  0.99415205   0.7001405     0.03245049      0.9999155 0.06284948   0.0099562219         0.8471463
# vis_ws_0_foggy          0.40480827 0.008440690  0.85526316   0.4002514     0.01422077      0.9963552 0.02797638   0.0085652791         0.6277573
# vis_pres_ws_0_foggy     0.11210999 0.001525121  0.96491228   0.1034830     0.01077059      0.9965817 0.02130338   0.0096633919         0.5341976
# vis_spread_wd_0         0.98998521 0.000000000  0.00000000   1.0000000            NaN      0.9899852         NA   0.0000000000         0.5000000
# vis_spread_month_0      0.98998521 0.000000000  0.00000000   1.0000000            NaN      0.9899852         NA   0.0000000000         0.5000000
# vis_spread_pres_ws_0    0.98998521 0.000000000  0.00000000   1.0000000            NaN      0.9899852         NA   0.0000000000         0.5000000
# vis_spread_pres_ws_wd_0 0.98998521 0.000000000  0.00000000   1.0000000            NaN      0.9899852         NA   0.0000000000         0.5000000
# vis_spread_ws_wd_0      0.98998521 0.000000000  0.00000000   1.0000000            NaN      0.9899852         NA   0.0000000000         0.5000000
# vis_pres_0              0.98998521 0.000000000  0.00000000   1.0000000            NaN      0.9899852         NA   0.0000000000         0.5000000
# vis_pres_0_foggy        0.01001479 0.000000000  1.00000000   0.0000000     0.01001479            NaN 0.01983097   0.0100147879         0.5000000

# The models are sorted from best to worst. vis_spread_0 and vis_spread_0_foggy
# look like that have high performance to predict foggy events (positive
# events) than others. This shows that spread is a better predictor
# for fog events. The model vis_ws_0_foggy looks like that it has high
# sensitivity by predicting 585 of fog events. However, It has also False
# Negative prediction rate for non-foggy observations. This makes it a
# poor model to take into account. According to the analysis above, It might
# be a good idea to use CART models instead of pure linear regression models.

# Confusion Matrix : vis_spread_0
#             Reference
# Prediction  FOGGY NOT-FOGGY
# FOGGY       439       834
# NOT-FOGGY   245     66781

# Confusion Matrix : vis_spread_0_foggy
#             Reference
# Prediction  FOGGY NOT-FOGGY
# FOGGY       656      6501
# NOT-FOGGY    28     61114

# Confusion Matrix : vis_spread_ws_0_foggy
#             Reference
# Prediction  FOGGY NOT-FOGGY
# FOGGY       571      6241
# NOT-FOGGY   113     61374

# SEE: 'reg_logistic.R' file to see a binomial logistic regression model
# compared with linear regression results.

