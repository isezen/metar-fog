library(caret)
# NOTE: First run "01-prepare_data.R" file.

# CONFUSION MATRICES
# -------------------
# Here, I will try to establish a 3 basic prediction models to see the results
# of confusion matrices and accuracy values of 3 models. This will help me to
# interpret how much the other models are useful for fog prediction.
# 68307 observations from 2019 to end of 2022 (3 years) will be used.

df <- readRDS("data/metar.rds")

# Define predictors

# Will predict non-fog for every day.
rabia <- function(x) rep(FALSE, length.out = length(x))

# Will predict fog for every day.
ismail <- function(x) rep(TRUE, length.out = length(x))

# Will predict tomorrow looking at today.
sukran <- function(x) {
  real <- x < 1000
  c(NA, real[1:(length(real) - 1)])
}

real <- factor(df$fog_type == "FG",
               levels = c("TRUE", "FALSE"),
               labels = c("FOGGY", "NOT-FOGGY"))

pred <- list()
pred$rabia <- factor(rabia(df$vis),
                     levels = c("TRUE", "FALSE"),
                     labels = c("FOGGY", "NOT-FOGGY"))

pred$ismail <- factor(ismail(df$vis),
                     levels = c("TRUE", "FALSE"),
                     labels = c("FOGGY", "NOT-FOGGY"))

pred$sukran <- factor(sukran(df$vis),
                      levels = c("TRUE", "FALSE"),
                      labels = c("FOGGY", "NOT-FOGGY"))

# CREATE CONFUSION MATRICES
# -------------------------
# We have 3 predictors.
# 1- Rabia always predicts always non-fog for every day.
# 2- Ismail ALWAYS predicts fog for every day.
# 3- Sukran looks today and predicts tomorrow for today. For instance,
#    if today is foggy, it will predict tomorrow as foggy.
#
# According to those properties, let's examine confusion matrices and accuracy
# values.

cf <- lapply(pred, confusionMatrix, reference = real)

# Get accuracy and Kappa values
ac <- as.data.frame(
  t(sapply(cf, function(x) c(x$overall[1:2], x$byClass[c(1:4, 7, 9, 11)]))))
# Sort the values such as best will be on top.
(ac[with(ac, order(-Kappa, -Accuracy)),])

# Model order by accuracy:
#           Accuracy     Kappa Sensitivity Specificity Pos Pred Value Neg Pred Value          F1 Detection Rate Balanced Accuracy
# sukran 0.993381944 0.4476491     0.91133   0.9936266    0.298869144      0.9997340 0.450121655    0.002708718         0.9524783
# rabia  0.997027775 0.0000000     0.00000   1.0000000            NaN      0.9970278          NA    0.000000000         0.5000000
# ismail 0.002972225 0.0000000     1.00000   0.0000000    0.002972225            NaN 0.005926834    0.002972225         0.5000000

# Best models are sukran and rabia However, ismail's accuracy
# is very low. Let's see confusion tables.
#
# Ismail      Reference
# Prediction  FOGGY NOT-FOGGY
# FOGGY         203     68096
# NOT-FOGGY       0         0
#
# Ismail will ALWAYS predict fog for every observation. Hence, He can predict
# foggy observations (203 observation) truly but cannot predict non-foggy
# observation (68096). Since there are high number of non-foggy observations,
# accuracy is low.
#
# Sukran      Reference
# Prediction  FOGGY NOT-FOGGY
# FOGGY         185       434
# NOT-FOGGY      18     67661
#
# Sukran correctly predicted 185 foggy and 67661 non-foggy observations just by
# predicting next observation by looking current observation. This is named as
# persistence method.
#
# Rabia      Reference
# Prediction  FOGGY NOT-FOGGY
# FOGGY             0       0
# NOT-FOGGY       203   68096
#
# Rabia coldn't predict any fog event because She ALWAYS predicts non-foggy and
# predicted foggy observations as non-foggy. But she has got very good points
# from non-foggy observations (68096) and this increases her accuracy value to
# 0.997.
#
# As you noticed, kappa values of rabia and ismail are zero. This means that all
# predicted values are accumluated in one class. This means that our prediction
# is not usable even it has high accuracy. We also want kappa is high like
# accuracy. Sukran's kappa value is 0.4476 and accuracy is 0.9934. This means
# that sukran's predictions are more useful than others.
#
# Interpret other confusion matrices and inaccuracies in this project
# considering the information above.

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
