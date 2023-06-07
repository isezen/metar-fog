library(xts)
library(dplyr)
library(lubridate)
library(ggplot2)
# NOTE: First run "01-prepare_data.R" file.

# 68307 observations from 2019 to end of 2022 (3 years).
df <- readRDS("data/metar.rds")

# OVERVIEW
# -------------------------
# Number of METAR observation in 3 years is 68307.
# Mean of temperature, DPT (Dew Point Temperature), and spread
# (difference between temperature and DPT) are 15.5C, 10.2C and 5.3C,
# respectively. Average wind speed is 6 m/sec. Mean MSLP
# (mean sea level pressure) is 1016 hPa. Mean visibility is 9540m.
summary(df)

# 684 (~1%) of observations are foggy.
df_foggy <- df[df$is_foggy,] # 684
(100 * nrow(df_foggy)/nrow(df)) # ~1.00%

# There are 105 observations which visibility is below 1000m
# but without fog because of snowy conditions.
df_not_foggy <- df[!df$is_foggy & df$vis < 1000,]
(nrow(df_not_foggy)) # 105

# Number of full foggy observations is 515. This is 0.75% of total
# observations and 75.3% of foggy observations.
df_fog_under_1000 <- df_foggy[df_foggy$vis < 1000,]
(100 * nrow(df_fog_under_1000)/nrow(df))  # ~0.75%
(100 * nrow(df_fog_under_1000)/nrow(df_foggy)) # ~75.3%

# 276 (53.6%) and 203 (39.4%) out of 515 fully foggy observations which
# visibility is below 1000m are BCFG and FG, respectively.
(100 * sum(df_fog_under_1000$fog_type == "BCFG")/nrow(df_fog_under_1000))
(100 * sum(df_fog_under_1000$fog_type == "FG")/nrow(df_fog_under_1000))

# 169 (24.7%) out of 684 foggy observations are above 1000m and are originated
# from BCFG or PRFG.
df_fog_above_1000 <- df_foggy[df_foggy$vis >= 1000,]
(100 * nrow(df_fog_above_1000)/nrow(df_foggy))

# FOGGY OBSERVATIONS
# -------------------------
# Mean and median of visibility in foggy conditions are 850.9m and 600m,
# respectively. Means of temperature, dew point temp. (DPT) and spread
# (Difference of temperature and DPT) are 10.44C, 10.02C and 0.42C,
# respectively.
summary(df_foggy)

# Mean wind speed is 3.8 m/sec and prevailing wind direction is North
# (between 330 and 030 degrees in 49.1% of foggy events). Also 4.5% of
# events occurred under CALM conditions.
100 * table(df_foggy$wd)/nrow(df_foggy)

# If visibility is lower than 1000m,
# mean and median of visibility is 533.7m and 500m, respectively.
# Means of temperature, DPT and spread are 10.4C, 10.0C and 0.41C,
# respectively.
summary(df_fog_under_1000)

# Mean wind speed is 3.6 m/sec and prevailing wind direction is North
# (between 330 and 030 degrees in 46% of fog events). Also 4.3% of events
# occurred under CALM conditions.
100 * table(df_fog_under_1000$wd)/nrow(df_fog_under_1000)

# Total Number of visibility levels is 62 and levels start from 100 to 10000
# and visibility had been reported subjectively rounding to nearest integer
# number as much as possible. That's why, we created a 'vislev' parameter by
# cutting visibility into 3 parts where low is between 0-999, medium is
# between 1000-4999 and high is between 5000-Inf.
vis_levels <- sort(unique(df$vis)) # 62 level of visibility

# 96.9%, 2.6% and 0.5% of events are under good, medium and bad visibility
# conditions,respectively. However, total number of fog events below 1000m was
# 249. This means that visibility is below 1000m without fog for some
# observations. Investigation shows that visibility is under 1000m for
# snowy conditions.
#      low  medium   high
#      620    1776  65903
#    0.91%   2.60% 96.49%
(100 * table(df$vislev)/nrow(df))

# Analysis of monthly FOGGY days
# ------------------------------
# The most foggy month is Dec, 2020 with a 7 days of fog.
# There is not any fog in September for 3 years period.
# Less foggy months are July, August and September.
# Most foggy seasons are Winter and spring.
#
x <- xts(df$is_foggy, order.by = df$date)
colnames(x) <- c("is_foggy")
df2 <- apply.daily(x, function(y) {
  any(y$is_foggy)
})

df3 <- apply.monthly(df2, sum)
date <- index(df3)
date$mday <- 15
date <- as.Date(date)
df3 <- data.frame(
  date = date, year = factor(year(date)),
  month = factor(month.abb[month(date)], levels = month.abb),
  nFog = as.numeric(df3[,1]))

ggplot(df3, aes(x = date, y = nFog, fill = year)) +
  geom_bar(stat = "identity") +
  ggplot2::labs(x = "Month", y = "Number of Foggy days") +
  scale_x_date(
    breaks = df3$date + 15,
    date_breaks = "1 month",
    labels = label_date_short(format = c('%y', "%b", "%d", "%H:%M"), sep = " "),
    expand = c(0.005, 0.005)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 90, vjust = 2.0, hjust = 1.0))

df4 <- as.data.frame(df3 %>%
                       group_by(month = month) %>%
                       summarise(nFog = mean(nFog)))
df4 <- rbind(df4[9:12,], df4[1:8,])
df4$month <- relevel(df4$month, "Dec")
df4$month <- relevel(df4$month, "Nov")
df4$month <- relevel(df4$month, "Oct")
df4$month <- relevel(df4$month, "Sep")

ggplot(df4, aes(x = month, y = nFog)) +
  geom_bar(stat = "identity") +
  ggplot2::labs(x = "Month", y = "Average number of Foggy days") +
  theme_bw()


# So, low visibility happens in a very low percent of time.
# Question is, what causes this? My parameters are temp,
# DPT, spread, wind speed/direction and MSLP; and I want to
# predict the Fog

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
