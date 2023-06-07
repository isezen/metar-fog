# CLASSIFICATION and REGRESSION TREES (CART)
#
library(caret)
library(rpart)
library(partykit)
library(rattle)
library(ggparty)
# NOTE: First run "01-prepare_data.R" file.

df <- readRDS("data/metar.rds")
df$real <- factor(df$is_foggy,
                  levels = c("TRUE", "FALSE"),
                  labels = c("FOGGY", "NOT-FOGGY"))
df$ws2 <- ordered(round(df$ws, 1))
df$spread2 <- ordered(df$spread)
df2 <- df[df$vis < 3000,]
df3 <- df2[df2$is_foggy,]

# Just simple rpart implementation
fit.rp <- rpart(real ~ spread2 + ws2 + pres + season, data = df)
pred <- predict(fit.rp, newdata = df, type = "class")
(cf <- confusionMatrix(pred, df$real))
plot(as.party(fit.rp), type = "extended")
fancyRpartPlot(fit.rp)

# Conditional Inference Tree
fit.rp2 <- ctree(real ~ spread2 + pres + ws2 + season, data = df,
                 control = ctree_control(alpha = 0.001, maxdepth = 4))
pred <- predict(fit.rp2, newdata = df)
(cf <- confusionMatrix(pred, df$real))
plot(fit.rp2, type = "extended")
autoplot(fit.rp2)

# lmtree
fit.rp4 <- lmtree(vis ~ spread2 + pres + ws2 + wd + season,
                  data = df2, maxdepth = 4) # (0.23)
fit.rp4 <- lmtree(vis ~ spread + 0 | month,
                  data = df2, maxdepth = 4) # (0.23)
summary(fit.rp4)
pred <- factor(predict(fit.rp4, newdata = df) < 1000,
               levels = c("TRUE", "FALSE"),
               labels = c("FOGGY", "NOT-FOGGY"))
(cf <- confusionMatrix(pred, df$real))
plot(fit.rp4)
autoplot(fit.rp4)

fit.rp4$data$fog_type <- df2$fog_type
ggparty(fit.rp4, terminal_space = 0.5) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(
    geom_point(aes(x = spread2, y = vis, col = fog_type), alpha = 0.8),
    theme_bw(base_size = 8)),
    shared_axis_labels = FALSE,
    # graphical parameters for geom_line of predictions
    predict_gpar = list(col = "blue", size = 1.2),
    height = 0.5)

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
