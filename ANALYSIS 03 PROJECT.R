# ============================================================
# INFO 6105 Final Project — Analysis 3: Logistic Regression
# Dataset: RT-IoT2022
# Response: intrusion (1 = attack, 0 = normal)
# Predictors: log(flow_duration), fwd_pkts_per_sec,
#             bwd_pkts_per_sec, log(flow_pkts_payload.avg)
# ============================================================

# ------------------------------------------------------------
# CHUNK 1: Install and load packages
# ------------------------------------------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))

unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("ggplot2", INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("dplyr", INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("pROC", INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("caret", INSTALL_opts = "--no-lock")

library(ggplot2)
library(dplyr)
library(pROC)    # for ROC curve and AUC
library(caret)   # for confusion matrix

# ------------------------------------------------------------
# CHUNK 2: Prepare data
# We create the binary intrusion variable:
# 1 = attack traffic, 0 = normal traffic
# ------------------------------------------------------------

# If df is not loaded, run this first:
df <- read.csv("data/RT_IOT2022.csv")

df_logistic <- df %>%
  filter(
    is.finite(flow_duration)         & flow_duration > 0,
    is.finite(fwd_pkts_per_sec),
    is.finite(bwd_pkts_per_sec),
    is.finite(flow_pkts_payload.avg) & flow_pkts_payload.avg > 0
  ) %>%
  mutate(
    intrusion    = ifelse(Attack_type %in% c("MQTT_Publish",
                                             "Thing_Speak",
                                             "Wipro_bulb"), 0, 1),
    log_duration = log(flow_duration),
    log_payload  = log(flow_pkts_payload.avg)
  )

cat("Total rows:", nrow(df_logistic), "\n")
cat("Class distribution:\n")
print(table(df_logistic$intrusion))
cat("Normal %:", round((1 - mean(df_logistic$intrusion)) * 100, 2), "%\n")
cat("Attack %:", round(mean(df_logistic$intrusion) * 100, 2), "%\n")

print(table(df$Attack_type))

# ------------------------------------------------------------
# CHUNK 3: PLOT 1 — Intrusion rate by protocol type
# This shows what % of connections are attacks
# for each protocol — tcp, udp, icmp
# ------------------------------------------------------------
df_proto_plot <- df_logistic %>%
  filter(proto %in% c("tcp", "udp", "icmp")) %>%
  group_by(proto) %>%
  summarise(intrusion_rate = mean(intrusion), .groups = "drop")

png("plot6_intrusion_rate.png", width = 600, height = 450)
ggplot(df_proto_plot, aes(x = proto, y = intrusion_rate, fill = proto)) +
  geom_col(width = 0.5, alpha = 0.85) +
  scale_fill_manual(values = c(
    "tcp"  = "#7F77DD",
    "udp"  = "#1D9E75",
    "icmp" = "#EF9F27"
  )) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Intrusion rate by protocol type",
    x     = "Protocol type",
    y     = "Proportion of attack traffic"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
dev.off()
cat("Saved: plot6_intrusion_rate.png\n")

# ------------------------------------------------------------
# CHUNK 4: PLOT 2 — Density of fwd_pkts_per_sec
# by intrusion status
# This shows how attack and normal traffic differ
# in terms of forward packet rate
# ------------------------------------------------------------
df_density <- df_logistic %>%
  filter(fwd_pkts_per_sec < quantile(fwd_pkts_per_sec, 0.99)) %>%
  mutate(intrusion_label = ifelse(intrusion == 1, "Attack", "Normal"))

png("plot7_density.png", width = 650, height = 450)
ggplot(df_density, aes(x = fwd_pkts_per_sec, fill = intrusion_label)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Attack" = "#D85A30", "Normal" = "#1D9E75")) +
  labs(
    title = "Forward packets/sec by intrusion status",
    x     = "Forward packets per second",
    y     = "Density",
    fill  = "Traffic type"
  ) +
  theme_minimal(base_size = 13)
dev.off()
cat("Saved: plot7_density.png\n")

# ------------------------------------------------------------
# CHUNK 5: Split data into training and test sets
# Think of it like studying with 70% of the material
# and then testing yourself on the other 30%
# 70% for training — model learns from this
# 30% for testing — we check how well it learned
# ------------------------------------------------------------
set.seed(42)
n           <- nrow(df_logistic)
train_idx   <- sample(1:n, size = 0.7 * n)
df_train    <- df_logistic[train_idx, ]
df_test     <- df_logistic[-train_idx, ]

cat("Training rows:", nrow(df_train), "\n")
cat("Testing rows:", nrow(df_test), "\n")

# ------------------------------------------------------------
# CHUNK 6: Fit the Logistic Regression model
# glm = generalized linear model
# family = binomial means we're predicting 0 or 1
# ------------------------------------------------------------
logit_model <- glm(
  intrusion ~ log_duration + fwd_pkts_per_sec +
    bwd_pkts_per_sec + log_payload,
  data   = df_train,
  family = binomial
)

summary(logit_model)

# ------------------------------------------------------------
# CHUNK 7: Odds Ratios
# Odds ratio tells us how much MORE likely an attack is
# for each unit increase in a predictor
# OR > 1 = increases attack probability
# OR < 1 = decreases attack probability
# OR = 1 = no effect
# ------------------------------------------------------------
odds_ratios <- exp(coef(logit_model))
conf_int    <- exp(coef(logit_model) + 
                     outer(sqrt(diag(vcov(logit_model))), c(-1.96, 1.96)))
colnames(conf_int) <- c("Lower 95%", "Upper 95%")

cat("\nOdds Ratios with 95% Confidence Intervals:\n")
print(cbind(OR = round(odds_ratios, 4), round(conf_int, 4)))

# ------------------------------------------------------------
# CHUNK 8: Make predictions on test data
# Default threshold = 0.5
# meaning: if predicted probability > 0.5 → predict attack
# ------------------------------------------------------------
pred_probs  <- predict(logit_model, newdata = df_test, type = "response")
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

cat("Prediction distribution:\n")
print(table(pred_labels))

# ------------------------------------------------------------
# CHUNK 9: Confusion Matrix
# This is like a report card for our model
# showing how many attacks it correctly caught
# and how many it missed
# ------------------------------------------------------------
conf_matrix <- confusionMatrix(
  factor(pred_labels, levels = c(0, 1)),
  factor(df_test$intrusion, levels = c(0, 1)),
  positive = "1"
)
print(conf_matrix)

cat("\nKey metrics:\n")
cat("Accuracy:   ", round(conf_matrix$overall["Accuracy"], 4), "\n")
cat("Sensitivity:", round(conf_matrix$byClass["Sensitivity"], 4), "\n")
cat("Specificity:", round(conf_matrix$byClass["Specificity"], 4), "\n")
cat("Precision:  ", round(conf_matrix$byClass["Precision"], 4), "\n")
cat("F1 Score:   ", round(conf_matrix$byClass["F1"], 4), "\n")

# ------------------------------------------------------------
# CHUNK 10: ROC Curve and AUC
# ROC curve shows the tradeoff between catching attacks
# (sensitivity) and avoiding false alarms (specificity)
# AUC = area under the curve
# AUC = 0.5 means random guessing (bad)
# AUC = 1.0 means perfect model (great)
# AUC > 0.85 is considered very good
# ------------------------------------------------------------
roc_obj <- roc(df_test$intrusion, pred_probs)
auc_val <- auc(roc_obj)
cat("\nAUC:", round(auc_val, 4), "\n")

png("plot8_roc_curve.png", width = 600, height = 550)
plot(roc_obj,
     col  = "#7F77DD",
     lwd  = 2,
     main = paste("ROC Curve — AUC =", round(auc_val, 4)),
     print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()
cat("Saved: plot8_roc_curve.png\n")

# ------------------------------------------------------------
# CHUNK 11: Threshold tuning
# Instead of 0.5, we try different thresholds
# to improve recall (catching more attacks)
# This is important because missing an attack
# is much worse than a false alarm in cybersecurity
# ------------------------------------------------------------
thresholds  <- seq(0.1, 0.9, by = 0.1)
results     <- data.frame()

for (thresh in thresholds) {
  preds <- ifelse(pred_probs > thresh, 1, 0)
  tp    <- sum(preds == 1 & df_test$intrusion == 1)
  fp    <- sum(preds == 1 & df_test$intrusion == 0)
  tn    <- sum(preds == 0 & df_test$intrusion == 0)
  fn    <- sum(preds == 0 & df_test$intrusion == 1)
  
  accuracy    <- (tp + tn) / (tp + fp + tn + fn)
  sensitivity <- ifelse((tp + fn) > 0, tp / (tp + fn), NA)
  specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), NA)
  f1          <- ifelse((tp + fp + fn) > 0,
                        2 * tp / (2 * tp + fp + fn), NA)
  
  results <- rbind(results, data.frame(
    threshold   = thresh,
    accuracy    = round(accuracy, 4),
    sensitivity = round(sensitivity, 4),
    specificity = round(specificity, 4),
    f1_score    = round(f1, 4)
  ))
}

cat("\nThreshold tuning results:\n")
print(results)

# ------------------------------------------------------------
# CHUNK 12: Session info (required by rubric)
# ------------------------------------------------------------
sessionInfo()