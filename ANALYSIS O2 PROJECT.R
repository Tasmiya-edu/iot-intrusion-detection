# ============================================================
# INFO 6105 Final Project — Analysis 2: One-Way ANOVA
# Dataset: RT-IoT2022
# Response: log(flow_duration)
# Factor: proto (tcp, udp, icmp)
# ============================================================

# ------------------------------------------------------------
# CHUNK 1: Load packages
# ------------------------------------------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))

unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("ggplot2",    INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("dplyr",      INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("car",        INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("multcomp",   INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("effectsize", INSTALL_opts = "--no-lock")

library(ggplot2)
library(dplyr)
library(car)        # for Levene's test
library(multcomp)   # for Tukey HSD
library(effectsize) # for eta-squared effect size

# ------------------------------------------------------------
# CHUNK 2: Load and prepare data
 
df <- read.csv("data/RT_IOT2022.csv")
# ------------------------------------------------------------

# Filter to only tcp, udp, icmp rows
# and create log-transformed duration
df_anova <- df %>%
  filter(
    proto %in% c("tcp", "udp", "icmp"),
    is.finite(flow_duration),
    flow_duration > 0
  ) %>%
  mutate(
    log_duration = log(flow_duration),
    proto        = factor(proto)   # tell R proto is a category, not a number
  )

cat("Rows for ANOVA:", nrow(df_anova), "\n")
cat("Protocol counts:\n")
print(table(df_anova$proto))

# ------------------------------------------------------------
# CHUNK 3: PLOT 1 — Boxplot of log(flow_duration) by protocol
# Think of a boxplot like a summary of how spread out the
# data is for each group — the box shows the middle 50%,
# the line in the middle is the median
# ------------------------------------------------------------
png("plot4_anova_boxplot.png", width = 650, height = 500)
ggplot(df_anova, aes(x = proto, y = log_duration, fill = proto)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.6) +
  scale_fill_manual(values = c(
    "tcp"  = "#7F77DD",
    "udp"  = "#1D9E75",
    "icmp" = "#EF9F27"
  )) +
  labs(
    title = "Log(flow duration) by protocol type",
    x     = "Protocol type",
    y     = "log(flow duration in microseconds)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
dev.off()
cat("Saved: plot4_anova_boxplot.png\n")

# ------------------------------------------------------------
# CHUNK 4: PLOT 2 — Group means bar chart with error bars
# This shows the average log duration for each protocol
# with ± 1 standard deviation shown as error bars
# ------------------------------------------------------------
summary_stats <- df_anova %>%
  group_by(proto) %>%
  summarise(
    n            = n(),
    mean_log_dur = mean(log_duration),
    sd_log_dur   = sd(log_duration),
    .groups      = "drop"
  )

print(summary_stats)

png("plot5_anova_means.png", width = 600, height = 450)
ggplot(summary_stats, aes(x = proto, y = mean_log_dur, fill = proto)) +
  geom_col(width = 0.5, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = mean_log_dur - sd_log_dur,
        ymax = mean_log_dur + sd_log_dur),
    width = 0.2, linewidth = 0.8
  ) +
  scale_fill_manual(values = c(
    "tcp"  = "#7F77DD",
    "udp"  = "#1D9E75",
    "icmp" = "#EF9F27"
  )) +
  labs(
    title = "Mean log(flow duration) by protocol (± 1 SD)",
    x     = "Protocol type",
    y     = "Mean log(flow duration)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
dev.off()
cat("Saved: plot5_anova_means.png\n")

# ------------------------------------------------------------
# CHUNK 5: Run the One-Way ANOVA
# This tests: "are the group means all the same, or
# does at least one group differ from the others?"
# If p < 0.05 — yes, at least one group is different!
# ------------------------------------------------------------
anova_model <- aov(log_duration ~ proto, data = df_anova)
summary(anova_model)

# ------------------------------------------------------------
# CHUNK 6: Tukey HSD Post-Hoc Test
# ANOVA only tells us "someone is different"
# Tukey HSD tells us exactly WHICH groups differ
# Think of it like ANOVA saying "one of these three
# pizza places is slower" and Tukey saying
# "it's specifically Dominos vs Papa Johns that differ"
# ------------------------------------------------------------
tukey_result <- TukeyHSD(anova_model)
print(tukey_result)

# ------------------------------------------------------------
# CHUNK 7: Check Assumptions
# Before trusting ANOVA results we check two things:
# 1. Normality — are the residuals normally distributed?
 #  Shapiro-Wilk test — p > 0.05 means normal, which is good
# 2. Equal variances — do all groups have similar spread?
  # Levene's test — p > 0.05 means equal variances, which is good
# ------------------------------------------------------------

# Normality check — Shapiro-Wilk
# (using a sample of 5000 because Shapiro-Wilk
#  can't handle more than 5000 rows at once)
set.seed(42)
residuals_sample <- sample(residuals(anova_model), 5000)
shapiro_result <- shapiro.test(residuals_sample)
print(shapiro_result)

# Equal variances check — Levene's test
levene_result <- leveneTest(log_duration ~ proto, data = df_anova)
print(levene_result)

# ------------------------------------------------------------
# CHUNK 8: Effect Size — Eta Squared (η²)
# This tells us how MEANINGFUL the difference is
# not just whether it's statistically significant
# η² < 0.01 = tiny effect
# η² 0.01–0.06 = small effect
# η² 0.06–0.14 = medium effect
# η² > 0.14 = large effect
# ------------------------------------------------------------
eta_result <- eta_squared(anova_model)
print(eta_result)

# ------------------------------------------------------------
# CHUNK 9: Session info (required by rubric)
# ------------------------------------------------------------
sessionInfo()