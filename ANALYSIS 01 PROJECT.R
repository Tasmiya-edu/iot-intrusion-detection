# ============================================================
# INFO 6105 Final Project — Analysis 1: Multiple Linear Regression
# Dataset: RT-IoT2022
# Response: log(flow_pkts_payload.avg)
# Predictors: fwd_pkts_per_sec, bwd_pkts_per_sec,
#             flow_duration, down_up_ratio
# ============================================================

# ------------------------------------------------------------
# CHUNK 1: Install and load packages
# Think of packages like apps on your phone —
# you install them once, then load them every time you use R
# ------------------------------------------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))

unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("ggplot2",  INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("dplyr",    INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("car",      INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("GGally",   INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("reshape2", INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("broom",    INSTALL_opts = "--no-lock")

library(ggplot2)   # for making beautiful plots
library(dplyr)     # for cleaning and organizing data
library(car)       # for VIF (multicollinearity check)
library(GGally)    # for scatterplot matrix
library(reshape2)  # for correlation heatmap
library(broom)     # for tidy regression output
# ------------------------------------------------------------
# CHUNK 2: Load the dataset
# We tell R exactly where your file lives on your computer
# ------------------------------------------------------------

df <- read.csv("data/RT_IOT2022.csv")

cat("Number of rows:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n")
head(df, 3)

 

# ------------------------------------------------------------
# CHUNK 3: Clean the data
# We remove rows with missing, infinite, or zero values
# in the columns we care about — like throwing out
# broken crayons before you start coloring
# ------------------------------------------------------------
df_clean <- df %>%
  filter(
    is.finite(flow_pkts_payload.avg) & flow_pkts_payload.avg > 0,
    is.finite(fwd_pkts_per_sec),
    is.finite(bwd_pkts_per_sec),
    is.finite(flow_duration)        & flow_duration > 0,
    is.finite(down_up_ratio)
  )

cat("Rows after cleaning:", nrow(df_clean), "\n")

set.seed(42)
df_sample <- df_clean %>% sample_n(10000)

# Take a random sample of 10,000 rows for faster computation
# (the full dataset is 123K rows — too big for quick plots)
 

# ------------------------------------------------------------
# CHUNK 4: Create log-transformed variables
# Why log? Because our data is heavily skewed —
# like if most kids have $1 but one kid has $1,000,000.
# Log makes it more balanced and easier to analyze.
# ------------------------------------------------------------
df_sample <- df_sample %>%
  mutate(
    log_payload  = log(flow_pkts_payload.avg),
    log_duration = log(flow_duration)
  )

summary(df_sample[, c("log_payload", "fwd_pkts_per_sec",
                      "bwd_pkts_per_sec", "log_duration", "down_up_ratio")])

# ------------------------------------------------------------
# CHUNK 5: PLOT 1 — Scatterplot Matrix
# This shows how every predictor relates to every other variable
# Think of it like a grid of mini scatterplots
# ------------------------------------------------------------
plot1_data <- df_sample %>%
  select(log_payload, fwd_pkts_per_sec,
         bwd_pkts_per_sec, log_duration, down_up_ratio)

png("plot1_scattermatrix.png", width = 850, height = 750)
ggpairs(
  plot1_data,
  title = "Scatterplot matrix: predictors vs log(payload avg)",
  upper = list(continuous = wrap("cor", size = 3.5)),
  lower = list(continuous = wrap("points", alpha = 0.15, size = 0.4)),
  diag  = list(continuous = wrap("densityDiag", fill = "#7F77DD", alpha = 0.5))
) +
  theme_minimal(base_size = 11)
dev.off()
cat("Saved: plot1_scattermatrix.png\n")


getwd()
# ------------------------------------------------------------
# CHUNK 6: PLOT 2 — Correlation Heatmap
# This is a colored grid showing how strongly
# each variable is related to every other variable.
# Dark red = strong positive relationship
# Dark blue = strong negative relationship
# White = no relationship
# ------------------------------------------------------------
cor_matrix <- cor(plot1_data, use = "complete.obs")
cor_melted  <- melt(cor_matrix)

png("plot2_correlation_heatmap.png", width = 650, height = 550)
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), size = 3.5) +
  scale_fill_gradient2(
    low = "#3B8BD4", mid = "white", high = "#D85A30",
    midpoint = 0, limits = c(-1, 1), name = "Correlation"
  ) +
  labs(
    title = "Correlation heatmap of regression variables",
    x = "", y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()
cat("Saved: plot2_correlation_heatmap.png\n")

# ------------------------------------------------------------
# CHUNK 7: Fit the Multiple Linear Regression model
# This is the main event! We're asking R:
# "Using these 4 clues, predict log(payload size)"
# The ~ means "is predicted by"
# ------------------------------------------------------------
model <- lm(log_payload ~ fwd_pkts_per_sec + bwd_pkts_per_sec +
              log_duration + down_up_ratio,
            data = df_sample)

summary(model)
# Tidy version — easier to read
tidy(model)

# ------------------------------------------------------------
# CHUNK 8: Check for Multicollinearity using VIF
# VIF = Variance Inflation Factor
# Think of it like checking if two clues are saying
# the exact same thing — if they are, one of them is useless.
# VIF < 5 = good
# VIF 5-10 = warning
# VIF > 10 = problem, drop that variable
# ------------------------------------------------------------
model2 <- lm(log_payload ~ fwd_pkts_per_sec + 
               log_duration + down_up_ratio,
             data = df_sample)

summary(model2)
vif(model2)
# If any VIF > 5, we'll note it in the report
# and consider removing that predictor
summary(model2)

# ------------------------------------------------------------
# CHUNK 9: Diagnostic Plots
# These 3 plots check whether our model is "well-behaved"
# The rubric requires at least 3 diagnostic plots
#
# Plot 1: Residuals vs Fitted
#   — checks if errors are random (good) or have a pattern (bad)
#   — like checking if your guesses are randomly wrong
#     or always wrong in the same direction
#
# Plot 2: Q-Q Plot
#   — checks if errors follow a normal distribution
#   — dots should follow the diagonal line
#
# Plot 3: Scale-Location Plot
#   — checks if errors are equally spread (homoscedasticity)
#   — dots should be randomly scattered, not fan-shaped
# ------------------------------------------------------------
png("plot3_diagnostics.png", width = 900, height = 700)
par(mfrow = c(2, 2))
plot(model2)
dev.off()
cat("Saved: plot3_diagnostics.png\n")

# ------------------------------------------------------------
# CHUNK 10: Extract key numbers for your report
# ------------------------------------------------------------

# R-squared: how much of the variation our model explains
# Think of it like: "what % of the time does our guess make sense?"
r_squared     <- summary(model2)$r.squared
adj_r_squared <- summary(model2)$adj.r.squared
cat("R-squared:", round(r_squared, 4), "\n")
cat("Adjusted R-squared:", round(adj_r_squared, 4), "\n")

f_stat <- summary(model2)$fstatistic
cat("F-statistic:", round(f_stat[1], 2), "\n")
cat("p-value:", pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE), "\n")
# ------------------------------------------------------------
# CHUNK 11: Session info (required by rubric)
# ------------------------------------------------------------
sessionInfo()