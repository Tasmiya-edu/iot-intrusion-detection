# ============================================================
# INFO 6105 Final Project — Analysis 4: Bootstrap & Permutation
# Dataset: RT-IoT2022
# Statistic: Median of fwd_pkts_per_sec
# Groups: intrusion = 0 (normal) vs intrusion = 1 (attack)
# ============================================================

# ------------------------------------------------------------
# CHUNK 1: Load packages
# ------------------------------------------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))

unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("ggplot2", INSTALL_opts = "--no-lock")
unlink("C:/Users/iram3/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)
install.packages("dplyr",   INSTALL_opts = "--no-lock")

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# CHUNK 2: Prepare data
# We need fwd_pkts_per_sec split into attack vs normal groups
# df_logistic already has the intrusion variable from Analysis 3
# If starting fresh run:
df <- read.csv("data/RT_IOT2022.csv")
# then recreate df_logistic from Analysis 3 code
# ------------------------------------------------------------
df_boot <- df_logistic %>%
  filter(is.finite(fwd_pkts_per_sec)) %>%
  select(fwd_pkts_per_sec, intrusion)

attack_fwd <- df_boot$fwd_pkts_per_sec[df_boot$intrusion == 1]
normal_fwd <- df_boot$fwd_pkts_per_sec[df_boot$intrusion == 0]

cat("Attack group size:", length(attack_fwd), "\n")
cat("Normal group size:", length(normal_fwd), "\n")
cat("Median fwd_pkts_per_sec (attack):", median(attack_fwd), "\n")
cat("Median fwd_pkts_per_sec (normal):", median(normal_fwd), "\n")
cat("Observed difference in medians:",
    median(attack_fwd) - median(normal_fwd), "\n")

# ------------------------------------------------------------
# CHUNK 3: PLOT 1 — Histogram by intrusion status
# Shows how skewed fwd_pkts_per_sec is
# and how different attack vs normal look
# ------------------------------------------------------------
df_hist <- df_boot %>%
  filter(fwd_pkts_per_sec < quantile(fwd_pkts_per_sec, 0.99)) %>%
  mutate(intrusion_label = ifelse(intrusion == 1, "Attack", "Normal"))

png("plot9_bootstrap_histogram.png", width = 700, height = 450)
ggplot(df_hist, aes(x = fwd_pkts_per_sec, fill = intrusion_label)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("Attack" = "#D85A30", "Normal" = "#1D9E75")) +
  labs(
    title = "Forward packets/sec by intrusion status (showing skewness)",
    x     = "Forward packets per second",
    y     = "Count",
    fill  = "Traffic type"
  ) +
  theme_minimal(base_size = 13)
dev.off()
cat("Saved: plot9_bootstrap_histogram.png\n")

# ------------------------------------------------------------
# CHUNK 4: Bootstrap CI for the median
# We resample 1000 times from each group
# and compute the median each time
# This gives us a distribution of medians
# from which we extract a 95% confidence interval
# ------------------------------------------------------------
set.seed(42)
B <- 1000

boot_medians_attack <- replicate(B, {
  sample_data <- sample(attack_fwd, size = 500, replace = TRUE)
  median(sample_data)
})

boot_medians_normal <- replicate(B, {
  sample_data <- sample(normal_fwd, size = 500, replace = TRUE)
  median(sample_data)
})

ci_attack <- quantile(boot_medians_attack, c(0.025, 0.975))
ci_normal <- quantile(boot_medians_normal, c(0.025, 0.975))

cat("\nBootstrap Results:\n")
cat("Attack group — Median:", median(attack_fwd), "\n")
cat("Attack group — 95% CI: [", ci_attack[1], ",", ci_attack[2], "]\n")
cat("Normal group — Median:", median(normal_fwd), "\n")
cat("Normal group — 95% CI: [", ci_normal[1], ",", ci_normal[2], "]\n")

# ------------------------------------------------------------
# CHUNK 5: PLOT 2 — Bootstrap sampling distributions
# Shows the distribution of bootstrap medians
# for both attack and normal groups
# with 95% CI overlay
# ------------------------------------------------------------
boot_df <- data.frame(
  median = c(boot_medians_attack, boot_medians_normal),
  group  = rep(c("Attack", "Normal"), each = B)
)

png("plot10_bootstrap_dist.png", width = 700, height = 450)
ggplot(boot_df, aes(x = median, fill = group)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = ci_attack[1], color = "#D85A30",
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = ci_attack[2], color = "#D85A30",
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = ci_normal[1], color = "#1D9E75",
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = ci_normal[2], color = "#1D9E75",
             linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values = c("Attack" = "#D85A30", "Normal" = "#1D9E75")) +
  labs(
    title = "Bootstrap sampling distribution of median fwd packets/sec",
    subtitle = "Dashed lines = 95% confidence interval boundaries",
    x     = "Bootstrap median (fwd packets per second)",
    y     = "Frequency",
    fill  = "Traffic type"
  ) +
  theme_minimal(base_size = 13)
dev.off()
cat("Saved: plot10_bootstrap_dist.png\n")

# ------------------------------------------------------------
# CHUNK 6: Permutation Test
# We ask: "could the observed difference in medians
# happen by random chance if there was no real difference?"
# We shuffle the group labels 1000 times and see
# how often we get a difference as big as what we observed
# If p < 0.05 — the difference is real, not random!
# ------------------------------------------------------------
set.seed(42)
n_perm <- 1000

obs_diff <- median(attack_fwd) - median(normal_fwd)
cat("Observed difference in medians:", obs_diff, "\n")

all_data <- c(attack_fwd, normal_fwd)
n_attack <- length(attack_fwd)

perm_diffs <- replicate(n_perm, {
  shuffled    <- sample(all_data)
  perm_attack <- shuffled[1:n_attack]
  perm_normal <- shuffled[(n_attack + 1):length(shuffled)]
  median(perm_attack) - median(perm_normal)
})

p_value <- mean(abs(perm_diffs) >= abs(obs_diff))
cat("Permutation test p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Result: The difference is statistically significant!\n")
} else {
  cat("Result: The difference is NOT statistically significant.\n")
}

# ------------------------------------------------------------
# CHUNK 7: Parametric comparison — two sample t-test
# We run a regular t-test on the same data
# to compare with our bootstrap/permutation results
# ------------------------------------------------------------
set.seed(42)
attack_sample <- sample(attack_fwd, 500)
normal_sample <- sample(normal_fwd, 500)

t_result <- t.test(attack_sample, normal_sample)
cat("\nParametric t-test results:\n")
print(t_result)

cat("\nComparison summary:\n")
cat("Bootstrap CI attack: [", ci_attack[1], ",", ci_attack[2], "]\n")
cat("Bootstrap CI normal: [", ci_normal[1], ",", ci_normal[2], "]\n")
cat("Permutation p-value:", p_value, "\n")
cat("Parametric t-test p-value:", round(t_result$p.value, 6), "\n")
# ------------------------------------------------------------
# CHUNK 8: Effect size — observed difference in medians
# This tells us HOW BIG the difference is
# not just whether it's statistically significant
# ------------------------------------------------------------
effect_size <- median(attack_fwd) - median(normal_fwd)
cat("Effect size (difference in medians):", effect_size, "\n")
cat("Attack median:", median(attack_fwd), "\n")
cat("Normal median:", median(normal_fwd), "\n")

# ------------------------------------------------------------
# CHUNK 9: Session info (required by rubric)
# ------------------------------------------------------------
sessionInfo()