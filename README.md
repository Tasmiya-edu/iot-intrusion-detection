# IoT Network Intrusion Detection — Statistical Analysis in R

A statistical analysis of the RT-IoT2022 dataset applying four 
methods to detect and characterize cyberattack traffic in IoT 
network flows.

**Course:** INFO 6105 — Statistical Learning | Northeastern University | Spring 2026

---

## Project Overview

This project investigates which network flow characteristics best 
predict and distinguish attack traffic from normal IoT behavior 
using interpretable statistical models, no deep learning required.

**Dataset:** RT-IoT2022 (UCI Machine Learning Repository)  
123,117 network flow observations | 84 features | 9 attack types  
Source: https://doi.org/10.24432/C5P338

---

## Key Results

| Analysis | Result |
|---|---|
| Logistic Regression | AUC = 0.9937, F1 = 98.19% |
| Attack Detection Sensitivity | 98.34% (threshold = 0.3) |
| Bootstrap Effect Size | Attack traffic sends ~27,000x more packets/sec than normal |
| ANOVA Effect Size | η² = 0.15 — protocol type explains 15% of flow duration variance |
| MLR Best Predictor | down_up_ratio (β = 2.199, p < 0.001) |

---

## Four Analyses

**1. Multiple Linear Regression**
- Predicts average payload size from flow-level features
- Detected extreme multicollinearity (VIF > 200,000) between
  packet rate variables — resolved by removing redundant predictor
- Final model R² = 0.29, F = 1,360.8, p < 2.2e-16

**2. One-Way ANOVA**
- Tests whether flow duration differs across tcp, udp, icmp protocols
- Result: F(2, 107133) = 9,448, p < 2e-16, η² = 0.15
- Tukey HSD confirmed all pairwise differences significant
- Duration ordering: icmp > udp > tcp

**3. Logistic Regression**
- Binary classification: attack vs normal traffic
- Handled 88% class imbalance through threshold tuning
- Optimal threshold 0.3 maximizes F1 while prioritizing sensitivity
- Log payload size strongest predictor (OR = 3.18)

**4. Bootstrap and Permutation Testing**
- Compares median forward packet rates between attack/normal traffic
- Used bootstrap due to extreme right skew violating parametric
  assumptions
- Permutation p = 0, difference in medians = 246,714 packets/sec

---

## Repository Structure

```
iot-intrusion-detection/
│
├── README.md
├── scripts/
│   └── analysis.R          # Complete R code (all 4 analyses)
├── reports/
│   └── final_report.Rmd    # R Markdown source
└── data/
    └── README.md           # Dataset download instructions
```

---

## How to Run

1. Download the RT-IoT2022 dataset from UCI:
   https://doi.org/10.24432/C5P338

2. Update the file path in analysis.R:
```r
   df <- read.csv("data/RT_IOT2022.csv")
```

3. Install required packages:
```r
   install.packages(c("ggplot2", "dplyr", "car", "GGally",
   "reshape2", "broom", "multcomp", "effectsize", "pROC", "caret"))
```

4. Run analysis.R in RStudio or source section by section

---

## Tools & Environment

- **Language:** R 4.5.2
- **IDE:** RStudio
- **Key packages:** ggplot2, dplyr, car, pROC, caret, effectsize

---

## Key Finding

A logistic regression model using only four flow-level features 
achieves near-perfect IoT intrusion detection (AUC = 0.9937), 
demonstrating that interpretable statistical models can match deep 
learning performance while remaining deployable on 
resource-constrained IoT devices.
