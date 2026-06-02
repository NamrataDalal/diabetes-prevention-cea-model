# ============================================================
# Project: Cost-Effectiveness Analysis of Diabetes Prevention
#          Strategies - Indian Diabetes Prevention Programme
# Script:  01 - Model Inputs and ICER Calculations
# Author:  Namrata Dalal
# Date:    June 2026
# Reference: Ramachandran et al. (2007), Glick et al. (2007)
# ============================================================

# ---- 1. Load Libraries ----
library(tidyverse)

# ---- 2. Define Intervention Costs (2006 USD) ----
costs <- c(
  Control     = 61,
  LSM         = 225,
  MET         = 220,
  LSM_MET     = 270
)

# ---- 3. Define Probabilities of Avoiding Diabetes ----
# From Table 1 of original analysis
probs_avoiding <- data.frame(
  Intervention = c("Control", "LSM", "MET", "LSM_MET"),
  Year0 = c(1.000, 1.000, 1.000, 1.000),
  Year1 = c(0.766, 0.847, 0.841, 0.846),
  Year2 = c(0.587, 0.717, 0.707, 0.715),
  Year3 = c(0.450, 0.607, 0.595, 0.605)
)

cat("Probabilities of avoiding diabetes:\n")
print(probs_avoiding)

# ---- 4. Calculate AUC (Life Years Without Diabetes) ----
# Using trapezoid formula: AUC = 0.5*(y1+y2)*time_interval
calc_auc <- function(y0, y1, y2, y3) {
  auc1 <- 0.5 * (y0 + y1) * 1
  auc2 <- 0.5 * (y1 + y2) * 1
  auc3 <- 0.5 * (y2 + y3) * 1
  return(auc1 + auc2 + auc3)
}

auc_values <- probs_avoiding %>%
  rowwise() %>%
  mutate(
    AUC1 = 0.5 * (Year0 + Year1),
    AUC2 = 0.5 * (Year1 + Year2),
    AUC3 = 0.5 * (Year2 + Year3),
    Total_AUC = AUC1 + AUC2 + AUC3
  )

cat("\nAUC Values (Life Years Without Diabetes):\n")
print(auc_values %>% select(Intervention, AUC1, AUC2, AUC3, Total_AUC))

# ---- 5. Calculate ICERs Using Glick Method ----
icer_results <- data.frame(
  Intervention = c("Control", "MET", "LSM"),
  Total_AUC = c(2.08, 2.35, 2.37),
  Cost = c(61, 220, 225)
) %>%
  mutate(
    Incremental_Cost = Cost - 61,
    Incremental_Effect = Total_AUC - 2.08,
    ICER = Incremental_Cost / Incremental_Effect,
    Note = c("Reference",
             "Extendedly dominated by LSM",
             "Cost-effective if WTP >= $565.52")
  )

cat("\nICER Results (Glick Method):\n")
print(icer_results)

cat("\nConclusion: LSM is cost-effective if WTP >= $565.52\n")
cat("LSM+MET is dominated by LSM (higher cost, lower effectiveness)\n")
cat("MET is extendedly dominated by LSM\n")

# ---- 6. Save Inputs ----
save(costs, probs_avoiding, auc_values, icer_results,
     file = "data/model_inputs.RData")
cat("\nModel inputs saved to data/model_inputs.RData\n")