# ============================================================
# Project: Cost-Effectiveness Analysis - IDPP
# Script:  03 - Probabilistic Sensitivity Analysis (PSA)
#               Monte Carlo Simulation, CEAC/CEAF
# Author:  Namrata Dalal
# Date:    June 2026
# ============================================================

# ---- 1. Load Libraries ----
library(tidyverse)

# ---- 2. Load Model Inputs ----
load("data/model_inputs.RData")

# ---- 3. Define PSA Parameters ----
# Beta distribution parameters for effectiveness (from Table S8)
# Alpha and Beta for probability of avoiding diabetes at Year 3
beta_params <- data.frame(
  Intervention = c("Control", "LSM", "MET", "LSM_MET"),
  alpha = c(2.4992, 1.6373, 1.7564, 1.6607),
  beta  = c(2.0448, 2.5290, 2.5804, 2.5436),
  mean_cost = c(61, 225, 220, 270)
)

# ---- 4. Run Monte Carlo Simulation ----
set.seed(42)
n_sim <- 10000

results <- map_dfr(1:n_sim, function(i) {
  
  # Sample effectiveness from Beta distributions
  p_control <- rbeta(1, 2.4992, 2.0448)
  p_lsm     <- rbeta(1, 1.6373, 2.5290)
  p_met     <- rbeta(1, 1.7564, 2.5804)
  p_lsm_met <- rbeta(1, 1.6607, 2.5436)
  
  # Sample costs from Gamma distributions
  c_control <- rgamma(1, shape = 1, rate = 1/61)
  c_lsm     <- rgamma(1, shape = 1, rate = 1/225)
  c_met     <- rgamma(1, shape = 1, rate = 1/220)
  c_lsm_met <- rgamma(1, shape = 1, rate = 1/270)
  
  data.frame(
    sim = i,
    intervention = c("Control", "LSM", "MET", "LSM_MET"),
    effectiveness = c(p_control, p_lsm, p_met, p_lsm_met),
    cost = c(c_control, c_lsm, c_met, c_lsm_met)
  )
})

cat("PSA complete:", n_sim, "simulations run\n")

# ---- 5. Calculate NMB at Each WTP ----
wtp_values <- seq(0, 3000, by = 50)

nmb_results <- map_dfr(wtp_values, function(wtp) {
  results %>%
    mutate(NMB = effectiveness * wtp - cost) %>%
    group_by(sim) %>%
    mutate(is_optimal = NMB == max(NMB)) %>%
    group_by(intervention) %>%
    summarise(
      prob_cost_effective = mean(is_optimal),
      mean_NMB = mean(NMB),
      .groups = "drop"
    ) %>%
    mutate(WTP = wtp)
})

cat("NMB calculations complete\n")

# ---- 6. Plot CEAC ----
ceac_colors <- c("Control"  = "#666666",
                 "LSM"      = "#2166ac",
                 "MET"      = "#d6604d",
                 "LSM_MET"  = "#4dac26")

plot_ceac <- ggplot(nmb_results,
                    aes(x = WTP, y = prob_cost_effective,
                        color = intervention)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = ceac_colors,
                     labels = c("Control", "LSM", "LSM+MET", "MET")) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  geom_vline(xintercept = 565.52, linetype = "dashed",
             color = "darkblue", linewidth = 0.8) +
  annotate("text", x = 650, y = 0.85,
           label = "WTP = $565.52", color = "darkblue", size = 3.5) +
  labs(
    title = "Cost-Effectiveness Acceptability Curves (CEAC)",
    subtitle = "Probability each intervention is cost-effective across WTP thresholds",
    x = "Willingness-to-Pay Threshold (USD per LY without diabetes)",
    y = "Probability Cost-Effective",
    color = "Intervention",
    caption = "Based on 10,000 Monte Carlo simulations"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("outputs/plot3_ceac.png", plot_ceac,
       width = 10, height = 6, dpi = 300)
cat("Plot 3 saved: CEAC\n")

# ---- 7. Plot CEAF ----
ceaf_data <- nmb_results %>%
  group_by(WTP) %>%
  filter(prob_cost_effective == max(prob_cost_effective)) %>%
  slice(1) %>%
  ungroup()

plot_ceaf <- ggplot(ceaf_data,
                    aes(x = WTP, y = prob_cost_effective,
                        color = intervention)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 1.5) +
  scale_color_manual(values = ceac_colors,
                     labels = c("Control", "LSM", "LSM+MET", "MET")) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  geom_vline(xintercept = 565.52, linetype = "dashed",
             color = "darkblue", linewidth = 0.8) +
  labs(
    title = "Cost-Effectiveness Acceptability Frontier (CEAF)",
    subtitle = "Optimal intervention at each WTP threshold",
    x = "Willingness-to-Pay Threshold (USD per LY without diabetes)",
    y = "Probability Cost-Effective",
    color = "Intervention",
    caption = "Based on 10,000 Monte Carlo simulations"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("outputs/plot4_ceaf.png", plot_ceaf,
       width = 10, height = 6, dpi = 300)
cat("Plot 4 saved: CEAF\n")

# ---- 8. Save PSA Results ----
save(results, nmb_results, file = "data/psa_results.RData")
cat("PSA results saved\n")