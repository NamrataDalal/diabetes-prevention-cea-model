# ============================================================
# Project: Cost-Effectiveness Analysis - IDPP
# Script:  04 - EVPI and Net Loss Curves
# Author:  Namrata Dalal
# Date:    June 2026
# ============================================================

# ---- 1. Load Libraries ----
library(tidyverse)

# ---- 2. Load PSA Results ----
load("data/psa_results.RData")

# ---- 3. Calculate EVPI ----
wtp_values <- seq(0, 3000, by = 50)

evpi_results <- map_dfr(wtp_values, function(wtp) {
  
  # NMB for each intervention in each simulation
  nmb_sim <- results %>%
    mutate(NMB = effectiveness * wtp - cost)
  
  # Perfect information: always pick best intervention
  evpi_perfect <- nmb_sim %>%
    group_by(sim) %>%
    summarise(max_NMB = max(NMB), .groups = "drop") %>%
    summarise(mean_perfect = mean(max_NMB))
  
  # Current decision: pick intervention with highest average NMB
  evpi_current <- nmb_sim %>%
    group_by(intervention) %>%
    summarise(mean_NMB = mean(NMB), .groups = "drop") %>%
    summarise(max_mean = max(mean_NMB))
  
  data.frame(
    WTP  = wtp,
    EVPI = evpi_perfect$mean_perfect - evpi_current$max_mean
  )
})

cat("EVPI calculations complete\n")

# ---- 4. Plot EVPI ----
plot_evpi <- ggplot(evpi_results,
                    aes(x = WTP, y = EVPI)) +
  geom_line(linewidth = 1.2, color = "#2166ac") +
  geom_vline(xintercept = 565.52, linetype = "dashed",
             color = "darkred", linewidth = 0.8) +
  annotate("text", x = 650, y = max(evpi_results$EVPI) * 0.9,
           label = "WTP = $565.52", color = "darkred", size = 3.5) +
  labs(
    title = "Expected Value of Perfect Information (EVPI)",
    subtitle = "Maximum value of eliminating decision uncertainty, per patient",
    x = "Willingness-to-Pay Threshold (USD per LY without diabetes)",
    y = "EVPI (USD per patient)",
    caption = "Based on 10,000 Monte Carlo simulations"
  ) +
  theme_minimal(base_size = 13)

ggsave("outputs/plot5_evpi.png", plot_evpi,
       width = 10, height = 6, dpi = 300)
cat("Plot 5 saved: EVPI\n")

# ---- 5. Calculate Net Loss ----
netloss_results <- map_dfr(wtp_values, function(wtp) {
  
  nmb_sim <- results %>%
    mutate(NMB = effectiveness * wtp - cost)
  
  # Average NMB per intervention
  avg_nmb <- nmb_sim %>%
    group_by(intervention) %>%
    summarise(mean_NMB = mean(NMB), .groups = "drop")
  
  # Max average NMB (optimal treatment)
  max_avg_nmb <- max(avg_nmb$mean_NMB)
  
  # Net loss = max average NMB - average NMB for each intervention
  avg_nmb %>%
    mutate(
      Net_Loss = max_avg_nmb - mean_NMB,
      WTP = wtp
    )
})

cat("Net Loss calculations complete\n")

# ---- 6. Plot Net Loss Curves ----
nl_colors <- c("Control"  = "#666666",
               "LSM"      = "#2166ac",
               "MET"      = "#d6604d",
               "LSM_MET"  = "#4dac26")

plot_netloss <- ggplot(netloss_results,
                       aes(x = WTP, y = Net_Loss,
                           color = intervention)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = nl_colors,
                     labels = c("Control", "LSM", "LSM+MET", "MET")) +
  geom_vline(xintercept = 565.52, linetype = "dashed",
             color = "darkblue", linewidth = 0.8) +
  annotate("text", x = 650, y = max(netloss_results$Net_Loss) * 0.85,
           label = "WTP = $565.52", color = "darkblue", size = 3.5) +
  labs(
    title = "Net Loss Curves",
    subtitle = "Opportunity cost of selecting each intervention vs optimal choice",
    x = "Willingness-to-Pay Threshold (USD per LY without diabetes)",
    y = "Net Loss (USD per patient)",
    color = "Intervention",
    caption = "Based on 10,000 Monte Carlo simulations"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("outputs/plot6_netloss.png", plot_netloss,
       width = 10, height = 6, dpi = 300)
cat("Plot 6 saved: Net Loss curves\n")

# ---- 7. Save Final Results ----
save(evpi_results, netloss_results, file = "data/evpi_netloss.RData")
cat("All results saved\n")
cat("\n--- PROJECT COMPLETE ---\n")
cat("6 publication-quality plots generated\n")
cat("LSM confirmed as cost-effective intervention at WTP >= $565.52\n")