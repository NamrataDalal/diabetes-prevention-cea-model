# ============================================================
# Project: Cost-Effectiveness Analysis - IDPP
# Script:  02 - Survival Curves and CE Plane Visualizations
# Author:  Namrata Dalal
# Date:    June 2026
# ============================================================

# ---- 1. Load Libraries ----
library(tidyverse)

# ---- 2. Load Model Inputs ----
load("data/model_inputs.RData")

# ---- 3. Survival Curves ----
survival_data <- data.frame(
  Year = rep(0:3, 4),
  Intervention = rep(c("Control", "LSM", "MET", "LSM+MET"), each = 4),
  Prob_Avoiding = c(
    1, 0.766, 0.587, 0.450,   # Control
    1, 0.847, 0.717, 0.607,   # LSM
    1, 0.841, 0.707, 0.595,   # MET
    1, 0.846, 0.715, 0.605    # LSM+MET
  )
)

colors <- c("Control"  = "#666666",
            "LSM"      = "#2166ac",
            "MET"      = "#d6604d",
            "LSM+MET"  = "#4dac26")

plot1 <- ggplot(survival_data,
                aes(x = Year, y = Prob_Avoiding,
                    color = Intervention, group = Intervention)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = 0:3) +
  scale_y_continuous(limits = c(0, 1),
                     labels = scales::percent) +
  labs(
    title = "Survival Curves — Probability of Avoiding Diabetes",
    subtitle = "Indian Diabetes Prevention Programme (IDPP), 3-Year Trial",
    x = "Year",
    y = "Probability of Avoiding Diabetes",
    color = "Intervention",
    caption = "Source: Ramachandran et al. (2007)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("outputs/plot1_survival_curves.png", plot1,
       width = 9, height = 6, dpi = 300)
cat("Plot 1 saved: Survival curves\n")

# ---- 4. Cost-Effectiveness Plane ----
ce_data <- data.frame(
  Intervention = c("Control", "LSM", "MET", "LSM+MET"),
  Total_AUC    = c(2.08, 2.37, 2.35, 2.36),
  Cost         = c(61, 225, 220, 270)
) %>%
  mutate(
    Inc_Effect = Total_AUC - 2.08,
    Inc_Cost   = Cost - 61,
    Dominated  = Intervention %in% c("LSM_MET", "LSM+MET", "MET")
  )

plot2 <- ggplot(ce_data,
                aes(x = Inc_Effect, y = Inc_Cost, color = Intervention)) +
  geom_point(aes(shape = Dominated), size = 4) +
  geom_text(aes(label = Intervention),
            vjust = -0.8, hjust = 0.5, size = 4, fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # WTP threshold line
  geom_abline(slope = 565.52, intercept = 0,
              linetype = "dotted", color = "darkblue", linewidth = 1) +
  annotate("text", x = 0.25, y = 155,
           label = "WTP = $565.52", color = "darkblue", size = 3.5) +
  scale_color_manual(values = colors) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 4),
                     labels = c("Not dominated", "Dominated")) +
  labs(
    title = "Cost-Effectiveness Plane",
    subtitle = "Incremental costs and effects vs. Control group",
    x = "Incremental Life Years Without Diabetes",
    y = "Incremental Cost (2006 USD)",
    color = "Intervention",
    shape = "Dominance",
    caption = "Source: Ramachandran et al. (2007); Glick et al. (2007)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("outputs/plot2_ce_plane.png", plot2,
       width = 9, height = 6, dpi = 300)
cat("Plot 2 saved: CE Plane\n")

# ---- 5. ICER Summary Table ----
icer_summary <- data.frame(
  Intervention = c("Control", "MET", "LSM+MET", "LSM"),
  Total_Cost   = c("$61", "$220", "$270", "$225"),
  Life_Years_Without_Diabetes = c(2.08, 2.35, 2.36, 2.37),
  ICER = c("Reference",
           "$588.89 (Extendedly dominated)",
           "Dominated by LSM",
           "$565.52 — COST EFFECTIVE")
)

cat("\nFinal ICER Summary Table:\n")
print(icer_summary)

write_csv(icer_summary, "outputs/icer_summary_table.csv")
cat("ICER summary table saved\n")