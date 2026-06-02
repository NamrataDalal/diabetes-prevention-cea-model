# Cost-Effectiveness Analysis of Diabetes Prevention Strategies
## Indian Diabetes Prevention Programme (IDPP) Reanalysis

## Overview
This project replicates and extends the cost-effectiveness analysis of the Indian Diabetes Prevention Programme (Ramachandran et al., 2007), comparing four interventions for preventing type 2 diabetes among individuals with impaired glucose tolerance (IGT). The original analysis contained methodological errors in ICER calculation which this project corrects using Glick's systematic method.

## Interventions Compared
- Control (standard healthcare advice)
- Lifestyle Modification (LSM)
- Metformin (MET)
- LSM + Metformin combined

## Key Findings
- LSM is the cost-effective intervention at WTP >= $565.52 per life year without diabetes
- LSM+MET is dominated by LSM (higher cost, lower effectiveness)
- MET is extendedly dominated by LSM
- PSA with 10,000 Monte Carlo simulations confirmed LSM as optimal from WTP $300 onwards
- EVPI analysis suggests further research is warranted to reduce decision uncertainty

## Methods
- ICER calculation using Glick systematic method
- Survival curve construction and AUC calculation for life years without diabetes
- Probabilistic Sensitivity Analysis using Beta distributions for effectiveness and Gamma distributions for costs
- Cost-Effectiveness Acceptability Curves and Frontier (CEAC/CEAF)
- Expected Value of Perfect Information (EVPI)
- Net Loss Curves

## Repository Structure
- scripts/01_model_inputs.R — Model parameters, AUC calculation, ICER tables
- scripts/02_visualizations.R — Survival curves and CE plane
- scripts/03_psa_analysis.R — Monte Carlo PSA, CEAC, CEAR summary table
- data/ — Model inputs and simulation results

## Tools and Skills Demonstrated
- R with tidyverse and ggplot2
- Health economic decision modeling
- Markov-style state transition analysis
- Probabilistic sensitivity analysis with Monte Carlo simulation
- ICER, NMB, CEAC, CEAF, EVPI calculations
- Reproducible research workflow with GitHub

## Reference
Ramachandran A, et al. Cost-effectiveness of the interventions in the primary prevention of diabetes among Asian Indians. Diabetes Care. 2007;30(10):2548-2552.
Glick H. Economic Evaluation in Clinical Trials. Oxford University Press; 2007.

## Author
Namrata Dalal
MS Pharmaceutical Economics and Policy
