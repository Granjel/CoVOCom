# load packages ----------------------------------------------------------

# load or install pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# load required packages by category
pacman::p_load(
  tidyverse, # includes ggplot2, dplyr, tidyr, readr, etc.
  ggpubr, # publication-ready plots
  ggrepel, # non-overlapping text labels
  lme4, # linear mixed models
  lmerTest, # p-values for lmer
  glmmTMB, # generalized linear mixed models
  DHARMa, # model residual diagnostics
  lsmeans, # estimated marginal means (consider replacing with emmeans)
  multcomp, # simultaneous inference
  multcompView, # compact letter display
  car, # Anova(), VIF(), etc.
  vegan, # ordination, PERMANOVA
  permute, # control for permutation schemes
  lattice, # base plotting system for some model outputs
  reshape # legacy reshape tools (check if still needed)
)
