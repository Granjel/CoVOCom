# Models for each VOC type
#
# for each VOC type:
# 1) fit GLM/GLMM variants,
# 2) select the best model by AIC,
# 3) test covariates on the chosen structure,
# 4) compute EMMs and contrasts.
#
# load packages and data -------------------------------------------------
source("code/03-load-data.R")

# get unique VOC types to iterate over
voc_types <- vocs_info %>%
  dplyr::pull(type) %>%
  unique()

# fit models once per VOC type, then compute EMMs and contrasts ----------
results <- purrr::map(voc_types, function(voc_type) {
  # subset to the current VOC type and drop missing responses
  vocs_df <- vocs_type %>%
    dplyr::select(
      code,
      population,
      genotype,
      treatment,
      n,
      larva_emitter,
      size_emitter,
      voc_id,
      compound,
      !!rlang::sym(voc_type)
    ) %>%
    tidyr::drop_na(!!rlang::sym(voc_type))

  # base formulas
  base_formula <- paste0("`", voc_type, "` ~ treatment * population")
  glmm_formula <- paste0(base_formula, " + (1 | population:genotype)")

  # compare GLM vs GLMM
  glm_model <- glmmTMB(
    stats::as.formula(base_formula),
    data = vocs_df,
    family = tweedie(link = "log")
  )

  glmm_model <- glmmTMB(
    stats::as.formula(glmm_formula),
    data = vocs_df,
    family = tweedie(link = "log")
  )

  aic_random <- AIC(glm_model, glmm_model)
  use_random <- which.min(aic_random$AIC) == 2

  base_model <- if (use_random) glmm_model else glm_model
  cov_base <- if (use_random) glmm_formula else base_formula

  # test covariates on the chosen structure
  cov_models <- list(
    base_model,
    glmmTMB(
      stats::as.formula(paste0(cov_base, " + larva_emitter")),
      data = vocs_df,
      family = tweedie(link = "log")
    ),
    glmmTMB(
      stats::as.formula(paste0(cov_base, " + size_emitter")),
      data = vocs_df,
      family = tweedie(link = "log")
    ),
    glmmTMB(
      stats::as.formula(paste0(cov_base, " + larva_emitter + size_emitter")),
      data = vocs_df,
      family = tweedie(link = "log")
    )
  )

  # select the best covariate model by AIC
  aic_covariates <- AIC(
    cov_models[[1]],
    cov_models[[2]],
    cov_models[[3]],
    cov_models[[4]]
  )
  best_model <- cov_models[[which.min(aic_covariates$AIC)]]

  # estimated marginal means and contrasts
  emms <- emmeans(
    best_model,
    pairwise ~ treatment | population,
    adjust = "tukey",
    type = "response"
  )

  list(
    emmeans = as.data.frame(emms$emmeans) %>%
      dplyr::mutate(voc_type = voc_type, .before = 1),
    contrasts = as.data.frame(emms$contrasts) %>%
      dplyr::mutate(voc_type = voc_type, .before = 1)
  )
})

# combine EMMs and contrasts across VOC types
emm_all <- purrr::map_dfr(results, "emmeans")
contrasts_all <- purrr::map_dfr(results, "contrasts")

# save EMMs and contrasts to CSV files -----------------------------------
write.csv(
  emm_all,
  "tables/emms/emm-voc-types-emmeans.csv",
  row.names = FALSE
)

write.csv(
  contrasts_all,
  "tables/emms/emm-voc-types-contrasts.csv",
  row.names = FALSE
)

# plot -------------------------------------------------------------------

# ensure factor ordering if needed
emm_all <- emm_all %>%
  dplyr::mutate(
    treatment = factor(treatment, levels = c("Control", "Herbivore-induced"))
  )

stars <- contrasts_all %>%
  dplyr::mutate(
    label = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  dplyr::group_by(voc_type, population) %>%
  dplyr::summarise(
    label = dplyr::first(label),
    .groups = "drop"
  )

y_pos <- emm_all %>%
  dplyr::group_by(voc_type, population) %>%
  dplyr::summarise(
    y = max(asymp.UCL) * 1.1,
    .groups = "drop"
  )

stars <- dplyr::left_join(stars, y_pos, by = c("voc_type", "population"))

p_emm <- ggplot(
  emm_all,
  aes(
    x = population,
    y = response,
    color = treatment,
    group = treatment
  )
) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 2
  ) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    position = position_dodge(width = 0.5),
    width = 0.15
  ) +
  geom_text(
    data = stars,
    aes(x = population, y = y, label = label),
    inherit.aes = FALSE,
    size = 4
  ) +
  facet_wrap(
    ~voc_type,
    nrow = 1,
    labeller = ggplot2::labeller(
      voc_type = function(x) stringr::str_wrap(x, width = 12)
    )
  ) +
  scale_color_manual(values = pal_treat, name = "Treatment") +
  labs(
    x = "Population",
    y = expression(
      "Emissions (" * ng ~ h^{
        -1
      } *
        ")"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", margin = margin(t = 5, b = 12))
  )

# save the plot
ggsave(
  "figures/voc-types-emmeans.jpeg",
  width = 6.5,
  height = 3.5
)
