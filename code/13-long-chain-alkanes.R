# Long chain alkanes emissions by treatment and population

# load packages and data -------------------------------------------------

# load
source("code/03-load-data.R")

# explore and model "Long chain alkanes" ---------------------------------

# data exploration
# hist(vocs_type$`Long chain alkanes`)

# remove extreme outliers (> 3 SD above mean)
vocs_alkanes <- vocs_type %>%
  filter(
    `Long chain alkanes` <
      (mean(`Long chain alkanes`) + 3 * sd(`Long chain alkanes`))
  )

# GLMM of total VOCs emissions
glm_alkanes <- glmmTMB(
  `Long chain alkanes` ~ treatment * population + (1 | population:genotype),
  # removed one extreme outliers (> 3 SD above mean)
  data = vocs_alkanes,
  family = Gamma(link = "log")
)

# model diagnostics with DHARMa
# simulateResiduals(fittedModel = glm_alkanes, plot = TRUE)

# estimated marginal means (EMMs) for treatment within population
emm_alkanes <-
  emmeans(
    glm_alkanes,
    pairwise ~ treatment | population,
    adjustSigma = TRUE,
    adjust = "tukey",
    type = "response"
  )

# save EMMs to a CSV file
write.csv(
  as.data.frame(emm_alkanes$emmeans),
  "tables/emm-long-chain-alkanes-emmeans.csv",
  row.names = FALSE
)

# save pairwise contrasts to a CSV file
write.csv(
  as.data.frame(emm_alkanes$contrasts),
  "tables/emm-long-chain-alkanes-contrasts.csv",
  row.names = FALSE
)

# compact letter display (CLD) for EMMs for treatment within population
cld_alkanes <- cld(emm_alkanes, Letters = TRUE)

# p-values for pairwise comparisons
pvalue_alkanes <- summary(emm_alkanes$contrasts)$p.value

# plot -------------------------------------------------------------------

# global plot parameters
position_boxplot <- 3 # global knob for horizontal dodging; larger = more separation
transparency_boxplot <- 0.9 # alpha for all layers so overlaps are visible
justification_boxplot <- -0.26 * position_boxplot # shifts right half leftwards and left half rightwards
width_boxplot <- 0.35 # max boxplot width (as a fraction of panel width)
width_histogram <- 0.3 # max histogram width (as a fraction of panel width)
mean_size <- 2.5 # size of mean point
star_size <- 7 # size of significance stars

# plot Long chain alkanes emissions by treatment and population
p_alkanes <-
  ggplot(
    vocs_alkanes,
    aes(x = population, y = `Long chain alkanes`, fill = treatment)
  ) +

  # jittered raw points
  # - color encodes treatment (same as fill to keep legend consistent)
  # - position_jitterdodge jitters within each population and dodges between treatments
  geom_jitter(
    aes(color = treatment),
    position = position_jitterdodge(
      jitter.width = 0.075, # small horizontal jitter so points do not overlap
      dodge.width = 0.2 * position_boxplot, # align with box/halfeye offsets
      seed = 1 # reproducible jitter
    ),
    size = 0.75,
    alpha = transparency_boxplot
  ) +

  # central boxplot for a robust summary per group
  # - width is narrow so densities remain visible
  # - outliers hidden because raw points are already plotted
  geom_boxplot(
    position = position_dodge(width = 0.1 * position_boxplot),
    outlier.shape = NA,
    alpha = transparency_boxplot,
    width = 0.1
  ) +

  # distribution for control (left half)
  # - filter the data inside the layer
  # - side = "left" draws a half violin to the left of the x position
  # - justification nudges the slab so both halves mirror each other around the box
  stat_halfeye(
    data = ~ dplyr::filter(.x, treatment == "Control"),
    side = "left",
    justification = -justification_boxplot + 1, # mirrors the right half
    adjust = 0.5, # kernel bandwidth (smaller = wigglier)
    alpha = transparency_boxplot,
    width = width_histogram,
    .width = 0, # do not draw intervals
    point_colour = NA, # no point at the median
    slab_color = NA # no outline on the slab
  ) +

  # distribution for herbivore-induced (right half)
  stat_halfeye(
    data = ~ dplyr::filter(.x, treatment == "Herbivore-induced"),
    side = "right",
    justification = justification_boxplot, # mirror offset of the left half
    adjust = 0.5,
    alpha = transparency_boxplot,
    width = width_histogram,
    .width = 0,
    point_colour = NA # keep slab outline (default) for a subtle edge; set slab_color = NA to remove
  ) +

  stat_summary(
    fun = mean,
    geom = "point",
    alpha = transparency_boxplot,
    shape = 20,
    size = mean_size,
    stroke = 0.55,
    position = position_dodge(width = 0.1 * position_boxplot)
  ) +

  # padding around x-axis
  scale_x_discrete(expand = c(0, 0)) +

  # linear y scale
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +

  # zoom in to avoid outliers stretching the y axis
  coord_cartesian(
    ylim = c(0, quantile(vocs_alkanes$`Long chain alkanes`, 0.93) * 1.15)
  ) +

  # use the same two-color palette for both fill and point color
  scale_color_manual(values = pal_treat, name = "Treatment") +
  scale_fill_manual(values = pal_treat, name = "Treatment") +

  # labels and theme
  labs(
    x = "Population",
    y = expression(
      "Long chain alkanes emissions (" * ng ~
        h^{
          -1
        } *
          ")"
    )
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    # legend.background = element_rect(
    #   color = "black",
    #   linewidth = 0.5,
    #   linetype = "solid"
    # ),
    legend.position = "top"
  )

# compute a nice y position per population (just above the tallest value)
stars_alkanes <- vocs_alkanes %>%
  dplyr::distinct(population) %>%
  dplyr::mutate(
    y = quantile(vocs_alkanes$`Long chain alkanes`, c(0.925)) * 1.05,
    label = dplyr::case_when(
      pvalue_alkanes < 0.001 ~ "***",
      pvalue_alkanes < 0.01 ~ "**",
      pvalue_alkanes < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# add to your existing plot object
p_alkanes <-
  p_alkanes +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.10))) + # a bit of headroom
  geom_text(
    data = stars_alkanes,
    aes(x = population, y = y, label = label),
    inherit.aes = FALSE,
    size = star_size
  )

# save plot
f <- 0.85 # scaling factor for dimensions
ggsave(
  plot = p_alkanes,
  "figures/fig-long-chain-alkanes.png",
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)

# additional: regular boxplot --------------------------------------------

# global plot parameters
position_boxplot <- 0.6 # global knob for horizontal dodging; larger = more separation
transparency_boxplot <- 0.9 # alpha for all layers so overlaps are visible
width_boxplot <- 0.5 # max halfeye width (as a fraction of panel width)
mean_size <- 2.5 # size of mean point
star_size <- 7 # size of significance stars

# plot Long chain alkanes emissions by treatment and population
p_alkanes_boxplot <-
  ggplot(
    vocs_alkanes,
    aes(x = population, y = `Long chain alkanes`, fill = treatment)
  ) +

  # central boxplot for a robust summary per group
  geom_boxplot(
    position = position_dodge(width = position_boxplot),
    outlier.shape = NA,
    alpha = transparency_boxplot,
    width = width_boxplot
  ) +

  stat_summary(
    fun = mean,
    geom = "point",
    alpha = transparency_boxplot,
    shape = 20,
    size = mean_size,
    stroke = 0.55,
    position = position_dodge(width = position_boxplot)
  ) +

  # # log y scale
  # scale_y_log10(
  #   # breaks = scales::log_breaks(n = 15),
  #   breaks = c(2, 5, 10, 25, 50, 100, 250, 500, 1000),
  #   expand = expansion(mult = c(0, 0.05))
  # ) +

  # linear y scale
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +

  # limit y-axis to avoid extreme outliers
  coord_cartesian(
    ylim = c(0, quantile(vocs_alkanes$`Long chain alkanes`, 0.93) * 1.15)
  ) +

  # use the same two-color palette for both fill and point color
  scale_fill_manual(values = pal_treat, name = "Treatment") +

  # labels and theme
  labs(
    x = "Population",
    y = expression(
      "Long chain alkanes emissions (" * ng ~
        h^{
          -1
        } *
          ")"
    )
  ) +
  theme(
    panel.grid.major.x = element_blank(),
    # legend.background = element_rect(
    #   color = "black",
    #   linewidth = 0.5,
    #   linetype = "solid"
    # ),
    legend.position = "top"
  )

# compute a nice y position per population (just above the tallest value)
stars_alkanes <- vocs_alkanes %>%
  dplyr::distinct(population) %>%
  dplyr::mutate(
    y = quantile(vocs_alkanes$`Long chain alkanes`, c(0.925)) * 1.05,
    label = dplyr::case_when(
      pvalue_alkanes < 0.001 ~ "***",
      pvalue_alkanes < 0.01 ~ "**",
      pvalue_alkanes < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# add to your existing plot object
p_alkanes_boxplot <-
  p_alkanes_boxplot +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.10))) + # a bit of headroom
  geom_text(
    data = stars_alkanes,
    aes(x = population, y = y, label = label),
    inherit.aes = FALSE,
    size = star_size
  )

# save plot
f <- 0.85 # scaling factor for dimensions
ggsave(
  plot = p_alkanes_boxplot,
  "figures/fig-long-chain-alkanes-boxplot.png",
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
