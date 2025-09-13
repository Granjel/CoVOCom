# damage on receivers

# load packages and data -------------------------------------------------

# load packages
source("code/03-load-data.R")

# select only relevant data
receiver_damage <- df %>%
  dplyr::select(
    # select relevant columns
    code,
    population,
    genotype,
    treatment,
    n,
    larva_receiver,
    size_receiver,
    herbivory_receiver
  )

# explore data and fit model ---------------------------------------------

# data exploration
# hist(receiver_damage$herbivory_receiver) # untransformed data
# hist(sqrt(receiver_damage$herbivory_receiver)) # square root transformation

# LM of damage on receivers (without random effect)
lm_receiver_damage <- lm(
  sqrt(herbivory_receiver) ~ treatment * population,
  data = receiver_damage
)

# LMM of damage on receivers (random intercept for genotype nested within population)
lmm_receiver_damage <- lmer(
  sqrt(herbivory_receiver) ~ treatment * population + (1 | population:genotype),
  data = receiver_damage
)

# compare LMM to LM with likelihood ratio test (LRT)
# anova(lmm_receiver_damage, lm_receiver_damage) # LMM is better
rm(lm_receiver_damage) # remove LM to avoid confusion

# model diagnostics with DHARMa
# simulateResiduals(fittedModel = glmm_receiver_damage, plot = TRUE)

# estimated marginal means (EMMs) for treatment within population
emm_receiver_damage <-
  emmeans(
    lmm_receiver_damage,
    pairwise ~ treatment | population,
    adjustSigma = TRUE,
    adjust = "tukey",
    type = "response"
  ) # significant differences between treatments within populations

# save EMMs to a CSV file
write.csv(
  as.data.frame(emm_receiver_damage$emmeans),
  "tables/emm-receiver-damage-emmeans.csv",
  row.names = FALSE
)

# save pairwise contrasts to a CSV file
write.csv(
  as.data.frame(emm_receiver_damage$contrasts),
  "tables/emm-receiver-damage-contrasts.csv",
  row.names = FALSE
)

# compact letter display (CLD) for EMMs for treatment within population
cld_receiver_damage <- cld(emm_receiver_damage, Letters = TRUE)

# p-values for pairwise comparisons
pvalue_receiver_damage <- summary(emm_receiver_damage$contrasts)$p.value

# plot -------------------------------------------------------------------

# global plot parameters
position_boxplot <- 3 # global knob for horizontal dodging; larger = more separation
transparency_boxplot <- 1 # alpha for all layers so overlaps are visible
justification_boxplot <- -0.2 * position_boxplot # shifts right half leftwards and left half rightwards
width_boxplot <- 0.35 # max halfeye width (as a fraction of panel width)
mean_size <- 2.5 # size of mean point
star_size <- 7 # size of significance stars

# plot herbivory on receiver plants
p_receiver_damage <-
  ggplot(
    receiver_damage,
    aes(x = population, y = herbivory_receiver, fill = treatment)
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
    width = width_boxplot,
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
    width = width_boxplot,
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

  # tighten panel padding
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, 109, 20),
    expand = expansion(mult = c(0, 0.05))
  ) +

  # use the same two-color palette for both fill and point color
  scale_color_manual(values = pal_treat, name = "Treatment") +
  scale_fill_manual(values = pal_treat, name = "Treatment") +

  # labels and theme
  labs(x = "Population", y = "Leaf damage on receiver plants (%)") +
  theme(
    panel.grid.major.x = element_blank(),
    # legend.background = element_rect(
    #   color = "black",
    #   linewidth = 0.5,
    #   linetype = "solid"
    # ),
    legend.position = c(0.735, 0.89)
  )

# compute a nice y position per population (just above the tallest value)
stars_receiver_damage <- receiver_damage %>%
  dplyr::group_by(population) %>%
  dplyr::summarise(
    y = max(herbivory_receiver, na.rm = TRUE) +
      0.025 * diff(range(herbivory_receiver, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    label = dplyr::case_when(
      pvalue_receiver_damage < 0.001 ~ "***",
      pvalue_receiver_damage < 0.01 ~ "**",
      pvalue_receiver_damage < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# add to your existing plot object
p_receiver_damage <-
  p_receiver_damage +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.10))) + # a bit of headroom
  geom_text(
    data = stars_receiver_damage,
    aes(x = population, y = y, label = label),
    inherit.aes = FALSE,
    size = star_size
  )

# save plot
f <- 0.85 # scaling factor for dimensions
ggsave(
  plot = p_receiver_damage,
  "figures/fig-receiver-damage.png",
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)

# additional: regular boxplot --------------------------------------------

# global plot parameters
position_boxplot <- 0.6 # global knob for horizontal dodging; larger = more separation
transparency_boxplot <- 1 # alpha for all layers so overlaps are visible
width_boxplot <- 0.5 # max halfeye width (as a fraction of panel width)
mean_size <- 2.5 # size of mean point
star_size <- 7 # size of significance stars

# plot herbivory on receiver plants
p_receiver_damage_boxplot <-
  ggplot(
    receiver_damage,
    aes(x = population, y = herbivory_receiver, fill = treatment)
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

  # tighten panel padding
  scale_y_continuous(
    breaks = seq(0, 109, 20),
    expand = expansion(mult = c(0, 0.05))
  ) +

  # use the same two-color palette for both fill and point color
  scale_fill_manual(values = pal_treat, name = "Treatment") +

  # labels and theme
  labs(x = "Population", y = "Leaf damage on receiver plants (%)") +
  theme(
    panel.grid.major.x = element_blank(),
    # legend.background = element_rect(
    #   color = "black",
    #   linewidth = 0.5,
    #   linetype = "solid"
    # ),
    legend.position = c(0.735, 0.89)
  )

# compute a nice y position per population (just above the tallest value)
stars_receiver_damage <- receiver_damage %>%
  dplyr::group_by(population) %>%
  dplyr::summarise(
    y = max(herbivory_receiver, na.rm = TRUE) +
      0.025 * diff(range(herbivory_receiver, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    label = dplyr::case_when(
      pvalue_receiver_damage < 0.001 ~ "***",
      pvalue_receiver_damage < 0.01 ~ "**",
      pvalue_receiver_damage < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# add to your existing plot object
p_receiver_damage_boxplot <-
  p_receiver_damage_boxplot +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.10))) + # a bit of headroom
  geom_text(
    data = stars_receiver_damage,
    aes(x = population, y = y, label = label),
    inherit.aes = FALSE,
    size = star_size
  )

# save plot
f <- 0.85 # scaling factor for dimensions
ggsave(
  plot = p_receiver_damage_boxplot,
  "figures/fig-receiver-damage-boxplot.png",
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
