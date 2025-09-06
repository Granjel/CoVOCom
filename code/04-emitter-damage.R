# damage on emitters

# load packages and data -------------------------------------------------

# load packages
source("code/03-load-data.R")

# select only relevant data
emitter_damage <- df %>%
  dplyr::select(
    # select relevant columns
    code,
    population,
    genotype,
    treatment,
    n,
    larva_emitter,
    size_emitter,
    herbivory_emitter
  ) %>%
  filter(treatment == "Herbivore-induced") # filter only herbivory treatment

# explore data and fit model ---------------------------------------------

# data exploration
# hist(emitter_damage$herbivory_emitter) # untransformed data is right-skewed
# hist(sqrt(emitter_damage$herbivory_emitter)) # square root transformation looks better

# LMM of damage on emitters
lmm_emitter_damage <- lmer(
  sqrt(herbivory_emitter) ~ population + (1 | population:genotype),
  data = emitter_damage
)

# model diagnostics
shapiro.test(resid(lmm_emitter_damage)) # residuals are normally distributed (p > 0.05)
# hist(resid(lmm_emitter_damage)) # histogram of residuals looks reasonably normal

# estimated marginal means (EMMs) for population
emm_emitter_damage <-
  emmeans(
    lmm_emitter_damage,
    pairwise ~ population,
    adjustSigma = TRUE,
    adjust = "tukey",
    type = "response"
  )

# save EMMs to a CSV file
write.csv(
  as.data.frame(emm_emitter_damage$emmeans),
  "tables/emitter-damage-emmeans.csv",
  row.names = FALSE
)

# compact letter display (CLD) for EMMs
cld_emitter_damage <- cld(emm_emitter_damage)

# p-values for pairwise comparisons
pvalue_emitter_damage <- summary(emm_emitter_damage$contrasts)$p.value

# plot -------------------------------------------------------------------

# plot options
transparency_boxplot <- 1 # alpha for all layers so overlaps are visible
justification_boxplot <- 0.2 # shifts right half leftwards and left half rightwards
width_boxplot <- 0.2 # max halfeye width (as a fraction of panel width)
justification_hist <- 0.275 # shifts right half leftwards and left half rightwards
width_hist <- 0.3 # max halfeye width (as a fraction of panel width)
mean_size <- 2.5 # size of mean point
star_size <- 7 # size of significance stars

# plot herbivory on emitter plants
p_emitter_damage <-
  ggplot(
    emitter_damage,
    aes(x = population, y = herbivory_emitter, fill = population)
  ) +

  # jittered raw points
  # - color encodes treatment (same as fill to keep legend consistent)
  # - position_jitterdodge jitters within each population and dodges between treatments
  geom_jitter(
    aes(color = population),
    position = position_jitterdodge(
      jitter.width = 0.075, # small horizontal jitter so points do not overlap
      seed = 1 # reproducible jitter
    ),
    size = 0.75,
    alpha = transparency_boxplot
  ) +

  # central boxplot for a robust summary per group
  # - width is narrow so densities remain visible
  # - outliers hidden because raw points are already plotted
  geom_boxplot(
    position = position_nudge(
      x = c(justification_boxplot, -justification_boxplot)
    ),
    outlier.shape = NA,
    alpha = transparency_boxplot,
    width = width_boxplot,
    color = "black"
  ) +

  # distribution for control (left half)
  # - filter the data inside the layer
  # - side = "left" draws a half violin to the left of the x position
  # - justification nudges the slab so both halves mirror each other around the box
  stat_halfeye(
    data = ~ dplyr::filter(.x, population == "Bon"),
    side = "left",
    justification = 1 + justification_hist, # mirrors the right half
    adjust = 0.5, # kernel bandwidth (smaller = wigglier)
    alpha = transparency_boxplot,
    width = width_hist,
    .width = 0, # do not draw intervals
    point_colour = NA, # no point at the median
    slab_color = NA # no outline on the slab
  ) +

  # distribution for herbivore-induced (right half)
  stat_halfeye(
    data = ~ dplyr::filter(.x, population == "Cai"),
    side = "right",
    justification = -justification_hist, # mirror offset of the left half
    adjust = 0.5, # kernel bandwidth (smaller = wigglier)
    alpha = transparency_boxplot,
    width = width_hist,
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
    position = position_nudge(
      x = c(justification_boxplot, -justification_boxplot)
    )
  ) +

  # tighten panel padding
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, 109, 20),
    expand = expansion(mult = c(0, 0.038))
  ) +

  # use the same two-color palette for both fill and point color
  scale_color_manual(values = pal_pop, name = "Treatment") +
  scale_fill_manual(values = pal_pop, name = "Treatment") +

  # labels and theme
  labs(x = "Population", y = "Leaf damage on emitter plants (%)") +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")

# data frame with significance stars
stars_emitter_damage <- data.frame(
  population = 1.5,
  herbivory_emitter = max(emitter_damage$herbivory_emitter, na.rm = TRUE) *
    1.025,
  label = ifelse(
    pvalue_emitter_damage < 0.001,
    "***",
    ifelse(
      pvalue_emitter_damage < 0.01,
      "**",
      ifelse(pvalue_emitter_damage < 0.05, "*", "ns")
    )
  )
)

# add significance stars to the plot
p_emitter_damage <- p_emitter_damage +
  geom_text(
    data = stars_emitter_damage,
    aes(x = population, y = herbivory_emitter, label = label),
    inherit.aes = FALSE,
    size = star_size
  )

# save plot
f <- 0.75 # scaling factor for dimensions
ggsave(
  plot = p_emitter_damage,
  "figures/fig-emitter-damage.png",
  width = 6.5 * f,
  height = 4.5 * f,
  dpi = 640
)
