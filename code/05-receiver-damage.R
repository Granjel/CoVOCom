# damage on emitters

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


# plot -------------------------------------------------------------------

# global plot parameters
position_boxplot <- 3 # global knob for horizontal dodging; larger = more separation
transparency_boxplot <- 0.9 # alpha for all layers so overlaps are visible
justification_boxplot <- -0.2 * position_boxplot # shifts right half leftwards and left half rightwards
width_boxplot <- 0.35 # max halfeye width (as a fraction of panel width)
mean_size <- 2.5 # size of mean point
star_size <- 7 # size of significance stars

# plot herbivory on receiver plants
p_damage_receiver <-
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

# stars you used in the barplot (edit as needed)
star_lbl <- c(Bon = "**", Cai = "**") # or e.g., c(Bon="***", Cai="ns")

# compute a nice y position per population (just above the tallest value)
stars_df <- receiver_damage %>%
  dplyr::group_by(population) %>%
  dplyr::summarise(
    y_top = max(herbivory_receiver, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    y = y_top +
      0.025 * diff(range(receiver_damage$herbivory_receiver, na.rm = TRUE)),
    label = star_lbl[as.character(population)]
  )

# add to your existing plot object
p_damage_receiver <-
  p_damage_receiver +
  # scale_y_continuous(expand = expansion(mult = c(0, 0.10))) + # a bit of headroom
  geom_text(
    data = stars_df,
    aes(x = population, y = y, label = label),
    inherit.aes = FALSE,
    size = star_size
  )

# save plot
f <- 0.85 # scaling factor for dimensions
ggsave(
  plot = p_damage_receiver,
  "figures/fig-receiver-damage.png",
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
