# load packages
library(tidyverse)
library(MetBrewer)
library(MoMAColors) # colors from painting at the Museum of Modern Art
library(readxl)

# define theme for plots
theme_set(theme_classic(base_size = 14))

# colorblind friendly palette: 2 colors for populations
pal_pop <- met.brewer("OKeeffe1")[c(3, 9)]
names(pal_pop) <- c("Bon", "Cai")

# colorblind friendly palette: 2 colors for treatments
pal_treat <- moma.colors("Avedon")[c(6, 9)]
names(pal_treat) <- c(
  "Control",
  "Herbivore-induced"
)

# load data
dat <- readxl::read_xlsx("data/arabidopsis.xlsx") %>%
  # transform population values from B to Bon and from C to Cai
  mutate(
    population = recode(population, "B" = "Bon", "C" = "Cai"),
    population = factor(population, levels = c("Bon", "Cai"))
  ) %>%
  # transform treatment values from o to Control and from h to Herbivore-induced
  mutate(
    treatment = recode(treatment, "o" = "Control", "h" = "Herbivore-induced"),
    treatment = factor(treatment, levels = c("Control", "Herbivore-induced"))
  ) %>%
  # add interaction column for population and treatment
  mutate(pop_treat = interaction(population, treatment, sep = "_"))


# plot -------------------------------------------------------------------

# global plot parameters
position_boxplot <- 3 # global knob for horizontal dodging; larger = more separation
transparency_boxplot <- 0.9 # alpha for all layers so overlaps are visible
justification_boxplot <- -0.2 * position_boxplot # shifts right half leftwards and left half rightwards
width_boxplot <- 0.35 # max halfeye width (as a fraction of panel width)

# plot herbivory on receiver plants
p_damage_receiver <-
  ggplot(dat, aes(x = population, y = herbivory_receiver, fill = treatment)) +

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
    adjust = 0.5, # kernel bandwidth (smaller = wigglier). note: different from right half
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
    adjust = 0.75, # smoother than control; set equal to control if you want identical smoothing
    alpha = transparency_boxplot,
    width = width_boxplot,
    .width = 0,
    point_colour = NA # keep slab outline (default) for a subtle edge; set slab_color = NA to remove
  ) +

  # tighten panel padding
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, 109, 20),
    expand = expansion(mult = c(0, 0.025))
  ) +

  # use the same two-color palette for both fill and point color
  scale_color_manual(values = pal_treat, name = "Treatment") +
  scale_fill_manual(values = pal_treat, name = "Treatment") +

  # labels and theme
  labs(x = "Population", y = "Leaf damage on receiver plants (%)") +
  theme(legend.position = "top", panel.grid.major.x = element_blank())

# stars you used in the barplot (edit as needed)
star_lbl <- c(Bon = "**", Cai = "**") # or e.g., c(Bon="***", Cai="ns")

# compute a nice y position per population (just above the tallest value)
stars_df <- dat |>
  dplyr::group_by(population) |>
  dplyr::summarise(
    y_top = max(herbivory_receiver, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    y = y_top + 0.05 * diff(range(dat$herbivory_receiver, na.rm = TRUE)),
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
    size = 5
  )

p_damage_receiver

# save plot
ggsave(
  plot = p_damage_receiver,
  "figures/boxplot_herbivory_receiver.jpeg",
  width = 6.5,
  height = 4.5,
  dpi = 640
)
