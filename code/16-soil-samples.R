# analyse the volatiles from the background (soil samples, containers, etc.)

# load packages and data -------------------------------------------------
source("code/03-load-data.R")

# remove total from soils data
soils <- soils %>% filter(id != "total")

# add the category of VOC from vocs_info$type
soils <- soils %>%
  left_join(
    vocs_info %>%
      dplyr::select(id, type),
    by = "id"
  ) %>%
  relocate(type, .after = id)

# figure to show abundances per VOC and type -----------------------------

# summarise: mean and sd per VOC and type (keep compound name)
soils_sum <- soils %>%
  group_by(id, type, compound) %>%
  summarise(
    mean_abundance = mean(abundance, na.rm = TRUE),
    q05_abundance = quantile(abundance, probs = 0.05, na.rm = TRUE),
    q95_abundance = quantile(abundance, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(type, id)

# plot: points for mean and error bars for sd; order types alphabetically and compounds alphabetically within each type; abundance on x axis
plot_soil_blanks <- soils_sum %>%
  # order types alphabetically and compounds alphabetically within each type (reversed)
  arrange(type, compound) %>%
  mutate(
    type = factor(type, levels = sort(unique(type))),
    compound = factor(compound, levels = rev(unique(compound)))
  ) %>%
  ggplot(aes(x = mean_abundance, y = compound, color = type)) +
  geom_point(size = 2.75) +
  geom_errorbarh(
    aes(
      xmin = q05_abundance,
      xmax = q95_abundance
    ),
    height = 0.3,
    size = 0.6
  ) +
  labs(
    # title = "Mean abundance of VOCs in soil samples",
    x = expression(
      "Abundance (" * ng ~ h^{
        -1
      } *
        ")"
    ),
    y = "VOC compound",
    color = "VOC type"
  ) +
  # add a viridis color scale
  scale_color_viridis_d(begin = 0.075, end = 0.95) +
  # add legend inside plot area
  theme(legend.position = c(0.725, 0.5))

# save the plot
ggsave(
  "figures/soil-samples-voc-abundance.jpeg",
  plot_soil_blanks,
  width = 6,
  height = 7
)

# add VOC abundance means and sd for populations and treatments ----------

# summarise: mean and sd per VOC, population, and treatment (keep compound name and type)
vocs_sum <- vocs_type %>%
  group_by(population, treatment, compound) %>%
  summarise(
    `Mean Short-chain oxygenated VOCs` = mean(
      `Short-chain oxygenated VOCs`,
      na.rm = TRUE
    ),
    `Q05 Short-chain oxygenated VOCs` = quantile(
      `Short-chain oxygenated VOCs`,
      probs = 0.05,
      na.rm = TRUE
    ),
    `Q95 Short-chain oxygenated VOCs` = quantile(
      `Short-chain oxygenated VOCs`,
      probs = 0.95,
      na.rm = TRUE
    ),
    `Mean Terpenoids` = mean(`Terpenoids`, na.rm = TRUE),
    `Q05 Terpenoids` = quantile(`Terpenoids`, probs = 0.05, na.rm = TRUE),
    `Q95 Terpenoids` = quantile(`Terpenoids`, probs = 0.95, na.rm = TRUE),
    `Mean Alcohols and esters` = mean(`Alcohols and esters`, na.rm = TRUE),
    `Q05 Alcohols and esters` = quantile(
      `Alcohols and esters`,
      probs = 0.05,
      na.rm = TRUE
    ),
    `Q95 Alcohols and esters` = quantile(
      `Alcohols and esters`,
      probs = 0.95,
      na.rm = TRUE
    ),
    `Mean Long-chain aldehydes` = mean(`Long-chain aldehydes`, na.rm = TRUE),
    `Q05 Long-chain aldehydes` = quantile(
      `Long-chain aldehydes`,
      probs = 0.05,
      na.rm = TRUE
    ),
    `Q95 Long-chain aldehydes` = quantile(
      `Long-chain aldehydes`,
      probs = 0.95,
      na.rm = TRUE
    ),
    `Mean Long-chain alkanes` = mean(`Long-chain alkanes`, na.rm = TRUE),
    `Q05 Long-chain alkanes` = quantile(
      `Long-chain alkanes`,
      probs = 0.05,
      na.rm = TRUE
    ),
    `Q95 Long-chain alkanes` = quantile(
      `Long-chain alkanes`,
      probs = 0.95,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  # long format
  pivot_longer(
    cols = starts_with(c("Mean", "Q05", "Q95")),
    names_to = c(".value", "type"),
    names_pattern = "(Mean|Q05|Q95) (.+)"
  ) %>%
  # remove nonsense values (NA/NaN)
  filter(!is.na(Mean))

# plot the same way as soils_sum but faceted by population and with different shape dodged points for treatment; color by type of VOC
plot_vocs_experiment <- vocs_sum %>%
  # order types alphabetically and compounds alphabetically within each type
  arrange(type, compound) %>%
  mutate(
    type = factor(type, levels = sort(unique(type))),
    compound = factor(compound, levels = rev(unique(compound)))
  ) %>%
  # reverse the factor levels for treatment so dodged points plot in reverse order
  mutate(treatment = forcats::fct_rev(factor(treatment))) %>%
  ggplot(aes(x = Mean, y = compound, color = type, shape = treatment)) +
  geom_point(
    position = position_dodge(width = 0.7),
    size = 2.75
  ) +
  geom_errorbarh(
    aes(
      xmin = Q05,
      xmax = Q95
    ),
    height = 0.3,
    size = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  labs(
    x = expression(
      "Abundance (" * ng ~ h^{
        -1
      } *
        ")"
    ),
    y = "VOC compound",
    color = "VOC type",
    shape = "Treatment"
  ) +
  # add a viridis color scale
  scale_color_viridis_d(begin = 0.075, end = 0.95) +
  # adjust x axis padding
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  # reverse the order of treatment items only in the legend
  guides(shape = guide_legend(reverse = TRUE)) +
  # remove rectangle around facet labels and make labels bold
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  facet_wrap(~population, ncol = 1)

# save the plot
ggsave(
  "figures/voc-abundance-by-population-and-treatment.jpeg",
  plot_vocs_experiment,
  width = 8,
  height = 9.5
)

# lob --------------------------------------------------------------------

# calculate the limit of blanks (lob) for each VOC as the 95% quantile of the abundance values in the soil samples for each compound and type
soil_lob <- soils %>%
  group_by(id, type, compound) %>%
  summarise(
    lob = quantile(abundance, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# create a single summed VOC column and drop the original type columns
vocs_type_sum <- vocs_type %>%
  dplyr::mutate(
    value = rowSums(
      dplyr::across(`Short-chain oxygenated VOCs`:`Long-chain alkanes`),
      na.rm = TRUE
    )
  ) %>%
  dplyr::select(-(`Short-chain oxygenated VOCs`:`Long-chain alkanes`))

# add the lob for each VOC to the vocs_type_sum data frame by joining on compound name and create a new column indicating whether the mean abundance is above the lob
vocs_type_sum <- vocs_type_sum %>%
  left_join(
    soil_lob %>%
      dplyr::select(compound, lob),
    by = "compound"
  )

# create a new column indicating if the LOB is greater than the abundance value
vocs_type_sum <- vocs_type_sum %>%
  mutate(above_lob = value > lob)

# by compound, calculate the percentage of samples that are above the LOB
lob_perc <- vocs_type_sum %>%
  group_by(compound) %>%
  summarise(
    percent_above_lob = mean(above_lob, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(percent_above_lob)

# check if those are equally distributed across treatment and population
lob_perc_treatment_population <- vocs_type_sum %>%
  group_by(compound, treatment, population) %>%
  summarise(
    percent_above_lob = mean(above_lob, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# plot the percentage of samples above the lob for each compound, colored by compound and faceted by treatment and population
plot_lob_perc <- lob_perc_treatment_population %>%
  left_join(
    vocs_info %>% dplyr::select(compound, type),
    by = "compound"
  ) %>%
  arrange(type, compound) %>%
  mutate(
    type = factor(type, levels = sort(unique(type))),
    compound = factor(compound, levels = rev(unique(compound)))
  ) %>%
  ggplot(aes(x = percent_above_lob, y = compound, fill = type)) +
  geom_col(width = 0.7) +
  labs(
    title = "Percentage of samples above the LOB",
    x = "Percentage of samples above LOB",
    y = "VOC compound",
    fill = "VOC type"
  ) +
  scale_fill_viridis_d(begin = 0.075, end = 0.95) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  facet_grid(treatment ~ population)

# save the plot
ggsave(
  "figures/percentage-above-lob-by-compound-treatment-population.jpeg",
  plot_lob_perc,
  width = 10,
  height = 6
)
