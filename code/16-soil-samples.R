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
    sd_abundance = sd(abundance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(type, id)

# plot: points for mean and error bars for sd; all of the same VOC type grouped together; abundance on x axis
soils_sum %>%
  group_by(type) %>%
  mutate(max_mean = max(mean_abundance)) %>%
  ungroup() %>%
  arrange(desc(max_mean), type, desc(mean_abundance)) %>%
  mutate(
    type = factor(type, levels = unique(type)),
    compound = factor(compound, levels = rev(unique(compound)))
  ) %>%
  ggplot(aes(x = mean_abundance, y = compound, color = type)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(
      xmin = mean_abundance - sd_abundance,
      xmax = mean_abundance + sd_abundance
    ),
    height = 0.2
  ) +
  labs(
    title = "Mean abundance of VOCs in soil samples",
    x = "Mean abundance",
    y = "VOC compound",
    color = "VOC type"
  ) +
  # add legend inside plot area
  theme(legend.position = c(0.75, 0.25))

# save the plot
ggsave(
  "figures/soil-samples-voc-abundance.jpeg",
  width = 6,
  height = 7
)
