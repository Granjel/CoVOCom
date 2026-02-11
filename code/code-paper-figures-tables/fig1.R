# code to generate figure 1 for the paper

# load necessary scripts
source("code/03-load-data.R")

# load temperature data for the figure
temperature <- read.csv("data/climate-populations.csv", header = TRUE) %>%
  dplyr::select(-total_precipitation) %>%
  # make long format
  pivot_longer(
    cols = -population,
    names_to = "min_max",
    values_to = "temperature"
  )

# load precipitation data for the figure
precipitation <- read.csv("data/climate-populations.csv", header = TRUE) %>%
  dplyr::select(population, total_precipitation)

# figure temperature and precipitation -----------------------------------

# panel a: temperature range
p_temperature <- ggplot(
  temperature,
  aes(x = population, y = temperature, group = population, color = population)
) +
  geom_point(size = 3) +
  # add line between min and max temperature
  geom_line(aes(group = population), size = 1.5) +
  # add color palette
  scale_color_manual(name = "Population:", values = pal_pop) +
  scale_y_continuous(
    limits = c(0, 26),
    breaks = seq(0, 26, by = 5),
    expand = expansion(mult = c(0, 0.05)) # no space at bottom, 5% at top
  ) +
  labs(
    x = NULL,
    y = "Temperature (°C)"
  ) +
  theme(
    # remove any information on axis x
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.margin = margin(5, 5, 20, 5),
    axis.title.y = element_text(size = 14)
  )

# panel b: precipitation (with y axis on the right, not the left)
p_precipitation <- ggplot(
  precipitation,
  aes(
    x = population,
    y = total_precipitation,
    group = population,
    fill = population
  )
) +
  geom_bar(stat = "identity", width = 2 / 3) +
  scale_fill_manual(name = "Population:", values = pal_pop) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 775),
    expand = expansion(mult = c(0, 0.05)), # no space at bottom, 5% at top
    sec.axis = sec_axis(
      ~.,
      name = "Total precipitation (mm)",
      breaks = seq(0, 775, by = 150)
    )
  ) +
  theme(
    # legend.position = "none",
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.line.y.left = element_blank(),
    axis.title.y.right = element_text(size = 14, angle = 270),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.margin = margin(5, 5, 20, 5)
  )

# compose
p_temp_prec <- ggarrange(
  p_temperature,
  NULL,
  p_precipitation,
  common.legend = TRUE,
  legend = "bottom",
  ncol = 3,
  align = "h",
  widths = c(0.49, 0.02, 0.49)
)

# figure genetic clusters ------------------------------------------------

# load and reshape data
clusters <- read.csv("data/clusters-populations.csv", header = TRUE) %>%
  arrange(population, genotype) %>%
  mutate(
    genotype = factor(genotype, levels = unique(genotype))
  ) %>%
  pivot_longer(
    cols = c(cluster1, cluster2),
    names_to = "cluster",
    values_to = "prop"
  ) %>%
  mutate(
    cluster = factor(cluster, levels = c("cluster1", "cluster2")),
    pop_clu = interaction(population, cluster, sep = "_"),
    pop_clu = factor(
      pop_clu,
      levels = c("Bon_cluster1", "Bon_cluster2", "Cai_cluster1", "Cai_cluster2")
    )
  )

# define two shades per population
pal_clusters <- c(
  "Bon_cluster1" = unname(darken(pal_pop["Bon"], 0.075)),
  "Bon_cluster2" = unname(lighten(pal_pop["Bon"], 0.35)),
  "Cai_cluster1" = unname(darken(pal_pop["Cai"], 0.075)),
  "Cai_cluster2" = unname(lighten(pal_pop["Cai"], 0.35))
)

# plot
p_clusters <- ggplot(clusters, aes(x = genotype, y = prop, fill = pop_clu)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = pal_clusters) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.04, 0))) +
  labs(x = "Individual", y = NULL) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    plot.margin = margin(20, 10, 10, 12.5),
    axis.title.x = element_text(size = 14)
  )

# combine all panels
p_temp_prec_clu <- ggarrange(
  p_temp_prec,
  NULL,
  p_clusters,
  ncol = 1,
  heights = c(0.5, 0, 0.5),
  labels = c("d", "", "e"),
  font.label = list(size = 20, face = "bold"),
  vjust = c(1.25, 0, 0.5),
  hjust = -0.15
)

# save plot
ggsave(
  plot = p_temp_prec_clu,
  "figures/paper-figures/fig1.jpeg",
  width = 5,
  height = 7.25,
  dpi = fig_dpi
)
