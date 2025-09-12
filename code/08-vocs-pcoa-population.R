# VOCs PCoA for populations

# load data and variables ------------------------------------------------

# run permanova script, which loads data and variables
source("code/07-vocs-permanova.R")

# PCoA -------------------------------------------------------------------

# use capscale to do PCoA
vocs_pcoa_population <- capscale(
  compounds_bray ~ 1 + Condition(vocs$treatment),
  data = vocs,
  comm = vocs %>% dplyr::select(voc1:voc17)
)

# extract eigenvalues
eigenvalues_population <- eigenvals(vocs_pcoa_population)

# proportion of variance explained by each axis
variance_explained_population <-
  eigenvalues_population / sum(eigenvalues_population)

# site ordination scores for the first two axes
ordination_scores_population <-
  data.frame(
    id = rownames(vocs_pcoa_population$CA$u),
    population = vocs$population,
    plant = vocs$code,
    MDS1 = scores(vocs_pcoa_population, scaling = 3, correlation = TRUE)$sites[,
      1
    ],
    MDS2 = scores(vocs_pcoa_population, scaling = 3, correlation = TRUE)$sites[,
      2
    ]
  )

# envfit for fitted order arrows
envfit_population <- envfit(
  vocs_pcoa_population,
  vocs %>% dplyr::select(code, population, voc1:voc17)
)

# extract centroids for both populations
centroids_population <- data.frame(
  x = rownames(envfit_population$factors$centroids),
  MDS1 = envfit_population$factors$centroids[, 1],
  MDS2 = envfit_population$factors$centroids[, 2]
) %>%
  filter(x == "populationBon" | x == "populationCai")

# extract arrows for VOC compounds
arrows_population <- data.frame(
  x = rownames(envfit_population$vectors$arrows),
  MDS1 = envfit_population$vectors$arrows[, 1],
  MDS2 = envfit_population$vectors$arrows[, 2],
  pval = envfit_population$vectors$pvals,
  rsq = envfit_population$vectors$r
) %>%
  # keep only keep significant arrows with r2 >= 0.6
  filter(pval <= 0.05, rsq >= 0.6)

# change the names of the compounds to more meaningful names from vocs_info
arrows_population$x <-
  vocs_info$compound[match(arrows_population$x, vocs_info$id)]

# can roman letters be used in labels? for example: α-Pinene

# define axis labels with variance explained
xlabel <- paste0(
  "MDS1 (",
  round(variance_explained_population[1] * 100, 1),
  "% of total variation)"
)
ylabel <- paste0(
  "MDS2 (",
  round(variance_explained_population[2] * 100, 1),
  "% of total variation)"
)

# plot -------------------------------------------------------------------

# global plot parameters
transparency_pcoa <- 0.9

# plot PCoA for populations and VOCs
p_pcoa_population <-
  ordination_scores_population %>%
  ggplot(aes(x = MDS1, y = MDS2)) +

  # add dashed lines at 0,0
  geom_hline(
    aes(yintercept = 0),
    lty = "dashed",
    size = 0.5,
    color = "grey30"
  ) +
  geom_vline(
    aes(xintercept = 0),
    lty = "dashed",
    size = 0.5,
    color = "grey30"
  ) +

  # add ellipses for populations
  stat_ellipse(
    aes(color = population, fill = population),
    geom = "polygon",
    level = 0.95,
    alpha = 0.1,
    linewidth = 0.75
  ) +

  # add points for plants
  geom_point(
    aes(fill = population),
    shape = 21,
    size = 3,
    stroke = 0.33,
    color = "black",
    alpha = transparency_pcoa
  ) +

  # add arrows for significant VOC compounds
  geom_segment(
    data = arrows_population,
    aes(
      x = 0,
      xend = MDS1 * as.numeric(rsq),
      y = 0,
      yend = MDS2 * as.numeric(rsq)
    ),
    arrow = arrow(length = unit(0.25, "cm")),
    colour = "black",
    size = 0.7,
  ) +

  # add labels with the names of the VOCs well separated from each other and with a white background
  geom_label_repel(
    data = arrows_population,
    aes(
      x = MDS1 * as.numeric(rsq),
      y = MDS2 * as.numeric(rsq),
      label = x
    ),
    alpha = 0.5,
    color = NA,
    label.size = 0,
    segment.color = NA
  ) +

  # trick to add transparent background but not transparent text!
  geom_label_repel(
    data = arrows_population,
    aes(
      x = MDS1 * as.numeric(rsq),
      y = MDS2 * as.numeric(rsq),
      label = x
    ),
    color = "black",
    fill = NA,
    segment.color = NA
  ) +

  # color palette for populations
  scale_fill_manual(name = "Population", values = pal_pop) +
  scale_color_manual(name = "Population", values = pal_pop) +

  # remove axiss lines
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.625, 0.95),
    text = element_text(size = 12),
    plot.margin = margin(20, 5, 5, 5)
  ) +

  # add axis labels
  labs(x = xlabel, y = ylabel)

# save plot
f <- 0.9 # scaling factor for dimensions
ggsave(
  "figures/pcoa-vocs-population.png",
  p_pcoa_population,
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
