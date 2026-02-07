# VOCs PCoA for populations

# load data and variables ------------------------------------------------

# load packages and data
source("code/03-load-data.R")

# load saved subsets and distance matrices from PERMANOVA
vocs_objects <- readRDS(
  file.path(
    "tables/permanova/objects-for-pcoa",
    "vocs-subsets-and-distances.rds"
  )
)

# choose which VOC type to analyze (must match safe_name keys)
voc_type_key <- "all"

# subset and distance matrix for this VOC type
vocs_subset <- vocs_objects$subsets[[voc_type_key]]
compounds_bray <- as.dist(vocs_objects$distances[[voc_type_key]])

# PCoA -------------------------------------------------------------------

# partial PCoA: control for treatment, focus on population
vocs_pcoa_population <- capscale(
  compounds_bray ~ 1 + Condition(treatment),
  data = vocs_subset
)

# extract eigenvalues from unconstrained axes
eigenvalues_population <- vocs_pcoa_population$CA$eig

# proportion of variance explained by each axis
variance_explained_population <-
  eigenvalues_population / sum(eigenvalues_population)

# site ordination scores for the first two axes
site_scores <- scores(
  vocs_pcoa_population,
  scaling = 3,
  correlation = TRUE
)$sites

# create a data frame with ordination scores and metadata for plotting
ordination_scores_population <-
  data.frame(
    id = rownames(site_scores),
    population = vocs_subset$population,
    plant = vocs_subset$code,
    MDS1 = site_scores[, 1],
    MDS2 = site_scores[, 2]
  )

# envfit for population centroids
envfit_population <- envfit(
  vocs_pcoa_population,
  data.frame(population = vocs_subset$population)
)

# extract centroids for populations
centroids_population <- data.frame(
  x = rownames(envfit_population$factors$centroids),
  MDS1 = envfit_population$factors$centroids[, 1],
  MDS2 = envfit_population$factors$centroids[, 2]
)

# envfit for VOC arrows
envfit_vocs <- envfit(
  vocs_pcoa_population,
  vocs_subset %>% dplyr::select(starts_with("voc"))
)

# extract arrows for significant VOC compounds
arrows_population <- data.frame(
  x = rownames(envfit_vocs$vectors$arrows),
  MDS1 = envfit_vocs$vectors$arrows[, 1],
  MDS2 = envfit_vocs$vectors$arrows[, 2],
  pval = envfit_vocs$vectors$pvals,
  rsq = envfit_vocs$vectors$r
) %>%
  dplyr::filter(pval <= 0.05, rsq >= 0.6)

# change the names of the compounds
arrows_population$x <-
  vocs_info$compound[match(arrows_population$x, vocs_info$id)]

# define axis labels with variance explained
xlabel <- paste0(
  "MDS1 (",
  round(variance_explained_population[1] * 100, 1),
  "%)"
)
ylabel <- paste0(
  "MDS2 (",
  round(variance_explained_population[2] * 100, 1),
  "%)"
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
    size = 2,
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
    size = 0.7
  ) +

  # add centroids with colors for populations and different shape than other points
  geom_point(
    data = centroids_population,
    aes(x = MDS1, y = MDS2, color = x, fill = x),
    shape = 24,
    size = 3,
    stroke = 0.55,
    color = "black",
    alpha = transparency_pcoa,
    show.legend = FALSE
  ) +

  # add labels with the names of the VOCs well separated from each other and with a white background
  geom_label_repel(
    data = arrows_population,
    aes(
      x = MDS1 * as.numeric(rsq),
      y = MDS2 * as.numeric(rsq),
      label = x
    ),
    size = 3.25,
    alpha = 0.5,
    color = NA,
    label.size = 0,
    segment.color = NA,
    max.overlaps = Inf
  ) +

  # trick to add transparent background but not transparent text!
  geom_label_repel(
    data = arrows_population,
    aes(
      x = MDS1 * as.numeric(rsq),
      y = MDS2 * as.numeric(rsq),
      label = x
    ),
    size = 3.25,
    color = "black",
    fill = NA,
    segment.color = NA,
    max.overlaps = Inf
  ) +

  # color palette for populations
  scale_fill_manual(name = "Population", values = pal_pop) +
  scale_color_manual(name = "Population", values = pal_pop) +

  # remove axiss lines
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.15, 0.9),
    text = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5)
  ) +

  # add axis labels
  labs(x = xlabel, y = ylabel)

# save plot
f <- 0.9 # scaling factor for dimensions
ggsave(
  "figures/pcoa-vocs-population-all.png",
  p_pcoa_population,
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
