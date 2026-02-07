# VOCs PCoA for treatments

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

# use capscale to do PCoA
vocs_pcoa_treatment <- capscale(
  compounds_bray ~ 1 + Condition(population),
  data = vocs_subset
)

# extract eigenvalues from unconstrained axes
eigenvalues_treatment <- vocs_pcoa_treatment$CA$eig

# proportion of variance explained by each axis
variance_explained_treatment <-
  eigenvalues_treatment / sum(eigenvalues_treatment)

# site ordination scores for the first two axes
site_scores <- scores(
  vocs_pcoa_treatment,
  scaling = 3,
  correlation = TRUE
)$sites

# create a data frame with ordination scores and metadata for plotting
ordination_scores_treatment <-
  data.frame(
    id = rownames(site_scores),
    treatment = vocs_subset$treatment,
    plant = vocs_subset$code,
    MDS1 = site_scores[, 1],
    MDS2 = site_scores[, 2]
  )

# envfit for treatment centroids
envfit_treatment <- envfit(
  vocs_pcoa_treatment,
  data.frame(treatment = vocs_subset$treatment)
)

# extract centroids for treatments
centroids_treatment <- data.frame(
  x = gsub(
    "^treatment",
    "",
    rownames(envfit_treatment$factors$centroids)
  ),
  MDS1 = envfit_treatment$factors$centroids[, 1],
  MDS2 = envfit_treatment$factors$centroids[, 2]
)

# envfit for VOC arrows
envfit_vocs <- envfit(
  vocs_pcoa_treatment,
  vocs_subset %>% dplyr::select(starts_with("voc"))
)

# extract arrows for significant VOC compounds
arrows_treatment <- data.frame(
  x = rownames(envfit_vocs$vectors$arrows),
  MDS1 = envfit_vocs$vectors$arrows[, 1],
  MDS2 = envfit_vocs$vectors$arrows[, 2],
  pval = envfit_vocs$vectors$pvals,
  rsq = envfit_vocs$vectors$r
) %>%
  dplyr::filter(pval <= 0.05, rsq >= 0.6)

# change the names of the compounds
arrows_treatment$x <-
  vocs_info$compound[match(arrows_treatment$x, vocs_info$id)]

# define axis labels with variance explained
xlabel <- paste0(
  "MDS1 (",
  round(variance_explained_treatment[1] * 100, 1),
  "%)"
)
ylabel <- paste0(
  "MDS2 (",
  round(variance_explained_treatment[2] * 100, 1),
  "%)"
)

# plot -------------------------------------------------------------------

# global plot parameters
transparency_pcoa <- 0.9

# plot PCoA for treatments and VOCs
p_pcoa_treatment <-
  ordination_scores_treatment %>%
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

  # add ellipses for treatments
  stat_ellipse(
    aes(color = treatment, fill = treatment),
    geom = "polygon",
    level = 0.95,
    alpha = 0.1,
    linewidth = 0.75
  ) +

  # add points for plants
  geom_point(
    aes(fill = treatment),
    shape = 21,
    size = 2,
    stroke = 0.33,
    color = "black",
    alpha = transparency_pcoa
  ) +

  # add arrows for significant VOC compounds
  geom_segment(
    data = arrows_treatment,
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

  # add centroids with colors for treatments and different shape than other points
  geom_point(
    data = centroids_treatment,
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
    data = arrows_treatment,
    aes(
      x = MDS1 * as.numeric(rsq),
      y = MDS2 * as.numeric(rsq),
      label = x
    ),
    size = 3.25,
    alpha = 0.5,
    color = NA,
    label.size = 0,
    segment.color = NA
  ) +

  # trick to add transparent background but not transparent text!
  geom_label_repel(
    data = arrows_treatment,
    aes(
      x = MDS1 * as.numeric(rsq),
      y = MDS2 * as.numeric(rsq),
      label = x
    ),
    size = 3.25,
    color = "black",
    fill = NA,
    segment.color = NA
  ) +

  # color palette for treatments
  scale_fill_manual(name = "Treatment", values = pal_treat) +
  scale_color_manual(name = "Treatment", values = pal_treat) +

  # remove axiss lines
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.15, 0.95),
    # legend.position = "top",
    text = element_text(size = 12),
    plot.margin = margin(20, 5, 5, 5)
  ) +

  # add axis labels
  labs(x = xlabel, y = ylabel)

# save plot
f <- 0.9 # scaling factor for dimensions
ggsave(
  "figures/pcoa-vocs-treatment-all.png",
  p_pcoa_treatment,
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
