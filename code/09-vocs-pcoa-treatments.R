# VOCs PCoA for treatments

# load data and variables ------------------------------------------------

# run permanova script, which loads data and variables
source("code/07-vocs-permanova.R")

# PCoA -------------------------------------------------------------------

# use capscale to do PCoA
vocs_pcoa_treatment <- capscale(
  compounds_bray ~ 1 + Condition(vocs$population),
  data = vocs,
  comm = vocs %>% dplyr::select(voc1:voc17)
)

# extract eigenvalues
eigenvalues_treatment <- eigenvals(vocs_pcoa_treatment)

# proportion of variance explained by each axis
variance_explained_treatment <-
  eigenvalues_treatment / sum(eigenvalues_treatment)

# site ordination scores for the first two axes
ordination_scores_treatment <-
  data.frame(
    id = rownames(vocs_pcoa_treatment$CA$u),
    treatment = vocs$treatment,
    plant = vocs$code,
    MDS1 = scores(vocs_pcoa_treatment, scaling = 3, correlation = TRUE)$sites[,
      1
    ],
    MDS2 = scores(vocs_pcoa_treatment, scaling = 3, correlation = TRUE)$sites[,
      2
    ]
  )

# envfit for fitted order arrows
envfit_treatment <- envfit(
  vocs_pcoa_treatment,
  vocs %>% dplyr::select(code, treatment, voc1:voc17)
)

# extract centroids for both treatments
centroids_treatment <- data.frame(
  x = gsub("treatment", "", rownames(envfit_treatment$factors$centroids)),
  MDS1 = envfit_treatment$factors$centroids[, 1],
  MDS2 = envfit_treatment$factors$centroids[, 2]
) %>%
  filter(x == "Control" | x == "Herbivore-induced")

# extract arrows for VOC compounds
arrows_treatment <- data.frame(
  x = rownames(envfit_treatment$vectors$arrows),
  MDS1 = envfit_treatment$vectors$arrows[, 1],
  MDS2 = envfit_treatment$vectors$arrows[, 2],
  pval = envfit_treatment$vectors$pvals,
  rsq = envfit_treatment$vectors$r
) %>%
  # keep only keep significant arrows with r2 >= 0.6
  filter(pval <= 0.05, rsq >= 0.6)

# change the names of the compounds to more meaningful names from vocs_info
arrows_treatment$x <-
  vocs_info$compound[match(arrows_treatment$x, vocs_info$id)]

# can roman letters be used in labels? for example: α-Pinene

# define axis labels with variance explained
xlabel <- paste0(
  "MDS1 (",
  round(variance_explained_treatment[1] * 100, 1),
  "% of total variation)"
)
ylabel <- paste0(
  "MDS2 (",
  round(variance_explained_treatment[2] * 100, 1),
  "% of total variation)"
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
    size = 3,
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
    legend.position = c(0.7, 0.95),
    # legend.position = "top",
    text = element_text(size = 12),
    plot.margin = margin(20, 5, 5, 5)
  ) +

  # add axis labels
  labs(x = xlabel, y = ylabel)

# save plot
f <- 0.9 # scaling factor for dimensions
ggsave(
  "figures/pcoa-vocs-treatment.png",
  p_pcoa_treatment,
  width = fig_width * f,
  height = fig_height * f,
  dpi = fig_dpi
)
