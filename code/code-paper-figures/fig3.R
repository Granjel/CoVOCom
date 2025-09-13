# code to generate figure 2 for the paper

# run workflow? TRUE or FALSE (first time, set to TRUE)
workflow_run <- FALSE

# run config file
source("code/code-paper-figures/config-paper-figures.R")

# figure 2: emitter damage by population ---------------------------------

# tweak panel a
p_pcoa_treatment <- p_pcoa_treatment +
  theme(
    text = element_text(size = 13)
  )

# tweak panel b
p_glvs <- p_glvs +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(45, 20, 5, 20),
    axis.title.x = element_text(color = "white")
  )

# tweak panel c
p_alkanes <- p_alkanes +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(45, 20, 5, 20)
  )

# tweak panel d
p_ketones <- p_ketones +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(45, 20, 5, 20)
  )

# compose
fig3 <- ggarrange(
  p_pcoa_treatment,
  p_glvs,
  p_alkanes,
  p_ketones,
  ncol = 2,
  nrow = 2,
  labels = "auto",
  common.legend = TRUE,
  legend = "top",
  font.label = list(size = 18, face = "bold"),
  vjust = 2.25,
  hjust = -1.75
)

# save figure
ggsave(
  plot = fig3,
  "figures/paper-figures/fig3.png",
  width = 7.5,
  height = 7,
  dpi = fig_dpi
)
