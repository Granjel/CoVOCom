# code to generate figure 2 for the paper

# run necessary scripts
if (!exists("p_emitter_damage", inherits = TRUE)) {
  source("code/04-emitter-damage.R")
}

if (!exists("p_pcoa_population", inherits = TRUE)) {
  source("code/08-vocs-pcoa-population.R")
}

# figure 2: emitter damage by population ---------------------------------

# tweak panel a
p_emitter_damage <- p_emitter_damage +
  theme(
    text = element_text(size = 12),
    plot.margin = margin(20, 30, 5, 5)
  )

# tweak panel b
p_pcoa_population <- p_pcoa_population +
  theme(
    text = element_text(size = 12),
    legend.background = element_blank()
  )

# compose
fig2 <- ggarrange(
  p_emitter_damage,
  p_pcoa_population,
  labels = "auto",
  # align = "h",
  ncol = 2,
  widths = c(0.47, 0.53),
  heights = c(0.5, 1),
  font.label = list(size = 16, face = "bold"),
  vjust = 1.15
)

# save figure
ggsave(
  plot = fig2,
  "figures/paper-figures/fig2.jpeg",
  width = 6.75,
  height = 3.75,
  dpi = fig_dpi
)
