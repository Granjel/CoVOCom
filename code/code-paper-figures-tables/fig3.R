# code to generate figure 3 for the paper

# run necessary scripts
if (!exists("p_pcoa_treatment", inherits = TRUE)) {
  source("code/09-vocs-pcoa-treatment.R")
}

if (!exists("p_emm", inherits = TRUE)) {
  source("code/10-vocs-by-type.R")
}

# figure 3: voc types by treatment ---------------------------------------

# tweak panel a
p_pcoa_treatment <- p_pcoa_treatment +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(t = 10, r = 5, b = 20, l = 5),
    legend.position = "bottom",
    legend.margin = margin(t = 12, r = 0, b = 0, l = 0)
  )

# tweak panel b
p_emm <- p_emm +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5),
    legend.position = "none"
  )

# arrange panel a plus two blank spaces on each side
fig3a <- ggarrange(
  NULL,
  p_pcoa_treatment,
  NULL,
  ncol = 3,
  nrow = 1,
  widths = c(0.2, 0.95, 0.2),
  labels = c("", "a", ""),
  font.label = list(size = 19, face = "bold"),
  vjust = 2.25,
  hjust = -0.5
)

# compose final figure by stacking fig3a and fig3b
fig3 <- ggarrange(
  fig3a,
  p_emm,
  ncol = 1,
  nrow = 2,
  heights = c(0.6, 0.4),
  labels = c("", "b"),
  font.label = list(size = 19, face = "bold"),
  vjust = 2.2,
  hjust = -0.78
)

# save figure
ggsave(
  plot = fig3,
  "figures/paper-figures/fig3-emm.jpeg",
  width = 6.25,
  height = 7.25,
  dpi = fig_dpi
)
