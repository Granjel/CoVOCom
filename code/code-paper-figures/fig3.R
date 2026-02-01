# code to generate figure 3 for the paper

# run necessary scripts
if (!exists("p_pcoa_treatment", inherits = TRUE)) {
  source("code/09-vocs-pcoa-treatment.R")
}

if (!exists("p_glvs", inherits = TRUE)) {
  source("code/11-glvs.R")
}

if (!exists("p_alkanes", inherits = TRUE)) {
  source("code/13-long-chain-alkanes.R")
}

# figure 3: voc types by treatment ---------------------------------------

# tweak panel a
p_pcoa_treatment <- p_pcoa_treatment +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(t = 20, r = 5, b = 20, l = 5)
  )

# tweak panel b
p_glvs <- p_glvs +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(t = 10, r = 20, b = 5, l = 5)
  )

# tweak panel c
p_alkanes <- p_alkanes +
  theme(
    text = element_text(size = 13),
    plot.margin = margin(t = 10, r = 5, b = 5, l = 0)
  ) +
  # break y-axis label into two lines
  ylab(
    expression(
      atop(
        "Long chain",
        "alkanes (" * ng ~ h^{
          -1
        } *
          ")"
      )
    )
  )

# arrange panel a plus two blank spaces on each side
fig3a <- ggarrange(
  NULL,
  p_pcoa_treatment,
  NULL,
  ncol = 3,
  nrow = 1,
  widths = c(0.2, 1, 0.2),
  labels = c("", "a", ""),
  font.label = list(size = 19, face = "bold"),
  vjust = 2.25,
  hjust = -0.5
)

# arrange panels b and c together
fig3b <- ggarrange(
  p_glvs,
  p_alkanes,
  ncol = 2,
  nrow = 1,
  labels = c("b", "c"),
  common.legend = TRUE,
  legend = "top",
  font.label = list(size = 19, face = "bold"),
  vjust = 1.75,
  hjust = -0.8
)

# compose final figure by stacking fig3a and fig3b
fig3 <- ggarrange(
  fig3a,
  fig3b,
  ncol = 1,
  nrow = 2,
  heights = c(0.5, 0.45)
)

# save figure
ggsave(
  plot = fig3,
  "figures/paper-figures/fig3.jpeg",
  width = 6.75,
  height = 6.75,
  dpi = fig_dpi
)
