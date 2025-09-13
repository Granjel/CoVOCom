# code to generate figure 2 for the paper

# run workflow? TRUE or FALSE (first time, set to TRUE)
workflow_run <- FALSE

# run config file
source("code/code-paper-figures/config-paper-figures.R")

# figure 2: emitter damage by population ---------------------------------

# tweak panel a's margin
p_receiver_damage <- p_receiver_damage +
  theme(
    text = element_text(size = 12)
  )

# compose
fig4 <- p_receiver_damage # no need to arrange, just one panel

# save figure
ggsave(
  plot = fig4,
  "figures/paper-figures/fig4.png",
  width = 5.25,
  height = 3.75,
  dpi = fig_dpi
)
