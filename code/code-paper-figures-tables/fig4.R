# code to generate figure 4 for the paper

# run necessary scripts
if (!exists("p_receiver_damage", inherits = TRUE)) {
  source("code/05-receiver-damage.R")
}

# figure 4: receiver damage by population --------------------------------

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
  "figures/paper-figures/fig4.jpeg",
  width = 5.25,
  height = 3.75,
  dpi = fig_dpi
)
