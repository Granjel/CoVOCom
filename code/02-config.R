# config

# dependencies -----------------------------------------------------------

# load packages
source("code/01-load-packages.R")

# set seed for reproducibility -------------------------------------------

set.seed(42)

# plot config ------------------------------------------------------------

# define theme for plots
theme_set(theme_classic())

# colorblind friendly palette: 2 colors for populations
pal_pop <- met.brewer("OKeeffe1")[c(4, 8)]
names(pal_pop) <- c("Bon", "Cai")

# colorblind friendly palette: 2 colors for treatments
pal_treat <- moma.colors("Avedon")[c(6, 9)]
names(pal_treat) <- c(
  "Control",
  "Herbivore-induced"
)

# figure dimensions & resolution
fig_width <- 6.5 # width in inches
fig_height <- 4.5 # height in inches
fig_dpi <- 640 # resolution in dpi
