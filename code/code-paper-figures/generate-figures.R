# generate all figures for the paper -------------------------------------

# create directory for paper figures if it doesn't exist
if (!dir.exists("figures/paper-figures")) {
  dir.create("figures/paper-figures", recursive = TRUE)
}

# run workflow to generate all figures
source("code/code-paper-figures/fig1.R")
source("code/code-paper-figures/fig2.R")
source("code/code-paper-figures/fig3.R")
source("code/code-paper-figures/fig4.R")
