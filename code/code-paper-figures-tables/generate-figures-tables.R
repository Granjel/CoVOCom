# generate all figures for the paper -------------------------------------

# create directory for paper figures if it doesn't exist
if (!dir.exists("figures/paper-figures")) {
  dir.create("figures/paper-figures", recursive = TRUE)
}

# create directory for paper tables if it doesn't exist
if (!dir.exists("tables/paper-tables")) {
  dir.create("tables/paper-tables", recursive = TRUE)
}

# run workflow to generate all figures
source("code/code-paper-figures-tables/fig1.R")
source("code/code-paper-figures-tables/fig2.R")
source("code/code-paper-figures-tables/fig3.R")
source("code/code-paper-figures-tables/fig4.R")
source("code/code-paper-figures-tables/table1.R")
