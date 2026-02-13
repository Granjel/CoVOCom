# workflow

# run all analysis scripts in order
source("code/04-emitter-damage.R")
source("code/05-receiver-damage.R")
source("code/06-vocs-total.R")
source("code/07-vocs-permanova.R")
source("code/08-vocs-pcoa-population.R")
source("code/09-vocs-pcoa-treatment.R")
source("code/10-vocs-by-type.R")
source("code/11-vocs-traits-correlations.R")
source("code/12-soil-samples.R")

# generate all figures and tables for the paper
source("code/code-paper-figures-tables/fig1.R")
source("code/code-paper-figures-tables/fig2.R")
source("code/code-paper-figures-tables/fig3.R")
source("code/code-paper-figures-tables/fig4.R")
source("code/code-paper-figures-tables/table1.R")
source("code/code-paper-figures-tables/table2.R")
