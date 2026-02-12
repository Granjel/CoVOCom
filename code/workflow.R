# workflow

# run all analysis scripts in order
source("code/04-emitter-damage.R")
source("code/05-receiver-damage.R")
source("code/06-vocs-total.R")
source("code/07-vocs-permanova.R")
source("code/08-vocs-pcoa-population.R")
source("code/09-vocs-pcoa-treatment.R")
source("code/10-vocs-10-vocs-by-type.R")
source("code/11-vocs-traits-correlations.R")
source("code/12-soil-samples.R")

# generate all figures and tables for the paper; uncomment to run
# source("code/code-paper-figures/fig1.R")
# source("code/code-paper-figures/fig2.R")
# source("code/code-paper-figures/fig3.R")
# source("code/code-paper-figures/fig4.R")
# source("code/code-paper-figures/table1.R")
