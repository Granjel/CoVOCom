# workflow

# run all analysis scripts in order
source("code/04-emitter-damage.R")
source("code/05-receiver-damage.R")
source("code/06-vocs-total.R")
source("code/07-vocs-permanova.R")
source("code/08-vocs-pcoa-population.R")
source("code/09-vocs-pcoa-treatment.R")
# fix when Lucía confirms the final VOC classification with Greg
# source("code/10-alcohols-esters.R")
# source("code/11-glvs.R")
# source("code/12-ketones.R")
# source("code/13-long-chain-alkanes.R")
# source("code/14-terpenes.R")
source("code/15-vocs-traits-correlations.R")
source("code/16-soil-samples.R")

# generate all figures for the paper; uncomment to run
# source("code/code-paper-figures/fig1.R")
# source("code/code-paper-figures/fig2.R")
# source("code/code-paper-figures/fig3.R")
# source("code/code-paper-figures/fig4.R")
