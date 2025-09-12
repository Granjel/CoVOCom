# workflow

# run all analysis scripts in order
source("code/04-emitter-damage.R")
source("code/05-receiver-damage.R")
source("code/06-vocs-total.R")
source("code/07-vocs-permanova.R")
source("code/08-vocs-pcoa-population.R")
source("code/09-vocs-pcoa-treatment.R")
