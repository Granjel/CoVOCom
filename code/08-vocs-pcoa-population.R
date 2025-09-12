# VOCs Permanova

# load data and variables ------------------------------------------------

# run permanova script, which loads data and variables
source("code/07-vocs-permanova.R")

# PCoA -------------------------------------------------------------------

# use capscale to do PCoA
vocs_pcoa_population <- capscale(
  compounds_bray ~ 1 + Condition(vocs$treatment),
  data = vocs,
  comm = vocs %>% dplyr::select(voc1:voc17)
)

# extract eigenvalues
eigenvalues_population <- eigenvals(vocs_pcoa_population)

# site ordination scores for the first two axes
ordination_scores_population <-
  data.frame(
    id = rownames(vocs_pcoa_population$CA$u),
    treatment = vocs$population,
    plant = vocs$code,
    MDS1 = scores(vocs_pcoa_population, scaling = 3, correlation = TRUE)$sites[,
      1
    ],
    MDS2 = scores(vocs_pcoa_population, scaling = 3, correlation = TRUE)$sites[,
      2
    ]
  )
