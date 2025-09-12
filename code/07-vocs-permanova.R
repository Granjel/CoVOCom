# VOCs Permanova

# load packages and data -------------------------------------------------

# load
source("code/03-load-data.R")

# select only relevant data
vocs <- df %>%
  # select relevant columns
  dplyr::select(
    code,
    population,
    genotype,
    treatment,
    n,
    dplyr::starts_with("voc")
  ) %>%
  # add interaction between population and genotype for Permanova
  mutate(pop_gen = interaction(population, genotype, sep = "_")) %>%
  # remove NAs
  drop_na()

# calculate bray curtis distances for the VOC compounds
compounds_bray <- vegan::vegdist(
  # subset only the VOCs columns
  vocs %>% dplyr::select(voc1:voc17),
  method = "bray"
)

# Permanova --------------------------------------------------------------

# if `vocs_permanova` already exists in the environment, skip this step
if (exists("vocs_permanova")) {
  message("Data loaded; Permanova skipped")
} else {
  message("Data loaded; running Permanova...")

  # run Permanova
  vocs_permanova <- vegan::adonis2(
    formula = compounds_bray ~ treatment * population,
    data = vocs,
    permutations = 10000,
    strata = vocs$pop_gen,
    by = "terms"
  )

  # create table with Permanova ouput...
  vocs_permanova_table <- as.data.frame(vocs_permanova) %>%
    rownames_to_column(var = "factor")

  # ...and save it
  write.csv(
    vocs_permanova_table,
    "tables/permanova-vocs.csv",
    row.names = FALSE
  )
  message("Permanova ran!")
}
