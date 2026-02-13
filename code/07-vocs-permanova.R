# VOCs Permanova

# switch: run only "All" (all VOC types together) by default,
# or set to TRUE to run all VOC types separately + "All"
run_all_types <- FALSE

# create dirs for permanova ----------------------------------------------

# create directory for PERMANOVA results
if (!dir.exists("tables/permanova")) {
  dir.create("tables/permanova")
}

# create directory for combined PERMANOVA objects
if (!dir.exists("tables/permanova/objects-for-pcoa")) {
  dir.create("tables/permanova/objects-for-pcoa")
}

# load packages and data -------------------------------------------------
source("code/03-load-data.R")

# helper to make safe filenames
# (lowercase, non-alphanumerics -> hyphens, trim edge hyphens)
safe_name <- function(x) {
  gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(x)))
}

# prepare list of types (each VOC type plus an "All" aggregate)
types <- if (isTRUE(run_all_types)) {
  c(unique(vocs_info$type), "All")
} else {
  "All"
}

# lists to store results for each type
results <- list()
subsets <- list()
distances <- list()

# perform permanova for each type of VOCs and save results
for (t in types) {
  message(sprintf("Running PERMANOVA with VOC type: %s", t))

  # choose VOC ids for this type (or all ids when t == "All")
  ids <- if (t == "All") {
    unique(vocs_info$id)
  } else {
    vocs_info %>% filter(type == t) %>% pull(id) %>% unique()
  }
  if (length(ids) == 0) {
    next
  }

  # subset data for the type of VOCs:
  # - keep sample metadata and the selected VOC columns
  # - create a population:genotype strata column for permutations
  # - drop rows with any NA
  # - keep only samples with > 0 total emission
  #   (otherwise distance matrix will fail)
  sub <- df %>%
    dplyr::select(code, population, genotype, treatment, n, all_of(ids)) %>%
    mutate(pop_gen = interaction(population, genotype, sep = "_")) %>%
    drop_na() %>%
    rowwise() %>%
    mutate(total_emission = sum(c_across(starts_with("voc")), na.rm = TRUE)) %>%
    ungroup() %>%
    filter(total_emission > 0)

  # store subset for downstream analyses (e.g., PCoA)
  subsets[[safe_name(t)]] <- sub

  # need at least two samples to compute distances / permanova
  if (nrow(sub) < 2) {
    next
  }
  # ensure we have VOC columns to compute distances
  if (ncol(dplyr::select(sub, starts_with("voc"))) == 0) {
    next
  }

  # compute Bray-Curtis distance matrix on VOC columns
  dist <- vegan::vegdist(
    dplyr::select(sub, starts_with("voc")),
    method = "bray"
  )

  # store distance matrix for this VOC type
  distances[[safe_name(t)]] <- dist

  # run PERMANOVA with treatment, population and their interaction
  # stratify permutations by pop:genotype to respect group structure
  res <- vegan::adonis2(
    dist ~ treatment * population,
    data = sub,
    permutations = 10000,
    strata = sub$pop_gen,
    by = "terms"
  ) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("factor")

  # save results table for this VOC type
  write.csv(
    res,
    file.path(
      "tables/permanova",
      paste0("permanova-vocs-", safe_name(t), ".csv")
    ),
    row.names = FALSE
  )

  # store result in list (keyed by type)
  results[[t]] <- res
}

# save all subsets and distance matrices together
saveRDS(
  list(subsets = subsets, distances = distances),
  file.path(
    "tables/permanova/objects-for-pcoa",
    "vocs-subsets-and-distances.rds"
  )
)

# clean environment completely
rm(list = ls())

# # in df, count how many rows have VOC values that are all zero
# (i.e., total emission = 0); by treatment and population
# df %>%
#   drop_na() %>%
#   rowwise() %>%
#   mutate(total_emission = sum(c_across(starts_with("voc")), na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(total_emission == 0) %>%
#   count(treatment, population, genotype) %>%
#   arrange(treatment, population)
