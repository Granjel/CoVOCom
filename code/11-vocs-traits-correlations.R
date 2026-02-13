# correlations between VOCs and life history traits

# create dir for traits --------------------------------------------------
if (!dir.exists("tables/traits-correlations")) {
  dir.create("tables/traits-correlations")
}

# load packages and data -------------------------------------------------
source("code/03-load-data.R")

# select relevant data from VOCs
vocs <- df %>%
  dplyr::select(
    population,
    genotype,
    treatment,
    contains("voc"),
    total
  ) %>%

  # average: one value per population x genotype x treatment
  group_by(population, genotype, treatment) %>%
  summarise(across(voc1:total, \(x) mean(x, na.rm = TRUE))) %>%
  ungroup() %>%

  # arrange by treatment, population, genotype
  arrange(treatment, population, genotype)

# join VOCs with life-history traits
vocs_traits <- vocs %>%
  dplyr::left_join(
    traits,
    by = c("population", "genotype")
  )

# function to calculate correlations -------------------------------------

# define function
run_cor <- function(df, voc, trait) {
  x <- df[[voc]] # VOCs names
  y <- df[[trait]] # traits names

  # scale and remove NAs
  keep <- complete.cases(x, y)
  x <- scale(x[keep])
  y <- scale(y[keep])

  # Pearson correlation test
  test <- cor.test(x, y, method = "pearson")

  # save results
  tibble(
    voc = voc,
    trait = trait,
    r = unname(test$estimate),
    p = test$p.value,
    n = sum(keep)
  )
}

# calculate correlations -------------------------------------------------

# define traits
trait_cols <- traits %>%
  dplyr::select(greenhouse_flower_time:last_col()) %>%
  names

# define VOCs
voc_cols <- vocs %>%
  dplyr::select(voc1:total) %>%
  names

# run
cor_results <- vocs_traits %>%
  group_by(population, treatment) %>%
  group_modify(
    ~ {
      expand_grid(
        voc = voc_cols,
        trait = trait_cols
      ) %>%
        purrr::pmap_dfr(function(voc, trait) {
          run_cor(.x, voc, trait)
        })
    }
  ) %>%
  ungroup()

# find how many significant correlations there are
# per population x treatment x trait and add average r
summary_cor_results <- cor_results %>%
  group_by(population, treatment, trait) %>%
  summarise(
    mean_r = mean(r, na.rm = TRUE),
    n_significant = sum(p < 0.05),
    .groups = "drop"
  ) %>%
  arrange(-n_significant)

# export summary table
write.csv(
  summary_cor_results,
  "tables/traits-correlations/summary-vocs-traits-correlations.csv",
  row.names = FALSE
)

# filter by traits of interest and add stars:
# * for p < 0.05, ** for p < 0.01, *** for p < 0.001
cor_results_stars <- cor_results %>%
  mutate(
    significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      TRUE ~ ""
    ),
    voc = factor(voc, levels = c(paste0("voc", 1:17), "total"))
  ) %>%
  arrange(population, trait, treatment, voc)

# export full correlations table
write.csv(
  cor_results_stars,
  "tables/traits-correlations/full-vocs-traits-correlations.csv",
  row.names = FALSE
)

# select and export result tables ----------------------------------------

# select traits with at least some significant correlations
main_traits <- summary_cor_results %>%
  filter(n_significant > 0) %>% # comment to get all traits
  dplyr::pull(trait) %>%
  unique()

# export tables for each main trait
for (tname in main_traits) {
  cor_trait <- cor_results_stars %>%
    filter(trait == tname) %>%
    mutate(cell = sprintf("%.2f %s", r, significance)) %>%
    dplyr::select(population, treatment, voc, trait, cell) %>%
    unite(col = "col", population, treatment, sep = "_") %>%
    pivot_wider(names_from = col, values_from = cell) %>%
    mutate(voc = factor(voc, levels = c(paste0("voc", 1:17), "total"))) %>%
    dplyr::select(-trait) %>%
    arrange(voc)

  # export to CSV
  fname <- file.path(
    "tables/traits-correlations",
    paste0("vocs-traits-correlations-", tname, ".csv")
  )
  write.csv(cor_trait, fname, row.names = FALSE)
}
