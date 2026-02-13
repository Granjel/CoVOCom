# main data
#
# dependencies -----------------------------------------------------------

# load config and packages
source("code/02-config.R")

# data -------------------------------------------------------------------

# define VOCs extraction time (hours)
extraction_time <- 1.5

# VOCs emissions per type -----------------------------------------------

# load information about VOCs (compound names and types)
vocs_info <- read.csv(
  file = "data/vocs-names.csv",
  header = TRUE
) %>%

  # change alpha and beta to unicode greek letters
  mutate(
    compound = gsub("alpha", "\u03B1", compound),
    compound = gsub("beta", "\u03B2", compound),
  )

# soil (background) data -------------------------------------------------

# load soil samples data
soils <- read.csv("data/soil-samples.csv", header = TRUE) %>%
  # transform to ng/h
  mutate(ng = ng / extraction_time) %>%
  # rename a couple of things
  dplyr::rename(id = code, abundance = ng) %>%
  # add compound names from vocs_info
  left_join(
    vocs_info %>%
      dplyr::select(id, compound),
    by = "id"
  ) %>%
  # relocate compound after id
  relocate(compound, .after = id)


# main experiment --------------------------------------------------------

# load general data
df <- read.csv(
  file = "data/arabidopsis.csv",
  header = TRUE
) %>%

  # convert columns to appropriate types
  mutate(
    # rename population levels
    population = dplyr::recode(population, "B" = "Bon", "C" = "Cai"),
    population = factor(population, levels = c("Bon", "Cai")),

    # rename treatment levels
    treatment = dplyr::recode(
      treatment,
      "o" = "Control",
      "h" = "Herbivore-induced"
    ),
    treatment = factor(treatment, levels = c("Control", "Herbivore-induced")),

    # VOCs from ng to ng/h
    across(starts_with("voc"), ~ .x / extraction_time),

    # variables as factor
    code = as.factor(code),
    genotype = as.factor(genotype),

    # variables as numeric
    n = as.numeric(n),
    larva_emitter = as.numeric(larva_emitter),
    size_emitter = as.numeric(size_emitter),
    herbivory_emitter = as.numeric(herbivory_emitter),
    larva_receiver = as.numeric(larva_receiver),
    size_receiver = as.numeric(size_receiver),
    herbivory_receiver = as.numeric(herbivory_receiver),
    voc1 = as.numeric(voc1),
    voc2 = as.numeric(voc2),
    voc3 = as.numeric(voc3),
    voc4 = as.numeric(voc4),
    voc5 = as.numeric(voc5),
    voc6 = as.numeric(voc6),
    voc7 = as.numeric(voc7),
    voc8 = as.numeric(voc8),
    voc9 = as.numeric(voc9),
    voc10 = as.numeric(voc10),
    voc11 = as.numeric(voc11),
    voc12 = as.numeric(voc12),
    voc13 = as.numeric(voc13),
    voc14 = as.numeric(voc14),
    voc15 = as.numeric(voc15),
    voc16 = as.numeric(voc16),
    voc17 = as.numeric(voc17)
  )

# calculate the limit of blanks (lob) for each VOC as the 95% quantile of the
# abundance values in the soil samples for each compound and type
soil_lob <- soils %>%
  dplyr::left_join(
    vocs_info %>% dplyr::select(id, type),
    by = "id"
  ) %>%
  group_by(id, type, compound) %>%
  summarise(
    lob = quantile(abundance, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# take each VOC value in df and, if it is below the lob for that VOC, set it to
# 0; if it's NA, keep it as NA; if it's above the lob, keep the original value
df <- df %>%
  pivot_longer(
    cols = starts_with("voc"),
    names_to = "voc_id",
    values_to = "emission"
  ) %>%
  left_join(
    soil_lob %>% dplyr::select(id, lob),
    by = c("voc_id" = "id")
  ) %>%
  mutate(
    emission = case_when(
      is.na(emission) ~ NA_real_,
      emission < lob ~ 0,
      TRUE ~ emission
    )
  ) %>%
  dplyr::select(-lob) %>%
  pivot_wider(names_from = voc_id, values_from = emission)

# # check how many values were set to 0 for each VOC
# zeros_per_voc <- df %>%
#   pivot_longer(
#     cols = starts_with("voc"),
#     names_to = "voc_id",
#     values_to = "emission"
#   ) %>%
#   group_by(voc_id) %>%
#   summarise(
#     zeros = sum(emission == 0, na.rm = TRUE),
#     total = sum(!is.na(emission)),
#     percent_zeros = zeros / total * 100,
#     .groups = "drop"
#   ) %>%
#   left_join(
#     vocs_info %>% dplyr::select(id, compound),
#     by = c("voc_id" = "id")
#   ) %>%
#   arrange(percent_zeros)

# add a variable, total, that sums the VOCs by row
df <- df %>%
  rowwise() %>%
  mutate(total = sum(c_across(starts_with("voc")), na.rm = TRUE)) %>%
  ungroup() %>%
  relocate(total, .after = last_col())

# VOC emissions per type -------------------------------------------------

# columns of VOC emissions per type
vocs_type <- df %>%
  # keep experimental factors
  dplyr::select(
    code,
    population,
    genotype,
    treatment,
    n,
    larva_emitter,
    size_emitter,
    dplyr::starts_with("voc")
  ) %>%

  # remove rows with any NAs
  drop_na() %>%

  # reshape to long format: one row per VOC per sample
  pivot_longer(
    cols = starts_with("voc"),
    names_to = "voc_id",
    values_to = "emission"
  ) %>%

  # attach type
  dplyr::left_join(vocs_info, by = c("voc_id" = "id")) %>%

  # reshape back to wide format: one column per VOC type
  pivot_wider(names_from = type, values_from = emission)

# Life-history traits ----------------------------------------------------

# load life-history traits data
traits <- read.csv(
  file = "data/traits.csv",
  header = TRUE
) %>%

  # genotype as factor
  mutate(
    genotype = as.factor(genotype)
  ) %>%

  # # average variables across experiments
  # (e.g. the mean of rosettes1 and rosettes2 creates rosettes)
  # mutate(
  #   rosettes = rowMeans(cbind(rosettes1, rosettes2), na.rm = TRUE),
  #   adults = rowMeans(cbind(adults1, adults2), na.rm = TRUE),
  #   survival = rowMeans(cbind(survival1, survival2), na.rm = TRUE),
  #   flower_time = rowMeans(cbind(flower_time1, flower_time2), na.rm = TRUE),
  #   fruits = rowMeans(cbind(fruits1, fruits2), na.rm = TRUE),
  #   seeds = rowMeans(cbind(seeds1, seeds2), na.rm = TRUE),
  #   fitness = rowMeans(cbind(fitness1, fitness2), na.rm = TRUE)
  # ) %>%

  # select only greenhouse_flower_time, seed_weight, seed_germination as traits
  dplyr::select(
    population,
    genotype,
    greenhouse_flower_time,
    seed_weight,
    seed_germination
  )
