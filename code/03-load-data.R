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
    voc1 = voc1 / extraction_time,
    voc2 = voc2 / extraction_time,
    voc3 = voc3 / extraction_time,
    voc4 = voc4 / extraction_time,
    voc5 = voc5 / extraction_time,
    voc6 = voc6 / extraction_time,
    voc7 = voc7 / extraction_time,
    voc8 = voc8 / extraction_time,
    voc9 = voc9 / extraction_time,
    voc11 = voc11 / extraction_time,
    voc10 = voc10 / extraction_time,
    voc12 = voc12 / extraction_time,
    voc13 = voc13 / extraction_time,
    voc14 = voc14 / extraction_time,
    voc15 = voc15 / extraction_time,
    voc16 = voc16 / extraction_time,
    voc17 = voc17 / extraction_time,

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

# in df, divide each VOC by the mean of the corresponding soil VOCs
epsilon <- 0 # small constant to avoid division by zero; not needed since we have no zero values

for (i in seq_len(nrow(vocs_info))) {
  voc_id <- vocs_info$id[i]

  # get soil values for this VOC
  soil_values <- soils$abundance[soils$id == voc_id]

  # if no blank value exists or all are NA, treat background as zero
  if (length(soil_values) == 0 || all(is.na(soil_values))) {
    soil_mean <- 0
  } else {
    soil_mean <- mean(soil_values, na.rm = TRUE)
  }

  print(soil_mean)

  # always normalize, using epsilon for stability
  df[[voc_id]] <- df[[voc_id]] / (soil_mean + epsilon)
}

# add total VOC emissions (sum of all VOCs) as a new column
df <- df %>% mutate(total = as.numeric(rowSums(across(voc1:voc17))))

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

  # average variables across experiments (e.g. the mean of rosettes1 and rosettes2 creates rosettes)
  mutate(
    rosettes = rowMeans(cbind(rosettes1, rosettes2), na.rm = TRUE),
    adults = rowMeans(cbind(adults1, adults2), na.rm = TRUE),
    survival = rowMeans(cbind(survival1, survival2), na.rm = TRUE),
    flower_time = rowMeans(cbind(flower_time1, flower_time2), na.rm = TRUE),
    fruits = rowMeans(cbind(fruits1, fruits2), na.rm = TRUE),
    seeds = rowMeans(cbind(seeds1, seeds2), na.rm = TRUE),
    fitness = rowMeans(cbind(fitness1, fitness2), na.rm = TRUE)
  )
