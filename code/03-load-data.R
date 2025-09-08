# main data

# dependencies -----------------------------------------------------------

# load config and packages
source("code/02-config.R")

# data -------------------------------------------------------------------

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
    voc1 = voc1 / 2,
    voc2 = voc2 / 2,
    voc3 = voc3 / 2,
    voc4 = voc4 / 2,
    voc5 = voc5 / 2,
    voc6 = voc6 / 2,
    voc7 = voc7 / 2,
    voc8 = voc8 / 2,
    voc9 = voc9 / 2,
    voc10 = voc10 / 2,
    voc11 = voc11 / 2,
    voc12 = voc12 / 2,
    voc13 = voc13 / 2,
    voc14 = voc14 / 2,
    voc15 = voc15 / 2,
    voc16 = voc16 / 2,
    voc17 = voc17 / 2,
    total = rowSums(across(voc1:voc17)) # total VOC emissions
  )
