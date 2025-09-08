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
    voc17 = as.numeric(voc17),
    total = as.numeric(total)
  )
