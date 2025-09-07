# VOCs Permanova

# load packages and data -------------------------------------------------

# load packages
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
  # replace NA with 0
  dplyr::mutate(across(starts_with("voc"), ~ replace_na(., 0)))

# define names of VOCs
voc_names <-
  data.frame(
    id = paste0("voc", 1:17),
    compound = c(
      "2-butenal, 2-methyl-, (E)-",
      "alpha-pinene",
      "beta-pinene",
      "2-heptanone, 5-methyl",
      "5-hepten-2-one, 6-methyl-",
      "butanoic acid, butyl ester",
      "1-hexanol, 2-ethyl-",
      "acetophenone",
      "nonanal",
      "cis-2-nonenal",
      "dodecane",
      "decanal",
      "tridecane",
      "tetradecane",
      "dodecanal",
      "trans-geranylaceone",
      "tetradecanal"
    )
  )

write.csv(
  voc_names,
  "data/voc-names.csv",
  row.names = FALSE
)
