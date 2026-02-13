# table of trait correlations with VOC emissions

# load shared data objects (e.g., vocs_info) and dependencies
source("code/03-load-data.R")

# read correlations for seed germination
data <- read.csv(
  "tables/traits-correlations/vocs-traits-correlations-seed_germination.csv"
) %>%
  relocate(
    voc,
    Bon_Control,
    Cai_Control,
    Bon_Herbivore.induced,
    Cai_Herbivore.induced
  )

# replace VOC IDs with compound names and label the total VOCs row
data <- data %>%
  dplyr::left_join(
    vocs_info %>%
      dplyr::select(id, compound),
    by = c("voc" = "id")
  ) %>%
  dplyr::select(-voc) %>%
  dplyr::relocate(compound) %>%
  # The NA in compound corresponds to the total of all VOCs
  dplyr::mutate(
    compound = ifelse(is.na(compound), "<b>Total VOCs</b>", compound)
  )

# create desired compound order from vocs_info
compound_order <- vocs_info %>%
  arrange(type, compound) %>%
  pull(compound)

# add total VOCs at the end
compound_order <- c(compound_order, "<b>Total VOCs</b>")

# apply ordering to the correlation table
data <- data %>%
  mutate(
    compound = factor(compound, levels = compound_order)
  ) %>%
  arrange(compound) %>%
  mutate(
    compound = as.character(compound)
  )

# append correlations for greenhouse flowering time (drop duplicate voc column)
data <- data %>%
  cbind(
    rep(" ", nrow(data)),
    rep(" ", nrow(data)),
    rep(" ", nrow(data)),
    read.csv(
      "tables/traits-correlations/vocs-traits-correlations-greenhouse_flower_time.csv"
    ) %>%
      dplyr::select(-voc) %>%
      relocate(
        Bon_Control,
        Cai_Control,
        Bon_Herbivore.induced,
        Cai_Herbivore.induced
      )
  )

# build the HTML table with grouped headers and formatting
table_traits <- data %>%
  kbl(
    col.names = c(
      "VOCs",
      rep(c("Bon", "Cai"), 2),
      " ",
      " ",
      " ",
      rep(c("Bon", "Cai"), 2)
    ),
    align = "lccccccccccc",
    escape = FALSE
  ) %>%
  kable_paper(full_width = TRUE, html_font = "Roboto") %>%
  add_header_above(c(
    " " = 1,
    "Control" = 2,
    "Herbivore-induced" = 2,
    " " = 1,
    " " = 1,
    " " = 1,
    "Control" = 2,
    "Herbivore-induced" = 2
  )) %>%
  add_header_above(c(
    " " = 1,
    "Germination rate" = 4,
    " " = 1,
    " " = 1,
    " " = 1,
    "Flowering time" = 4
  )) %>%
  pack_rows("Benzoates", 1, 1) %>%
  pack_rows("Esters", 2, 2) %>%
  pack_rows("Long-chain aldehydes and alkanes", 3, 10) %>%
  pack_rows("Short-chain oxygenated VOCs", 11, 14) %>%
  pack_rows("Terpenoids", 15, 17) %>%
  pack_rows("", 18, 18, indent = FALSE)

# save table as HTML
save_kable(
  table_traits,
  "tables/paper-tables/table2.html",
)

# render HTML to PNG for inclusion in the paper
webshot2::webshot(
  "tables/paper-tables/table2.html",
  file = "tables/paper-tables/table2.png",
  vwidth = 900,
  zoom = 5
)

# remove HTML file after rendering
file.remove("tables/paper-tables/table2.html")
