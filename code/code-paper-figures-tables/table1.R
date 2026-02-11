# load shared data objects (e.g., vocs_info) and dependencies
source("code/03-load-data.R")

# read correlations for seed germination
data <- read.csv(
  "tables/traits-correlations/vocs-traits-correlations-seed_germination.csv"
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

# append correlations for greenhouse flowering time (drop duplicate voc column)
data <- data %>%
  cbind(
    read.csv(
      "tables/traits-correlations/vocs-traits-correlations-greenhouse_flower_time.csv"
    ) %>%
      dplyr::select(-voc)
  )

# build the HTML table with grouped headers and formatting
table_traits <- data %>%
  kbl(
    col.names = c(
      " ",
      rep(c("Control", "Herbivory-induced"), 4)
    ),
    align = "lcccccccc",
    escape = FALSE
  ) %>%
  kable_paper(full_width = TRUE, html_font = "Roboto") %>%
  add_header_above(c(
    " " = 1,
    "Bon" = 2,
    "Cai" = 2,
    "Bon" = 2,
    "Cai" = 2
  )) %>%
  add_header_above(c(
    " " = 1,
    "Germination rate" = 4,
    "Flowering time" = 4
  )) %>%
  pack_rows("Individual VOCs", 1, 17)

# save table as HTML
save_kable(
  table_traits,
  "tables/paper-tables/table1.html",
)

# render HTML to PNG for inclusion in the paper
webshot2::webshot(
  "tables/paper-tables/table1.html",
  file = "tables/paper-tables/table1.png",
  vwidth = 1100,
  zoom = 5
)

# remove HTML file after rendering
file.remove("tables/paper-tables/table1.html")
