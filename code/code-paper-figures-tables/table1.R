# table with mean and 95% CI of emissions by VOC type and compound

# load shared data objects (e.g., vocs_info, vocs_type) and dependencies
source("code/03-load-data.R")

# keep only the columns needed for the table and order rows
data_vocs <- vocs_info %>%
  arrange(type, compound) %>%
  dplyr::select(type, compound)

# reshape vocs_type to long format:
# - columns from "Short-chain oxygenated VOCs" to last are emissions
# - keep non-missing and positive emissions only
vocs_type_long <- vocs_type %>%
  pivot_longer(
    cols = `Short-chain oxygenated VOCs`:last_col(),
    names_to = "type",
    values_to = "emission"
  ) %>%
  dplyr::select(type, compound, emission) %>%
  drop_na() %>%
  filter(emission > 0)

# summaries by type (mean and 5th/95th percentiles)
vocs_type_summary <- vocs_type_long %>%
  group_by(type) %>%
  summarise(
    mean = mean(emission, na.rm = TRUE),
    q05 = quantile(emission, probs = 0.05, na.rm = TRUE),
    q95 = quantile(emission, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# summaries by compound (mean and 5th/95th percentiles)
vocs_compound_summary <- vocs_type_long %>%
  group_by(compound) %>%
  summarise(
    mean = mean(emission, na.rm = TRUE),
    q05 = quantile(emission, probs = 0.05, na.rm = TRUE),
    q95 = quantile(emission, probs = 0.95, na.rm = TRUE),
    .groups = "drop"
  )

# map each compound to its type to preserve ordering in the table
compound_type_map <- vocs_type_long %>%
  distinct(type, compound)

# join type onto compound summaries and order by type then compound
vocs_compound_summary2 <- vocs_compound_summary %>%
  left_join(compound_type_map, by = "compound") %>%
  arrange(type, compound)

# helper to format mean and interval as a single string
format_stat <- function(mean, q05, q95) {
  sprintf("%.2f (%.2f - %.2f)", mean, q05, q95)
}

# build table rows grouped by type:
# - a bold header row per type
# - followed by indented compound rows for that type
vocs_rows_by_type <- lapply(vocs_type_summary$type, function(t) {
  type_row <- vocs_type_summary %>%
    filter(type == t) %>%
    mutate(
      name = type,
      value = format_stat(mean, q05, q95)
    ) %>%
    dplyr::select(name, value)

  compound_rows <- vocs_compound_summary2 %>%
    filter(type == t) %>%
    mutate(
      name = compound,
      value = format_stat(mean, q05, q95)
    ) %>%
    dplyr::select(name, value)

  bind_rows(type_row, compound_rows)
})

# assemble the final long table and insert a blank spacer column
vocs_table_long <- bind_rows(vocs_rows_by_type) %>%
  mutate(blank = "") %>%
  dplyr::select(name, blank, value)

# table ------------------------------------------------------------------

# build the HTML table with styling, indentation, and bold group headers
table_vocs <- vocs_table_long %>%
  kbl(
    col.names = c("VOCs", "", "Mean emission (95% CI)"),
    align = c("l", "c", "r"),
    escape = FALSE
  ) %>%
  kable_paper(full_width = FALSE, html_font = "Roboto") %>%
  add_indent(c(2, 4, 6:13, 15:18, 20:22)) %>%
  row_spec(c(1, 3, 5, 14, 19), bold = TRUE)

# Save the HTML table
save_kable(
  table_vocs,
  "tables/paper-tables/table1.html",
)

# render HTML to PNG for manuscript inclusion
webshot2::webshot(
  "tables/paper-tables/table1.html",
  file = "tables/paper-tables/table1.png",
  vwidth = 500,
  zoom = 5
)

# remove the temporary HTML file after rendering
file.remove("tables/paper-tables/table1.html")
