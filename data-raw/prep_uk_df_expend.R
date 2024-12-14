# Note ----
# This script must be checked whenever the source data is updated because its
# shape might change, breaking the pipeline below. Sadly, ONS does not provide
# this data via API, and so until it does, one must rely on data extracts from
# their website...

# Data ----
# ONS (UK) publishes the expenditure per household by gross income decile in FY
# Link: https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/expenditure/datasets/familyspendingworkbook2expenditurebyincome

# Aim ----
# Compare average expenditure per person per year by gross income decile

# Issues ----
# 1. UK data contains expend per household
## Solution: Use the weighted average provided to estimate the expend per person
# 2. UK data is in financial years
## Solution: Nothing. Use FY as a proxy for CY
# 3. UK data lacks estimates of Heath and Education apart from the average
## Solution: Use the average to populate other deciles.
# 4. UK data includes non-adults
## Solution: use the total number of adults over all persons as a proxy for
## scaling expenditure



uk_df_expend_raw <- openxlsx::read.xlsx(here::here("./data-raw/uk_df_expend_raw.xlsx"), sheet = "A4")

# Select the weighted average number of Persons Per Deciles (PPD)
uk_ppd <- uk_df_expend_raw |>
  dplyr::slice(3, 8, 10) |>
  janitor::remove_empty(c("rows", "cols")) |>
  t() |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::slice(-1) |>
  dplyr::mutate(
    weighted_val = as.numeric(x3) / as.numeric(x2),
    deciles      = c(seq(10, 100, 10), "average")
  ) |>
  dplyr::select(link = x1, deciles, weighted_val)

# Clean the data
uk_df_expend <- uk_df_expend_raw |>
  dplyr::slice(3, 13:24, 26) |>
  dplyr::select(-1)
uk_df_expend[1, 1] <- "expenditure"
uk_df_expend <- uk_df_expend |>
  janitor::row_to_names(1) |>
  dplyr::mutate(
    expenditure = iconv(expenditure, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
    expenditure = stringr::str_remove_all(expenditure, "\\^1")
  ) |>
  tidyr::pivot_longer(cols = -expenditure, names_to = "link") |>
  dplyr::mutate(value = as.numeric(value)) |>
  # Issue 4:
  dplyr::group_by(expenditure) |>
  dplyr::mutate(value = ifelse(is.na(value), value[link == "All"], value))

# Compute the average expenditure per person per year
uk_df_expend <- uk_df_expend |>
  dplyr::left_join(uk_ppd, by = "link") |>
  dplyr::mutate(value = 52.1429 * value / weighted_val) |>
  dplyr::select(-link, -weighted_val)

# Compute the change in expenditure in respect to the total average
uk_df_expend <- dplyr::left_join(
  uk_df_expend |> dplyr::filter(deciles != "average"),
  uk_df_expend |> dplyr::filter(deciles == "average") |> dplyr::select(-deciles),
  by     = "expenditure",
  suffix = c("", "_avg")
) |>
  dplyr::mutate(perc_of_avg = value / value_avg) |>
  dplyr::group_by(deciles) |>
  dplyr::mutate(decile_perc = value / sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(deciles = as.integer(deciles)) |>
  dplyr::select(deciles, expenditure, value, value_avg, decile_perc, perc_of_avg)


# Save the data
usethis::use_data(uk_df_expend, overwrite = TRUE)
