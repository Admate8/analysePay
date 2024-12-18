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


# Getting values for each expenditure category per adult per percentile:

# 1. Calculate the weighted average number of adults per household: #adults / #households
# 2. Compute the average expenditure per person per year:
# [52.1429 (average #weeks in a year) * value per week] / result from 1.
# 3. Interpolate decile values per expenditure to get values for missing percentiles.
# 4. Find the percentile scaling factor for each expenditure:
# scaling factor for percentile "n" in "x" category =
## (total avg spend on x) / (interpolated value of n in x)
## The scaling factor will be used to scale the values for which percentiles are
## missing in other countries, unless they also publish breakdown by deciles
## (not just the average, which is presumably most common).

uk_df_expend_raw <- openxlsx::read.xlsx(here::here("./data-raw/uk_df_expend_raw.xlsx"), sheet = "A4")

# 1. Select the weighted average number of Persons Per Deciles (PPD)
uk_ppd <- uk_df_expend_raw |>
  dplyr::slice(3, 8, 10) |>
  janitor::remove_empty(c("rows", "cols")) |>
  t() |>
  tibble::as_tibble() |>
  janitor::clean_names() |>
  dplyr::slice(-1) |>
  dplyr::mutate(
    weighted_val = as.numeric(x3) / as.numeric(x2),
    decile       = c(seq(10, 100, 10), "average")
  ) |>
  dplyr::select(link = x1, decile, weighted_val)

# Clean the data
uk_df_expend_prep <- uk_df_expend_raw |>
  dplyr::slice(3, 13:24, 26) |>
  dplyr::select(-1)
uk_df_expend_prep[1, 1] <- "expenditure"
uk_df_expend_prep <- uk_df_expend_prep |>
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

# 2. Compute the average expenditure per person per year
uk_df_expend_prep <- uk_df_expend_prep |>
  dplyr::left_join(uk_ppd, by = "link") |>
  dplyr::mutate(value  = 52.1429 * value / weighted_val) |>
  dplyr::select(-link, -weighted_val)

# 3. Interpolate decile values per expenditures to get values for percentiles
uk_df_expend_interpolated <- uk_df_expend_prep |>
  dplyr::filter(decile != "average") |>
  dplyr::mutate(decile = as.numeric(decile)) |>
  dplyr::group_by(expenditure) |>
  dplyr::group_split() |>
  purrr::map( ~ {
    expenditure       <- unique(.x$expenditure)
    df_to_interpolate <- .x |> dplyr::select(-expenditure)
    interpolated_vals <- fit_percentile_distribution(df_to_interpolate)
    interpolated_vals |> dplyr::mutate(expenditure = expenditure)
  }) |>
  dplyr::bind_rows() |>
  dplyr::relocate(expenditure, .before = percentile)

# 4. Find the percentile scaling factor for each expenditure
uk_df_expend <- dplyr::left_join(
  uk_df_expend_interpolated,
  uk_df_expend_prep |>
    dplyr::filter(decile == "average") |>
    dplyr::select(-decile) |>
    dplyr::rename(avg = value),
  by     = "expenditure"
) |>
  dplyr::mutate(
    scaling_factor = avg / interpolated_values,
    # Impose order
    expenditure = factor(expenditure, levels = c(
      "Food & non-alcoholic drinks",
      "Alcoholic drinks, tobacco & narcotics",
      "Clothing & footwear",
      "Housing (net), fuel & power",
      "Household goods & services",
      "Health",
      "Transport",
      "Communication",
      "Recreation & culture",
      "Education",
      "Restaurants & hotels",
      "Miscellaneous goods & services",
      "Other expenditure items"
    ))
  ) |>
  dplyr::arrange(expenditure)


# Save the data
usethis::use_data(uk_df_expend, overwrite = TRUE)
