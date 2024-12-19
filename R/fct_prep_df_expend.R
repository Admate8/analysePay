
#' Prepare the Expenditure Breakdown Data
#'
#' Combine {country_from}_df_expend and {country_to}_df_expend into a single data.
#' Join the net incomes and calculate the percentage of expenditure of the total
#' income for each percentile.
#'
#' @param df_main df_main(). Reactive.
#'
#' @noRd
get_df_expend <- function(df_main) {

  country_from <- purrr::discard(unique(df_main$country_from), is.na)
  country_to   <- purrr::discard(unique(df_main$country_to), is.na)

  df_expend_from <- base::get(paste0(country_from, "_df_expend")) |>
    dplyr::rename_with(~ paste0(., "_from"), -c(percentile, expenditure)) |>
    dplyr::mutate(country_from = country_from)

  df_expend_to   <- base::get(paste0(country_to, "_df_expend")) |>
    dplyr::rename_with(~ paste0(., "_to"), -c(percentile, expenditure)) |>
    dplyr::mutate(country_to = country_to)

  df_expand_combined <- expand.grid(
    expenditure = as.factor(c(
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
    )),
    percentile = seq(0, 100, 1)
  ) |>
    dplyr::left_join(df_expend_from, by = c("expenditure", "percentile")) |>
    dplyr::left_join(df_expend_to, by = c("expenditure", "percentile")) |>
    dplyr::arrange(expenditure, percentile)

  # Join in the net earnings data
  df_main |>
    dplyr::select(percentile, net_income_from, net_income_to) |>
    dplyr::left_join(df_expand_combined, by = "percentile") |>
    dplyr::mutate(
      actual_values_from_perc       = actual_values_from / net_income_from,
      interpolated_values_from_perc = interpolated_values_from / net_income_from,
      avg_from_perc                 = avg_from / net_income_from,
      actual_values_to_perc         = actual_values_to / net_income_to,
      interpolated_values_to_perc   = interpolated_values_to / net_income_to,
      avg_to_perc                   = avg_to / net_income_to
    ) |>
    dplyr::select(-interpolated_values_from, -interpolated_values_to)
}
