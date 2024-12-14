#' Fit a Spline to Available Deciles to Find In-between Values
#'
#' @param available_deciles Named vector/list with deciles. The names should be
#' of the form *decile*th, e.g., *10th*, *20th*, etc. See examples.
#'
#' @return Tibble with actual deciles and interpolated values.
#' @examples
#' \dontrun{
#' decile_vector <- c("10th" = 200, "20th" = 400, "50th" = 800)
#' fit_earnings_dist(decile_vector)
#' }
fit_earnings_dist <- function(available_deciles) {
  stopifnot(
    "Check the names of the supplied vector" = all(grepl("^([0-9]{1,2}|100)th$", names(available_deciles))),
    "Deciles must be numerics" = all(is.numeric(as.numeric(gsub("th", "", names(available_deciles))))),
    "Deciles must be positive" = all(as.numeric(gsub("th", "", names(available_deciles))) > 0),
    "Deciles must be smaller than 100" = all(as.numeric(gsub("th", "", names(available_deciles))) < 100),
    "Values must be numeric" = all(is.numeric(available_deciles)),
    "Values must be positive" = available_deciles > 0
  )

  earning_deciles        <- unlist(available_deciles)
  names(earning_deciles) <- as.numeric(gsub("th", "", names(earning_deciles)))
  percentiles            <- seq(
    as.numeric(min(names(earning_deciles))),
    as.numeric(max(names(earning_deciles))),
    by = 1
  )

  # Create a vector of interpolated values using spline method
  spline_values <- stats::spline(
    x = names(earning_deciles),
    y = earning_deciles,
    xout = percentiles
  )

  # Create a vector of actual deciles with interpolated values equal to NA
  actual_values <- rep(NA, length(percentiles))
  for (i in seq_along(earning_deciles)) {
    match_index <- which(percentiles == names(earning_deciles)[i])
    if (length(match_index) > 0) {
      actual_values[match_index] <- earning_deciles[i]
    }
  }

  # Return clean data
  data.frame(
    deciles             = spline_values$x,
    actual_values       = actual_values,
    interpolated_values = spline_values$y
  )
}


#' Generate Join Earnings Distribution & Deductions Data
#'
#' @param settings_from Country from settings.
#' @param settings_to Country to settings.
#'
#' @return Join earnings distribution tibble with deciles and deduction breakdown.
#' This is be the main data in the app that will rely on the user input and update
#' on the button click. This function will also return a helper legend-table.
#'
#' @examples \dontrun{
#' get_df_earnings_dist(uk_settings, pl_settings)$df_main
#' get_df_earnings_dist(uk_settings, pl_settings)$df_cat_table
#' }
get_df_earnings_dist <- function(
    settings_from = uk_settings,
    settings_to   = pl_settings
) {

  country_from <- settings_from$global$short_cut
  country_to   <- settings_to$global$short_cut

  # Get defaults
  deciles_from <- unlist(settings_from$earning_deciles)
  deciles_to   <- unlist(settings_to$earning_deciles)

  # Generate distribution from available earning deciles
  earnings_dist_from <- fit_earnings_dist(deciles_from)
  earnings_dist_to   <- fit_earnings_dist(deciles_to)

  # Combine the data into a single tibble
  df_earnings_dist <- dplyr::left_join(
    earnings_dist_from,
    earnings_dist_to,
    by     = "deciles",
    suffix = c("_from", "_to")
  )


  # Calculate deduction breakdown on the interpolated distributional values
  ## Call the appropriate `calc_{country}_deductions` function
  fct_deduction_from_name <- paste0("calc_", country_from, "_deductions")
  fct_deduction_to_name   <- paste0("calc_", country_to, "_deductions")

  ## Calculate deduction breakdowns of the fitted values
  df_fit_deduction_from <- base::get(fct_deduction_from_name)(
    annual_earnings = df_earnings_dist$interpolated_values_from,
    alpha_scheme    = settings_from$pension$alpha_scheme,
    standard_tax    = settings_from$tax$standard_tax,
    settings        = settings_from
  )
  df_fit_deduction_to <- base::get(fct_deduction_to_name)(
    annual_earnings = df_earnings_dist$interpolated_values_to,
    alpha_scheme    = settings_to$pension$alpha_scheme,
    standard_tax    = settings_to$tax$standard_tax,
    settings        = settings_to
  )

  ### Create Categories Table
  df_categories <- dplyr::left_join(
    df_fit_deduction_from$df_deductions_split,
    df_fit_deduction_to$df_deductions_split,
    by = "split"
  )
  if (country_from == country_to) {
    names(df_categories)[2:3] <- c(
      paste(settings_from$global$full_name, "(1)"),
      paste(settings_to$global$full_name, "(2)")
    )
  } else {
    names(df_categories)[2:3] <- c(
      settings_from$global$full_name,
      settings_to$global$full_name
    )
  }

  # Combine all deductions into a single data set - this is the main data for
  # the app after choosing all settings
  df <- dplyr::left_join(

    df_earnings_dist |>
      dplyr::select(deciles, actuals = actual_values_from) |>
      cbind(df_fit_deduction_from$df_deductions) |>
      dplyr::mutate(actuals = ifelse(is.na(actuals), 0, 1)) |>
      dplyr::rename_with(~ paste0(., "_from"), -deciles) |>
      dplyr::mutate(country_from = country_from),

    df_earnings_dist |>
      dplyr::select(deciles, actuals = actual_values_to) |>
      cbind(df_fit_deduction_to$df_deductions) |>
      dplyr::mutate(actuals = ifelse(is.na(actuals), 0, 1)) |>
      dplyr::rename_with(~ paste0(., "_to"), -deciles) |>
      dplyr::mutate(country_to = country_to),

    by = "deciles"
  ) |>
    # Fill in remaining deciles
    tibble::add_row(deciles = seq(0, min(df_earnings_dist$deciles) - 1, by = 1), .before = 1) |>
    tibble::add_row(deciles = seq(max(df_earnings_dist$deciles) + 1, 100, by = 1)) |>
    tidyr::replace_na(list(actuals_from = 0L, actuals_to = 0L))

  return(list(
    "df_main"      = df,
    "df_cat_table" = df_categories
  ))
}
