uk_settings <- list(
  "pension" = list(
    "rate"          = 4 / 100,
    "source"        = "https://www.civilservicepensionscheme.org.uk/your-pension/managing-your-pension/contribution-rates/",
    "alpha_rate_1"  = 4.6 / 100,
    "alpha_rate_2"  = 5.45 / 100,
    "alpha_rate_3"  = 7.35 / 100,
    "alpha_rate_4"  = 8.05 / 100,
    "alpha_value_1" = 32000,
    "alpha_value_2" = 56000,
    "alpha_value_3" = 150000
  ),

  "insurance" = list(
    "source"  = "https://www.gov.uk/national-insurance-rates-letters",
    "rate_1"  = 0,
    "rate_2"  = 8 / 100,
    "rate_3"  = 2 / 100,
    "value_1" = 12571,
    "value_2" = 50270
  ),

  "tax" = list(
    "source"  = "https://www.gov.uk/income-tax-rates",
    "rate_1"  = 0,
    "rate_2"  = 20 / 100,
    "rate_3"  = 40 / 100,
    "rate_4"  = 45 / 100, # + no personal allowance!
    "value_1" = 12571,
    "value_2" = 50270,
    "value_3" = 125140
  ),

  "sl_plan2" = list(
    "source" = "https://www.gov.uk/repaying-your-student-loan/what-you-pay",
    "rate"   = 9 / 100,
    "value"  = 27295
  ),

  "sl_plan3" = list(
    "source" = "https://www.gov.uk/repaying-your-student-loan/what-you-pay",
    "rate"   = 6 / 100,
    "value"  = 21000
  ),

  # The range must be 10 - 95! and names must be of the form 'decile'th
  # to stay consistent in the code
  "earning_deciles" = list(
    "source" = "https://www.statista.com/statistics/416102/average-annual-gross-pay-percentiles-united-kingdom/",
    "10th"   = 21000,
    "20th"   = 24496,
    "25th"   = 26485,
    "30th"   = 27673,
    "40th"   = 31069,
    "50th"   = 34632,
    "60th"   = 39519,
    "70th"   = 44738,
    "75th"   = 48000,
    "80th"   = 52007,
    "90th"   = 66669,
    "95th"   = 80000
  )
)
usethis::use_data(uk_settings, overwrite = TRUE)


# Category is unique to the country but category_wide is common across countries
uk_deduction_types <- tibble::tribble(
  ~category,            ~category_wide,
  "Earnings",           "Earnings",
  "Pension",            "Pension - Voluntary",
  "National Insurance", "Insurance - Mandatory",
  "Tax",                "Tax",
  "SL Plan 2",          "Student Loan",
  "SL Plan 3",          "Student Loan",
  "Net Income",         "Net Income"
)


#' Calculate UK Deductions Breakdown Given Gross Annual Earnings
#'
#' Note that the arguments must be the same across all \code{calc_\{country\}_deductions}
#' functions to ensure that all Shiny settings are reversible.
#'
#' @param annual_earnings Vector of annual gross earnings.
#' @param alpha_scheme Should the pension contribution be deducted using alpha scheme?
#' @param standard_tax Not in use for the UK deductions.
#' @param incluse_slp2 Include Student Loan Plan 2 in the breakdown?
#' @param incluse_slp3 Include Student Loan Plan 3 in the breakdown?
#' @param user_data Use either default setting for the UK (\code{NULL}) or the
#' data supplied by the user.
#'
#' @return Tibbles with deduction and net income values by either the category
#' or the category wide (see \code{uk_deduction_types}).
#' @examples
#' \dontrun{
#' calc_uk_deductions(
#'    annual_earnings = c(30000, 35000, 50000),
#'    alpha_scheme    = TRUE,
#'    standard_tax    = TRUE,
#'    incluse_slp2    = TRUE,
#'    incluse_slp3    = TRUE,
#'    user_data       = NULL
#' )$df_deductions_category_wide
#' }
calc_uk_deductions <- function(
    annual_earnings,
    alpha_scheme  = TRUE,
    standard_tax  = TRUE,
    incluse_slp2  = TRUE,
    incluse_slp3  = TRUE,
    user_data     = NULL
) {
  # Do some basic checks of the input
  stopifnot(
    "All values must be numerics" = all(is.numeric(annual_earnings)),
    "All values must be positive" = all(annual_earnings > 0),
    is.logical(alpha_scheme)
  )

  # Use default country settings or user's settings
  if (is.null(user_data)) uk_settings <- analysePay::uk_settings
  else uk_settings <- user_data

  # Define a set of variables from the uk_settings object
  pension_rate  = uk_settings$pension$rate
  alpha_rate_1  = uk_settings$pension$alpha_rate_1
  alpha_rate_2  = uk_settings$pension$alpha_rate_2
  alpha_rate_3  = uk_settings$pension$alpha_rate_3
  alpha_rate_4  = uk_settings$pension$alpha_rate_4
  alpha_value_1 = uk_settings$pension$alpha_value_1
  alpha_value_2 = uk_settings$pension$alpha_value_2
  alpha_value_3 = uk_settings$pension$alpha_value_3

  ni_rate_1  = uk_settings$insurance$rate_1
  ni_rate_2  = uk_settings$insurance$rate_2
  ni_rate_3  = uk_settings$insurance$rate_3
  ni_value_1 = uk_settings$insurance$value_1
  ni_value_2 = uk_settings$insurance$value_2

  tax_rate_1  = uk_settings$tax$rate_1
  tax_rate_2  = uk_settings$tax$rate_2
  tax_rate_3  = uk_settings$tax$rate_3
  tax_rate_4  = uk_settings$tax$rate_4
  tax_value_1 = uk_settings$tax$value_1
  tax_value_2 = uk_settings$tax$value_2
  tax_value_3 = uk_settings$tax$value_3

  sl_plan2_rate  = uk_settings$sl_plan2$rate
  sl_plan2_value = uk_settings$sl_plan2$value

  sl_plan3_rate  = uk_settings$sl_plan3$rate
  sl_plan3_value = uk_settings$sl_plan3$value

  # Assume no bonuses and rewards, so that
  pensionable_earnings <- annual_earnings

  # Calculate deductions and net income
  # Pension ----
  if (alpha_scheme == TRUE) {
    pension_deduction <- ifelse(
      pensionable_earnings <= alpha_value_1,
      pensionable_earnings * alpha_rate_1,

      ifelse(
        pensionable_earnings <= alpha_value_2,
        pensionable_earnings * alpha_rate_2,

        ifelse(
          pensionable_earnings <= alpha_value_3,
          pensionable_earnings * alpha_rate_3,
          pensionable_earnings * alpha_rate_4
        )
      )
    )
  } else {
    pension_deduction <- pensionable_earnings * pension_rate
  }

  taxable_earnings <- pensionable_earnings - pension_deduction


  # Insurance ----
  national_insurance_deduction <- ifelse(
    annual_earnings <= ni_value_1,
    annual_earnings * ni_rate_1,

    ifelse(
      annual_earnings <= ni_value_2,
      ni_value_1 * ni_rate_1 + (annual_earnings - ni_value_1) * ni_rate_2,
      ni_value_1 * ni_rate_1 + (ni_value_2 - ni_value_1) * ni_rate_2 + (annual_earnings - ni_value_2) * ni_rate_3
    )
  )


  # Tax ----
  tax_deduction <- ifelse(
    taxable_earnings <= tax_value_1,
    taxable_earnings * tax_rate_1,

    ifelse(
      taxable_earnings <= tax_value_2,
      tax_value_1 * tax_rate_1 + (taxable_earnings - tax_value_1) * tax_rate_2,

      ifelse(
        taxable_earnings <= tax_value_3,
        tax_value_1 * tax_rate_1 + (tax_value_2 - tax_value_1) * tax_rate_2 + (taxable_earnings - tax_value_2) * tax_rate_3,
        # Remove personal allowance
        tax_value_2 * tax_rate_2 + (tax_value_3 - tax_value_2) * tax_rate_3 + (taxable_earnings - tax_value_3) * tax_rate_4
      )
    )
  )


  # Student Loans ----
  if (incluse_slp2) {
    sl_plan2_deduction <- ifelse(
      annual_earnings <= sl_plan2_value, 0,
      (annual_earnings - sl_plan2_value) * sl_plan2_rate
    )
  } else sl_plan2_deduction <- 0
  if (incluse_slp3) {
    sl_plan3_deduction <- ifelse(
      annual_earnings <= sl_plan3_value, 0,
      (annual_earnings - sl_plan3_value) * sl_plan3_rate
    )
  } else sl_plan3_deduction <- 0


  # Net Income ----
  net_income <- annual_earnings - (pension_deduction + national_insurance_deduction + tax_deduction + sl_plan2_deduction + sl_plan3_deduction)

  # Results ----
  df_results_category <- tibble::tibble(
    earnings           = annual_earnings,
    pension            = pension_deduction,
    national_insurance = national_insurance_deduction,
    tax                = tax_deduction,
    sl_plan_2          = sl_plan2_deduction,
    sl_plan_3          = sl_plan3_deduction,
    net_income         = net_income
  ) |>
    dplyr::mutate(dplyr::across(-earnings, function(x) x / earnings, .names = "{.col}_perc"))

  df_results_category_wide <- tibble::tibble(
    earnings            = annual_earnings,
    pension_voluntary   = pension_deduction,
    insurance_mandatory = national_insurance_deduction,
    tax                 = tax_deduction,
    student_loan        = sl_plan2_deduction + sl_plan3_deduction,
    net_income          = net_income
  ) |>
    dplyr::mutate(dplyr::across(-earnings, function(x) x / earnings, .names = "{.col}_perc"))

  return(list(
    "df_deductions_category"      = df_results_category,
    "df_deductions_category_wide" = df_results_category_wide
  ))
}
