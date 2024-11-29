uk_settings <- list(
  "global" = list(
    "full_name" = "United Kingdom",
    "short_cut" = "uk",
    "currency"  = "GBP",
    "locale"    = "en-GB", # 'https://www.w3schools.com/jsref/jsref_tolocalestring_number.asp'
    # Settings for autonumericInput
    "currencySymbol"          = "\U00A3",
    "currencySymbolPlacement" = "p",
    "decimalCharacter"        = ".",
    "digitGroupSeparator"     = ",",
    "minimumValue"            = 0,
    "maximumValue"            = 200000
  ),
  "pension" = list(
    "rate"               = 4 / 100,
    "source_alpha"       = "https://www.civilservicepensionscheme.org.uk",
    "source_alpha_rates" = "https://www.civilservicepensionscheme.org.uk/your-pension/managing-your-pension/contribution-rates/",
    "alpha_scheme"       = TRUE,
    "alpha_rate_1"       = 4.6 / 100,
    "alpha_rate_2"       = 5.45 / 100,
    "alpha_rate_3"       = 7.35 / 100,
    "alpha_rate_4"       = 8.05 / 100,
    "alpha_value_1"      = 32000,
    "alpha_value_2"      = 56000,
    "alpha_value_3"      = 150000
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
    "source"       = "https://www.gov.uk/income-tax-rates",
    "standard_tax" = TRUE, # NOT IN USE, but must exist to repeat big chunks of the code in the server
    "rate_1"       = 0,
    "rate_2"       = 20 / 100,
    "rate_3"       = 40 / 100,
    "rate_4"       = 45 / 100, # + no personal allowance (coded in the calc function)
    "value_1"      = 12571,
    "value_2"      = 50270,
    "value_3"      = 125140
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
  # to stay consistent in the code. Note that id the deciles change, so must the
  # reference in the server in `04_uk_settings_user.R`!
  "earning_deciles" = list(
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
  ),
  "decile_source" = "https://www.statista.com/statistics/416102/average-annual-gross-pay-percentiles-united-kingdom/"
)
#usethis::use_data(uk_settings, overwrite = TRUE)


#' Calculate UK Deductions Breakdown Given Gross Annual Earnings
#'
#' Note that the arguments must be the same across all \code{calc_\{country\}_deductions}
#' functions to ensure that all Shiny settings are reversible.
#'
#' @param annual_earnings Vector of annual gross earnings.
#' @param alpha_scheme Should the pension contribution be deducted using alpha scheme?
#' @param standard_tax Not in use for the UK deductions.
#' @param settings Use either default setting for the UK or the data supplied by the user.
#'
#' @return Tibbles with deductions and net income values.
#' @examples
#' \dontrun{
#' calc_uk_deductions(annual_earnings = c(NA, 30000, 35000, 50000, NA))$df_deductions
#' }
calc_uk_deductions <- function(
    annual_earnings,
    alpha_scheme  = TRUE,
    standard_tax  = TRUE,
    settings      = uk_settings
) {

  # Define a set of variables from the `settings` argument
  pension_rate  = settings$pension$rate
  alpha_rate_1  = settings$pension$alpha_rate_1
  alpha_rate_2  = settings$pension$alpha_rate_2
  alpha_rate_3  = settings$pension$alpha_rate_3
  alpha_rate_4  = settings$pension$alpha_rate_4
  alpha_value_1 = settings$pension$alpha_value_1
  alpha_value_2 = settings$pension$alpha_value_2
  alpha_value_3 = settings$pension$alpha_value_3

  ni_rate_1  = settings$insurance$rate_1
  ni_rate_2  = settings$insurance$rate_2
  ni_rate_3  = settings$insurance$rate_3
  ni_value_1 = settings$insurance$value_1
  ni_value_2 = settings$insurance$value_2

  tax_rate_1  = settings$tax$rate_1
  tax_rate_2  = settings$tax$rate_2
  tax_rate_3  = settings$tax$rate_3
  tax_rate_4  = settings$tax$rate_4
  tax_value_1 = settings$tax$value_1
  tax_value_2 = settings$tax$value_2
  tax_value_3 = settings$tax$value_3

  sl_plan2_rate  = settings$sl_plan2$rate
  sl_plan2_value = settings$sl_plan2$value

  sl_plan3_rate  = settings$sl_plan3$rate
  sl_plan3_value = settings$sl_plan3$value

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
  sl_plan2_deduction <- ifelse(
    annual_earnings <= sl_plan2_value, 0,
    (annual_earnings - sl_plan2_value) * sl_plan2_rate
  )
  sl_plan3_deduction <- ifelse(
    annual_earnings <= sl_plan3_value, 0,
    (annual_earnings - sl_plan3_value) * sl_plan3_rate
  )

  # Net Income ----
  net_income <- annual_earnings - (pension_deduction + national_insurance_deduction + tax_deduction + sl_plan2_deduction + sl_plan3_deduction)


  # Results ----
  # Make sure this table contains all categories
  df_deductions <- tibble::tibble(
    earnings            = annual_earnings,
    pension_mandatory   = NA,
    pension_voluntary   = pension_deduction,
    insurance_mandatory = national_insurance_deduction,
    insurance_voluntary = NA,
    income_tax          = tax_deduction,
    student_loan_plan_2 = sl_plan2_deduction,
    student_loan_plan_3 = sl_plan3_deduction,
    net_income          = net_income
  ) |>
    dplyr::mutate(dplyr::across(-earnings, function(x) x / earnings, .names = "{.col}_perc"))

  # Each tax system has a different system, so this needs to be hard-coded
  # We only need this for a legend-table, so everything should display ready
  df_deductions_split <- tibble::tribble(
    ~split,                  ~categories,
    "Pension - Mandatory",   "",
    "Pension - Voluntary",   "Pension*",
    "Insurance - Mandatory", "National Insurance",
    "Insurance - Voluntary", "",
    "Income Tax",            "Income Tax",
    "Student Loan Plan 2",   "Student Loan Plan 2",
    "Student Loan Plan 3",   "Student Loan Plan 3",
    "Net Income",            "Net Income"
  )

  return(list(
    "df_deductions"       = df_deductions,
    "df_deductions_split" = df_deductions_split
  ))
}
