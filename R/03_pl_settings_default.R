pl_settings <- list(
  "pension" = list(
    "sk_emerytalna" = list(
      "source" = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"   = 9.76 / 100
    ),
    # Pracownicze Plany Kapitalowe (PPK)
    "ppk" = list(
      "source" = "https://www.mojeppk.pl",
      "rate"   = 2 / 100
    )
  ),

  "insurance" = list(
    "sk_rentowa" = list(
      "source" = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"   = 1.5 / 100
    ),
    # Dobrowolna skladka chorobowa
    "sk_chorobowa" = list(
      "source" = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"   = 2.45 / 100
    ),
    "sk_zdrowotna" = list(
      "source" = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"   = 9 / 100
    )
  ),

  "tax" = list(
    "stopniowy" = list(
      "source"  = "https://biznes.gov.pl/pl/portal/00264",
      "rate_1"  = 0,
      "rate_2"  = 12 / 100,
      "rate_3"  = 32 / 100,
      "value_1" = 30000,
      "value_2" = 120000
    ),
    "liniowy" = list(
      "source"   = "https://www.biznes.gov.pl/pl/portal/00253",
      "rate"     = 19 / 100
    )
  ),

  "sl_plan2" = list(
    "source" = "https://www.gov.uk/government/publications/overseas-earnings-thresholds-for-plan-2-student-loans/overseas-earnings-thresholds-for-plan-2-student-loans-2024-25",
    "rate"   = 9 / 100,
    "value"  = 16380 / 0.190581
  ),

  "sl_plan3" = list(
    "source" = "https://www.gov.uk/government/publications/overseas-earnings-thresholds-for-postgraduate-student-loans/overseas-earnings-thresholds-for-postgraduate-student-loans-2024-25",
    "rate"   = 6 / 100,
    "value"  = 12600 / 0.190581
  ),

  # The range must be 10 - 95! and names must be of the form 'decile'th
  # to stay consistent in the code
  "earning_deciles" = list(
    "source" = "https://stat.gov.pl/sygnalne/komunikaty-i-obwieszczenia/lista-komunikatow-i-obwieszczen/komunikat-w-sprawie-przecietnego-wynagrodzenia-w-drugim-kwartale-2024-roku,271,45.html",
    "10th"   = 12 * 4242,
    "20th"   = 12 * 4536.2,
    "30th"   = 12 * 5138.72,
    "40th"   = 12 * 5775.76,
    "50th"   = 12 * 6500,
    "60th"   = 12 * 7393.75,
    "70th"   = 12 * 8415.2,
    "80th"   = 12 * 9929.3,
    "90th"   = 12 * 13012.14,
    "95th"   = 12 * 17500 # Estimated (not in the source)
  )
)
usethis::use_data(pl_settings, overwrite = TRUE)


# Category is unique to the country but category_wide is common across countries
pl_deduction_types <- tibble::tribble(
  ~category,            ~category_wide,
  "Earnings",           "Earnings",
  "Emerytalna",         "Pension - Mandatory",
  "PPK",                "Pension - Voluntary",
  "Rentowa",            "Insurance - Mandatory",
  "Chorobowa",          "Insurance - Voluntary",
  "Zdrowotna",          "Insurance - Mandatory",
  "Tax",                "Tax",
  "SL Plan 2",          "Student Loan",
  "SL Plan 3",          "Student Loan",
  "Net Income",         "Net Income"
)


#' Calculate PL Deductions Breakdown Given Gross Annual Earnings
#'
#' Note that the arguments must be the same across all \code{calc_\{country\}_deductions}
#' to ensure that all Shiny settings are reversible.
#'
#' @param annual_earnings Vector of annual gross earnings.
#' @param alpha_scheme Not in use for the PL deductions.
#' @param standard_tax Use progressive tax (UoP) or linear tax (B2B)?
#' @param incluse_slp2 Include Student Loan Plan 2 in the breakdown?
#' @param incluse_slp3 Include Student Loan Plan 3 in the breakdown?
#' @param user_data Use either default setting for the UK (\code{NULL}) or the
#' data supplied by the user.
#'
#' @return Tibbles with deduction and net income values by either the category
#' or the category wide (see \code{pl_deduction_types}).
#' @examples
#' \dontrun{
#' calc_pl_deductions(
#'    annual_earnings = c(70000, 80000, 90000),
#'    alpha_scheme    = TRUE,
#'    standard_tax    = TRUE,
#'    incluse_slp2    = TRUE,
#'    incluse_slp3    = TRUE,
#'    user_data       = NULL
#' )$df_deductions_category_wide
#' }
calc_pl_deductions <- function(
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
    is.logical(standard_tax)
  )

  # Use default country settings if settings_user == FALSE
  if (is.null(user_data)) pl_settings <- analysePay::pl_settings
  else pl_settings <- user_data

  # Define a set of variables from the pl_settings object
  emerytalna_rate = pl_settings$pension$sk_emerytalna$rate
  ppk_rate        = pl_settings$pension$ppk$rate

  rentowa_rate    = pl_settings$insurance$sk_rentowa$rate
  chorobowa_rate  = pl_settings$insurance$sk_chorobowa$rate
  zdrowotna_rate  = pl_settings$insurance$sk_zdrowotna$rate

  tax_stopniowy_rate_1  = pl_settings$tax$stopniowy$rate_1
  tax_stopniowy_rate_2  = pl_settings$tax$stopniowy$rate_2
  tax_stopniowy_rate_3  = pl_settings$tax$stopniowy$rate_3
  tax_stopniowy_value_1 = pl_settings$tax$stopniowy$value_1
  tax_stopniowy_value_2 = pl_settings$tax$stopniowy$value_2
  tax_liniowy_rate      = pl_settings$tax$liniowy$rate

  sl_plan2_rate  = pl_settings$sl_plan2$rate
  sl_plan2_value = pl_settings$sl_plan2$value

  sl_plan3_rate  = pl_settings$sl_plan3$rate
  sl_plan3_value = pl_settings$sl_plan3$value

  # Calculate deductions and net income
  # Pension ----
  emerytalna_deduction <- annual_earnings * emerytalna_rate
  ppk_deduction        <- annual_earnings * ppk_rate

  # Insurance ----
  rentowa_deduction   <- annual_earnings * rentowa_rate
  chorobowa_deduction <- annual_earnings * chorobowa_rate
  zdrowotna_deduction <- (annual_earnings - emerytalna_deduction - rentowa_deduction - chorobowa_deduction) * zdrowotna_rate

  # Tax ----
  # Nie mozna od niego odliczyc skladki chorobowej i ppk
  taxable_earnings <- annual_earnings - (emerytalna_deduction + rentowa_deduction + zdrowotna_deduction)

  if (standard_tax == TRUE) {
    tax_deduction <- ifelse(
      taxable_earnings <= tax_stopniowy_value_1,
      taxable_earnings * tax_stopniowy_rate_1,

      ifelse(
        taxable_earnings <= tax_stopniowy_value_2,
        tax_stopniowy_value_1 * tax_stopniowy_rate_1 + (taxable_earnings - tax_stopniowy_value_1) * tax_stopniowy_rate_2,
        tax_stopniowy_value_1 * tax_stopniowy_rate_1 + (tax_stopniowy_value_2 - tax_stopniowy_value_1) * tax_stopniowy_rate_2 + (taxable_earnings - tax_stopniowy_value_2) * tax_stopniowy_rate_3
      )
    )
  } else {
    tax_deduction <- taxable_earnings * tax_liniowy_rate
  }

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
  net_income <- annual_earnings - (emerytalna_deduction + ppk_deduction + rentowa_deduction + chorobowa_deduction + zdrowotna_deduction + tax_deduction + sl_plan2_deduction + sl_plan3_deduction)


  # Results ----
  df_results_category <- tibble::tibble(
    earnings   = annual_earnings,
    emerytalna = emerytalna_deduction,
    ppk        = ppk_deduction,
    rentowa    = rentowa_deduction,
    chorobowa  = chorobowa_deduction,
    zdrowotna  = zdrowotna_deduction,
    tax        = tax_deduction,
    sl_plan_2  = sl_plan2_deduction,
    sl_plan_3  = sl_plan3_deduction,
    net_income = net_income
  ) |>
    dplyr::mutate(dplyr::across(-earnings, function(x) x / earnings, .names = "{.col}_perc"))

  df_results_category_wide <- tibble::tibble(
    earnings            = annual_earnings,
    pension_mandatory   = emerytalna_deduction,
    pension_voluntary   = ppk_deduction,
    insurance_mandatory = rentowa_deduction + zdrowotna_deduction,
    insurance_voluntary = chorobowa_deduction,
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
