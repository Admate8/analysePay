pl_settings <- list(
  "global" = list(
    "full_name" = "Poland",
    "short_cut" = "pl",
    "currency"  = "PLN",
    "locale"    = "pl-PL" # 'https://www.w3schools.com/jsref/jsref_tolocalestring_number.asp'
  ),
  "pension" = list(
    "alpha_scheme"  = TRUE, # NOT IN USE, but must exist to repeat big chunks of the code in the server
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
    "standard_tax" = TRUE,
    "stopniowy"    = list(
      "source"  = "https://biznes.gov.pl/pl/portal/00264",
      "rate_1"  = 0,
      "rate_2"  = 12 / 100,
      "rate_3"  = 32 / 100,
      "value_1" = 30000,
      "value_2" = 120000
    ),
    "liniowy"      = list(
      "source"   = "https://www.biznes.gov.pl/pl/portal/00253",
      "rate"     = 19 / 100
    )
  ),

  "sl_plan2" = list(
    "source" = "https://www.gov.uk/government/publications/overseas-earnings-thresholds-for-plan-2-student-loans/overseas-earnings-thresholds-for-plan-2-student-loans-2024-25",
    "rate"   = 9 / 100,
    "value"  = round(16380 / 0.190581, 2)
  ),

  "sl_plan3" = list(
    "source" = "https://www.gov.uk/government/publications/overseas-earnings-thresholds-for-postgraduate-student-loans/overseas-earnings-thresholds-for-postgraduate-student-loans-2024-25",
    "rate"   = 6 / 100,
    "value"  = round(12600 / 0.190581, 2)
  ),

  # The range must be 10 - 95! and names must be of the form 'decile'th
  # to stay consistent in the code. Note that id the deciles change, so must the
  # reference in the server in `04_pl_settings_user.R`!
  "earning_deciles" = list(
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
  ),
  "decile_source" = "https://stat.gov.pl/sygnalne/komunikaty-i-obwieszczenia/lista-komunikatow-i-obwieszczen/komunikat-w-sprawie-przecietnego-wynagrodzenia-w-drugim-kwartale-2024-roku,271,45.html"
)
usethis::use_data(pl_settings, overwrite = TRUE)


#' Calculate PL Deductions Breakdown Given Gross Annual Earnings
#'
#' Note that the arguments must be the same across all \code{calc_\{country\}_deductions}
#' to ensure that all Shiny settings are reversible.
#'
#' @param annual_earnings Vector of annual gross earnings.
#' @param alpha_scheme Not in use for the PL deductions.
#' @param standard_tax Use progressive tax (UoP) or linear tax (B2B)?
#' @param settings Use either default setting for the PL or the data supplied by the user.
#'
#' @return Tibbles with deductions and net income values.
#' @examples
#' \dontrun{
#' calc_pl_deductions(annual_earnings = c(NA, 70000, 80000, 90000, NA))$df_deductions
#' }
calc_pl_deductions <- function(
    annual_earnings,
    alpha_scheme  = TRUE,
    standard_tax  = TRUE,
    settings      = pl_settings
) {

  # Define a set of variables from the `settings` argument
  emerytalna_rate = settings$pension$sk_emerytalna$rate
  ppk_rate        = settings$pension$ppk$rate

  rentowa_rate    = settings$insurance$sk_rentowa$rate
  chorobowa_rate  = settings$insurance$sk_chorobowa$rate
  zdrowotna_rate  = settings$insurance$sk_zdrowotna$rate

  tax_stopniowy_rate_1  = settings$tax$stopniowy$rate_1
  tax_stopniowy_rate_2  = settings$tax$stopniowy$rate_2
  tax_stopniowy_rate_3  = settings$tax$stopniowy$rate_3
  tax_stopniowy_value_1 = settings$tax$stopniowy$value_1
  tax_stopniowy_value_2 = settings$tax$stopniowy$value_2
  tax_liniowy_rate      = settings$tax$liniowy$rate

  sl_plan2_rate  = settings$sl_plan2$rate
  sl_plan2_value = settings$sl_plan2$value

  sl_plan3_rate  = settings$sl_plan3$rate
  sl_plan3_value = settings$sl_plan3$value

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
  sl_plan2_deduction <- ifelse(
    annual_earnings <= sl_plan2_value, 0,
    (annual_earnings - sl_plan2_value) * sl_plan2_rate
  )
  sl_plan3_deduction <- ifelse(
    annual_earnings <= sl_plan3_value, 0,
    (annual_earnings - sl_plan3_value) * sl_plan3_rate
  )

  # Net Income ----
  net_income <- annual_earnings - (emerytalna_deduction + ppk_deduction + rentowa_deduction + chorobowa_deduction + zdrowotna_deduction + tax_deduction + sl_plan2_deduction + sl_plan3_deduction)


  # Results ----
  # Make sure this table contains all categories
  df_deductions <- tibble::tibble(
    earnings            = annual_earnings,
    pension_mandatory   = emerytalna_deduction,
    pension_voluntary   = ppk_deduction,
    insurance_mandatory = rentowa_deduction + zdrowotna_deduction,
    insurance_voluntary = chorobowa_deduction,
    income_tax          = tax_deduction,
    student_loan_plan_2 = sl_plan2_deduction,
    student_loan_plan_3 = sl_plan3_deduction,
    net_income          = net_income
  ) |>
    dplyr::mutate(dplyr::across(-earnings, function(x) x / earnings, .names = "{.col}_perc"))

  # `insurance_mandatory` consist of two parts - find their percentage contribution
  # they will be the same regardless of annual earnings. Add them to the data
  rentowa_perc <- round(100 * rentowa_deduction[1] / (rentowa_deduction[1] + zdrowotna_deduction[1]), 2)

  # Each tax system has a different system, so this needs to be hard-coded
  # We only need this for a legend-table, so everything should display ready
  df_deductions_split <- tibble::tribble(
    ~split,                  ~categories,
    "Pension - Mandatory",   "State Pension*",
    "Pension - Voluntary",   "PPK Pension",
    "Insurance - Mandatory", paste0("State Insurance* (", rentowa_perc, "%), Health Insurance* (", 100 - rentowa_perc, "%)"),
    "Insurance - Voluntary", "Illness Insurance",
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
