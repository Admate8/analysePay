pl_settings <- list(
  "global" = list(
    "full_name" = "Poland",
    "short_cut" = "pl",
    "currency"  = "PLN",
    "locale"    = "pl-PL", # 'https://www.w3schools.com/jsref/jsref_tolocalestring_number.asp',
    # Settings for autonumericInput
    "currencySymbol"          = "z\U0142",
    "currencySymbolPlacement" = "s",
    "decimalCharacter"        = ",",
    "digitGroupSeparator"     = " ",
    "minimumValue"            = 0,
    "maximumValue"            = 1000000
  ),
  "pension" = list(
    "alpha_scheme"  = TRUE, # NOT IN USE, but must exist to repeat big chunks of the code in the server
    "sk_emerytalna" = list(
      "source"      = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"        = 9.76 / 100,
      "rate_linear" = 19.52 / 100
    ),
    # Pracownicze Plany Kapitalowe (PPK)
    "ppk" = list(
      "source" = "https://www.mojeppk.pl/informacje-ogolne.html",
      "rate"   = 2 / 100,
      "rate_employer" = 1.5 / 100
    )
  ),

  "insurance" = list(
    "sk_rentowa" = list(
      "source"      = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"        = 1.5 / 100,
      "rate_linear" = 8 / 100
    ),
    # Dobrowolna skladka chorobowa
    "sk_chorobowa" = list(
      "source"      = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"        = 2.45 / 100,
      "rate_linear" = 2.45 / 100
    ),
    "sk_zdrowotna" = list(
      "source"      = "https://www.biznes.gov.pl/pl/portal/00274",
      "rate"        = 9 / 100,
      "rate_linear" = 4.9 / 100
    ),
    "sk_wypadkowa" = list(
      "source" = "https://www.zus.pl/-/wysokość-stóp-procentowych-składki-na-ubezpieczenie-wypadkowe-na-okres-od-1-kwietnia-2024-r.-do-31-marca-2025-r.?redirect=%2Fo-zus%2Faktualnosci",
      "rate"   = 1.67 / 100
    ),
    "sk_fpfs" = list(
      "source" = "https://www.zus.pl/firmy/rozliczenia-z-zus/skladki-na-ubezpieczenia/fp-fs-i-fgsp",
      "rate"   = 2.45 / 100
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
      "rate"     = 19 / 100,
      "social_deductions_base" = 12 * 4694.4 # In case I want to make it dynamic later
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

  # The range must be 10 - 95! and names must be of the form 'percentile'th
  # to stay consistent in the code. Note that id the percentiles change, so must the
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
#usethis::use_data(pl_settings, overwrite = TRUE)


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

  # Student Loans ----
  sl_plan2_rate  = settings$sl_plan2$rate
  sl_plan2_value = settings$sl_plan2$value

  sl_plan3_rate  = settings$sl_plan3$rate
  sl_plan3_value = settings$sl_plan3$value

  sl_plan2_deduction <- ifelse(
    annual_earnings <= sl_plan2_value, 0,
    (annual_earnings - sl_plan2_value) * sl_plan2_rate
  )
  sl_plan3_deduction <- ifelse(
    annual_earnings <= sl_plan3_value, 0,
    (annual_earnings - sl_plan3_value) * sl_plan3_rate
  )

  # https://www.podatki.gov.pl/pit/ulgi-odliczenia-i-zwolnienia/odliczenie-skladek-na-ubezpieczenie-zdrowotne/#:~:text=Łączna%20wysokość%20składek%20na%20ubezpieczenie,ten%20wynosi%2011%20600%20zł.
  max_annual_health_insurance <- 11600

  # Step tax system ----
  if (standard_tax == TRUE) {

    # This might become dynamic at some point, but the amount is so small,
    # it doesn't make sense to add it to the UI
    koszty_uzyskania_przychodu  <- 12 * 250

    # Calculate deductions and net income
    # https://www.hrkadryiplace.pl/obnizenie-skladki-zdrowotnej-do-wysokosci-zaliczki-na-podatek-umowa-o-prace-zlecenia-jezek-przemyslaw/#:~:text=Kwotę%2C%20o%20której%20mowa%20w%20art.,dzień%2031%20grudnia%202021%20r.&text=4%20900%20zł%20–%204%20047,zł%20%3D%20852%2C24%20zł.

    ## Pension ----
    emerytalna_rate   <- settings$pension$sk_emerytalna$rate
    ppk_rate          <- settings$pension$ppk$rate
    ppk_rate_employer <- settings$pension$ppk$rate_employer

    emerytalna_deduction <- annual_earnings * emerytalna_rate
    ppk_deduction        <- annual_earnings * ppk_rate
    pension_deductions   <- emerytalna_deduction + ppk_deduction

    ## Insurance ----
    rentowa_rate   <- settings$insurance$sk_rentowa$rate
    chorobowa_rate <- settings$insurance$sk_chorobowa$rate
    zdrowotna_rate <- settings$insurance$sk_zdrowotna$rate

    rentowa_deduction        <- annual_earnings * rentowa_rate
    chorobowa_deduction      <- annual_earnings * chorobowa_rate
    insurance_tax_deductable <- emerytalna_deduction + rentowa_deduction + chorobowa_deduction

    ### Health Insurance ----
    # This is necessarily complicated and proves why the system is nuts...
    zdrowotna_1 <- (annual_earnings - insurance_tax_deductable) * zdrowotna_rate
    zdrowotna_2 <- (annual_earnings - insurance_tax_deductable - koszty_uzyskania_przychodu) * 0.17 - 12 * 43.76 # Old tax system: 17% and 43.76 allowance
    zdrowotna_deduction <- ifelse(zdrowotna_1 <= zdrowotna_2, zdrowotna_1, zdrowotna_2)
    zdrowotna_deduction <- ifelse(zdrowotna_deduction <= max_annual_health_insurance, zdrowotna_deduction, max_annual_health_insurance)
    insurance_mandatory <- rentowa_deduction + zdrowotna_deduction

    ## Tax ----
    tax_stopniowy_rate_1  <- settings$tax$stopniowy$rate_1
    tax_stopniowy_rate_2  <- settings$tax$stopniowy$rate_2
    tax_stopniowy_rate_3  <- settings$tax$stopniowy$rate_3
    tax_stopniowy_value_1 <- settings$tax$stopniowy$value_1
    tax_stopniowy_value_2 <- settings$tax$stopniowy$value_2

    taxable_earnings <- annual_earnings + ppk_rate_employer * annual_earnings - insurance_tax_deductable - koszty_uzyskania_przychodu

    tax_deduction <- ifelse(
      taxable_earnings <= tax_stopniowy_value_1,
      taxable_earnings * tax_stopniowy_rate_1,

      ifelse(
        taxable_earnings <= tax_stopniowy_value_2,
        tax_stopniowy_value_1 * tax_stopniowy_rate_1 + (taxable_earnings - tax_stopniowy_value_1) * tax_stopniowy_rate_2,
        tax_stopniowy_value_1 * tax_stopniowy_rate_1 + (tax_stopniowy_value_2 - tax_stopniowy_value_1) * tax_stopniowy_rate_2 + (taxable_earnings - tax_stopniowy_value_2) * tax_stopniowy_rate_3
      )
    )
    tax_deduction        <- 12 * round(tax_deduction / 12)
    insurance_deductions <- rentowa_deduction + chorobowa_deduction + zdrowotna_deduction

    ## Legend-table ----

    # Each tax system has a different system, so this needs to be hard-coded
    # We only need this for a legend-table, so everything should be display-ready
    df_deductions_split <- tibble::tribble(
      ~split,                  ~categories,
      "Pension - Mandatory",   "State Pension*",
      "Pension - Voluntary",   "PPK Pension",
      "Insurance - Mandatory", "State Insurance*<br>Health Insurance",
      "Insurance - Voluntary", "Illness Insurance*",
      "Income Tax",            "Income Tax",
      "Student Loan Plan 2",   "Student Loan Plan 2",
      "Student Loan Plan 3",   "Student Loan Plan 3",
      "Net Income",            "Net Income"
    )
  }

  # Linear Tax System ----
  else {
    ## Social Deductions ----
    social_deductions_base <- analysePay::pl_settings$tax$liniowy$social_deductions_base

    emerytalna_deduction   <- social_deductions_base * settings$pension$sk_emerytalna$rate_linear
    pension_deductions     <- emerytalna_deduction
    ppk_deduction          <- NA

    rentowa_deduction      <- social_deductions_base * settings$insurance$sk_rentowa$rate_linear
    chorobowa_deduction    <- social_deductions_base * settings$insurance$sk_chorobowa$rate_linear
    wypadkowa_deduction    <- social_deductions_base * settings$insurance$sk_wypadkowa$rate
    fpfs_deduction         <- social_deductions_base * settings$insurance$sk_fpfs$rate

    zdrowotna_deduction_base <- annual_earnings - (emerytalna_deduction + rentowa_deduction + chorobowa_deduction + wypadkowa_deduction + fpfs_deduction)
    zdrowotna_deduction      <- zdrowotna_deduction_base * settings$insurance$sk_zdrowotna$rate_linear
    zdrowotna_deduction      <- ifelse(zdrowotna_deduction <= max_annual_health_insurance, zdrowotna_deduction, max_annual_health_insurance)
    insurance_deductions     <- zdrowotna_deduction + rentowa_deduction + chorobowa_deduction + wypadkowa_deduction + fpfs_deduction
    insurance_mandatory      <- rentowa_deduction + zdrowotna_deduction + wypadkowa_deduction + fpfs_deduction

    ## Tax ----
    taxable_earnings <- zdrowotna_deduction_base - zdrowotna_deduction
    tax_deduction    <- taxable_earnings * settings$tax$liniowy$rate


    ## Legend-table ----
    # Each tax system has a different system, so this needs to be hard-coded
    # We only need this for a legend-table, so everything should be display-ready
    df_deductions_split <- tibble::tribble(
      ~split,                  ~categories,
      "Pension - Mandatory",   "State Pension*",
      "Pension - Voluntary",   "",
      "Insurance - Mandatory", "State Insurance*<br>Health Insurance*<br>Accident Insurance*<br>FP, FS and FG\U015AP*",
      "Insurance - Voluntary", "Illness Insurance*",
      "Income Tax",            "Income Tax",
      "Student Loan Plan 2",   "Student Loan Plan 2",
      "Student Loan Plan 3",   "Student Loan Plan 3",
      "Net Income",            "Net Income"
    )
  }

  # Net Income ----
  net_income <- annual_earnings - (pension_deductions + insurance_deductions + tax_deduction + sl_plan2_deduction + sl_plan3_deduction)

  # Results ----
  # Make sure this table contains all categories
  df_deductions <- data.frame(
    earnings            = annual_earnings,
    pension_mandatory   = emerytalna_deduction,
    pension_voluntary   = ppk_deduction,
    insurance_mandatory = insurance_mandatory,
    insurance_voluntary = chorobowa_deduction,
    income_tax          = tax_deduction,
    student_loan_plan_2 = sl_plan2_deduction,
    student_loan_plan_3 = sl_plan3_deduction,
    net_income          = net_income
  ) |>
    dplyr::mutate(dplyr::across(-earnings, function(x) x / earnings, .names = "{.col}_perc"))



  return(list(
    "df_deductions"       = df_deductions,
    "df_deductions_split" = df_deductions_split
  ))
}
