plSettingsUserUI <- function(id) {

  bslib::accordion(
    multiple = FALSE,
    width    = "100%",
    open     = FALSE,

    # Pension ----
    bslib::accordion_panel(
      title = "Pension",
      value = "accordion-pension",
      icon  = icon("piggy-bank", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Pension - Mandatory',]$col }")),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_sk_emerytalna_rate"),
        label = "State Pension",
        value = analysePay::pl_settings$pension$sk_emerytalna$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_sk_ppk_rate"),
        label = "PPK",
        value = analysePay::pl_settings$pension$ppk$rate
      )
    ),

    # Insurance ----
    bslib::accordion_panel(
      title = "Insurance",
      value = "accordion-insurance",
      icon  = icon("house-chimney-crack", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Insurance - Mandatory',]$col }")),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_sk_rentowa_rate"),
        label = "Social Insurance",
        value = analysePay::pl_settings$insurance$sk_rentowa$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_sk_chorobowa_rate"),
        label = "Ilness Insurance",
        value = analysePay::pl_settings$insurance$sk_chorobowa$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_sk_zdrowotna_rate"),
        label = "Health Insurance",
        value = analysePay::pl_settings$insurance$sk_zdrowotna$rate
      )
    ),

    # Tax ----
    bslib::accordion_panel(
      title = "Tax",
      value = "accordion-tax",
      icon  = icon("money-bill-wave", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Tax',]$col }")),

      shinyWidgets::materialSwitch(
        label     = "Step tax?",
        inputId   = shiny::NS(id, "select_extra_settings_pl"),
        value     = TRUE,
        width     = "100%"
      ),

      shiny::conditionalPanel(
        condition = "input.select_extra_settings_pl == 0",
        ns        = shiny::NS(id),

        shiny::numericInput(
          inputId = shiny::NS(id, "select_pl_tax_rate"),
          label = "Linear Tax",
          value = analysePay::pl_settings$tax$liniowy$rate
        )
      ),

      shiny::conditionalPanel(
        condition = "input.select_extra_settings_pl == 1",
        ns        = shiny::NS(id),

        shiny::numericInput(
          inputId = shiny::NS(id, "select_pl_tax_rate_1"),
          label = "Step Tax Lower",
          value = analysePay::pl_settings$tax$stopniowy$rate_1
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "select_pl_tax_rate_2"),
          label = "Step Tax Middle",
          value = analysePay::pl_settings$tax$stopniowy$rate_2
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "select_pl_tax_rate_3"),
          label = "Step Tax Upper",
          value = analysePay::pl_settings$tax$stopniowy$rate_3
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "select_pl_tax_value_1"),
          label = "Threshold Lower",
          value = analysePay::pl_settings$tax$stopniowy$value_1
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "select_pl_tax_value_2"),
          label = "Threshold Upper",
          value = analysePay::pl_settings$tax$stopniowy$value_2
        )
      )
    ),

    # Student Loan Plan 2 ----
    bslib::accordion_panel(
      title = "Student Loan Plan 2",
      value = "accordion-slp2",
      icon  = icon("credit-card", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Student Loan',]$col }")),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_pl_slp2_rate"),
        label = "Rate",
        value = analysePay::pl_settings$sl_plan2$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_pl_slp2_value"),
        label = "Threshold",
        value = analysePay::pl_settings$sl_plan2$value
      )
    ),

    # Student Loan Plan 3 ----
    bslib::accordion_panel(
      title = "Student Loan Plan 3",
      value = "accordion-slp2",
      icon  = icon("credit-card", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Student Loan',]$col }")),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_pl_slp3_rate"),
        label = "Rate",
        value = analysePay::pl_settings$sl_plan3$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_pl_slp3_value"),
        label = "Threshold",
        value = analysePay::pl_settings$sl_plan3$value
      )
    )
  )
}


plSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    reactive({list(
      "pension" = list(
        "alpha_scheme"  = TRUE, # NOT IN USE
        "sk_emerytalna" = list("rate" = input$select_sk_emerytalna_rate),
        "ppk"           = list("rate" = input$select_sk_ppk_rate)
      ),

      "insurance" = list(
        "sk_rentowa"   = list("rate" = input$select_sk_rentowa_rate),
        "sk_chorobowa" = list("rate" = input$select_sk_chorobowa_rate),
        "sk_zdrowotna" = list("rate" = input$select_sk_zdrowotna_rate)
      ),

      "tax" = list(
        "standard_tax" = input$select_extra_settings_pl,
        "liniowy"      = list("rate" = input$select_pl_tax_rate),
        "stopniowy"    = list(
          "rate_1"     = input$select_pl_tax_rate_1,
          "rate_2"     = input$select_pl_tax_rate_2,
          "rate_3"     = input$select_pl_tax_rate_3,
          "value_1"    = input$select_pl_tax_value_1,
          "value_2"    = input$select_pl_tax_value_2
        )
      ),

      "sl_plan2" = list(
        "rate"  = input$select_pl_slp2_rate,
        "value" = input$select_pl_slp2_value
      ),

      "sl_plan3" = list(
        "rate"  = input$select_pl_slp3_rate,
        "value" = input$select_pl_slp3_value
      )
    )})
  })
}
