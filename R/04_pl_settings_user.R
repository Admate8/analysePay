plSettingsUserUI <- function(id) {
  bslib::accordion(
    multiple = FALSE,
    width = "100%",
    open = FALSE,

    # Pension ----
    bslib::accordion_panel(
      title = "Pension",
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
    # NEED CONDITIONAL PANEL FOR THE TAX
    bslib::accordion_panel(
      title = "Tax",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate"),
        label = "Linear Tax",
        value = analysePay::pl_settings$tax$liniowy$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_1"),
        label = "Step Tax Lower",
        value = analysePay::pl_settings$tax$stopniowy$rate_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_2"),
        label = "Step Tax Middle",
        value = analysePay::pl_settings$tax$stopniowy$rate_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_3"),
        label = "Step Tax Upper",
        value = analysePay::pl_settings$tax$stopniowy$rate_3
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_value_1"),
        label = "Threshold Lower",
        value = analysePay::pl_settings$tax$stopniowy$value_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_value_2"),
        label = "Threshold Upper",
        value = analysePay::pl_settings$tax$stopniowy$value_2
      )
    ),

    # Student Loan Plan 2 ----
    bslib::accordion_panel(
      title = "Student Loan Plan 2",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp2_rate"),
        label = "Rate",
        value = analysePay::pl_settings$sl_plan2$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp2_value"),
        label = "Threshold",
        value = analysePay::pl_settings$sl_plan2$value
      )
    ),

    # Student Loan Plan 3 ----
    bslib::accordion_panel(
      title = "Student Loan Plan 3",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp3_rate"),
        label = "Rate",
        value = analysePay::pl_settings$sl_plan3$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp3_value"),
        label = "Threshold",
        value = analysePay::pl_settings$sl_plan3$value
      )
    )
  )
}


plSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    pl_settings_user <- reactive({list(
      "pension" = list(
        "sk_emerytalna" = list("rate" = input$select_sk_emerytalna_rate),
        "ppk"           = list("rate" = input$select_sk_ppk_rate)
      ),

      "insurance" = list(
        "sk_rentowa"   = list("rate" = input$select_sk_rentowa_rate),
        "sk_chorobowa" = list("rate" = input$select_sk_chorobowa_rate),
        "sk_zdrowotna" = list("rate" = input$select_sk_zdrowotna_rate)
      ),

      "tax" = list(
        "liniowy"   = list("rate" = input$select_tax_rate),
        "stopniowy" = list(
          "rate_1"  = input$select_tax_rate_1,
          "rate_2"  = input$select_tax_rate_2,
          "rate_3"  = input$select_tax_rate_3,
          "value_1" = input$select_tax_value_1,
          "value_2" = input$select_tax_value_2
        )
      ),

      "sl_plan2" = list(
        "rate"  = input$select_slp2_rate,
        "value" = input$select_slp2_value
      ),

      "sl_plan3" = list(
        "rate"  = input$select_slp3_rate,
        "value" = input$select_slp3_value
      )
    )})
  })
}
