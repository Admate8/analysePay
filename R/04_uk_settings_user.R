
ukSettingsUserUI <- function(id) {
  bslib::accordion(
    multiple = FALSE,
    width = "100%",

    # Pension ----
    bslib::accordion_panel(
      title = "Pension",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_pension_rate"),
        label = "Pension Rate",
        value = analysePay::uk_settings$pension$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_rate_1"),
        label = "Alpha Rate Lower",
        value = analysePay::uk_settings$pension$alpha_rate_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_rate_2"),
        label = "Alpha Rate Middle",
        value = analysePay::uk_settings$pension$alpha_rate_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_rate_3"),
        label = "Alpha Rate Upper",
        value = analysePay::uk_settings$pension$alpha_rate_3
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_rate_4"),
        label = "Alpha Rate Additional",
        value = analysePay::uk_settings$pension$alpha_rate_4
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_value_1"),
        label = "Alpha Threshold Lower",
        value = analysePay::uk_settings$pension$alpha_value_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_value_2"),
        label = "Alpha Threshold Middle",
        value = analysePay::uk_settings$pension$alpha_value_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_alpha_value_3"),
        label = "Alpha Threshold Upper",
        value = analysePay::uk_settings$pension$alpha_value_3
      )
    ),

    # Insurance ----
    bslib::accordion_panel(
      title = "National Insurance",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_ni_rate_1"),
        label = "Rate Lower",
        value = analysePay::uk_settings$national_insurance$rate_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_ni_rate_2"),
        label = "Rate Middle",
        value = analysePay::uk_settings$national_insurance$rate_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_ni_rate_3"),
        label = "Rate Upper",
        value = analysePay::uk_settings$national_insurance$rate_3
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_ni_value_1"),
        label = "Threshold Lower",
        value = analysePay::uk_settings$national_insurance$value_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_ni_value_2"),
        label = "Threshold Upper",
        value = analysePay::uk_settings$national_insurance$value_2
      )
    ),

    # Tax ----
    bslib::accordion_panel(
      title = "Tax",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_1"),
        label = "Rate Lower",
        value = analysePay::uk_settings$tax$rate_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_2"),
        label = "Rate Middle",
        value = analysePay::uk_settings$tax$rate_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_3"),
        label = "Rate Upper",
        value = analysePay::uk_settings$tax$rate_3
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_rate_4"),
        label = "Rate Additional",
        value = analysePay::uk_settings$tax$rate_4
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_value_1"),
        label = "Threshold Lower",
        value = analysePay::uk_settings$tax$value_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_value_2"),
        label = "Threshold Middle",
        value = analysePay::uk_settings$tax$value_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_tax_value_3"),
        label = "Threshold Upper",
        value = analysePay::uk_settings$tax$value_3
      )
    ),

    # Student Loan Plan 2 ----
    bslib::accordion_panel(
      title = "Student Loan Plan 2",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp2_rate"),
        label = "Rate",
        value = analysePay::uk_settings$sl_plan2$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp2_value"),
        label = "Threshold",
        value = analysePay::uk_settings$sl_plan2$value
      )
    ),

    # Student Loan Plan 3 ----
    bslib::accordion_panel(
      title = "Student Loan Plan 3",
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp3_rate"),
        label = "Rate",
        value = analysePay::uk_settings$sl_plan3$rate
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_slp3_value"),
        label = "Threshold",
        value = analysePay::uk_settings$sl_plan3$value
      )
    )
  )

}

ukSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    uk_settings_user <- reactive({list(
      "pension" = list(
        "rate"          = input$select_pension_rate,
        "alpha_rate_1"  = input$select_alpha_rate_1,
        "alpha_rate_2"  = input$select_alpha_rate_2,
        "alpha_rate_3"  = input$select_alpha_rate_3,
        "alpha_rate_4"  = input$select_alpha_rate_4,
        "alpha_value_1" = input$select_alpha_value_1,
        "alpha_value_2" = input$select_alpha_value_2,
        "alpha_value_3" = input$select_alpha_value_3
      ),

      "national_insurance" = list(
        "rate_1"  = input$select_ni_rate_1,
        "rate_2"  = input$select_ni_rate_2,
        "rate_3"  = input$select_ni_rate_3,
        "value_1" = input$select_ni_value_1,
        "value_2" = input$select_ni_value_2
      ),

      "tax" = list(
        "rate_1"  = input$select_tax_rate_1,
        "rate_2"  = input$select_tax_rate_2,
        "rate_3"  = input$select_tax_rate_3,
        "rate_4"  = input$select_tax_rate_4,
        "value_1" = input$select_tax_value_1,
        "value_2" = input$select_tax_value_2,
        "value_3" = input$select_tax_value_3
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
