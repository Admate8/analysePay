
ukSettingsUserUI <- function(id) {

  bslib::accordion(
    multiple = FALSE,
    width    = "100%",
    open     = FALSE,

    # Pension ----
    bslib::accordion_panel(
      title = tags$strong("Pension"),
      value = "accordion-pension",
      icon  = icon("piggy-bank", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Pension - Mandatory',]$col }")),

      shinyWidgets::materialSwitch(
        label     = "Alpha pension scheme?",
        inputId   = shiny::NS(id, "select_extra_settings_uk"),
        value     = TRUE,
        width     = "100%"
      ),

      shiny::conditionalPanel(
        condition = "input.select_extra_settings_uk == 0",
        ns        = shiny::NS(id),

        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_uk_pension_rate"),
          label   = "Contribution Rate",
          min     = 0,
          max     = 5,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * analysePay::uk_settings$pension$rate
        )
      ),

      shiny::conditionalPanel(
        condition = "input.select_extra_settings_uk == 1",
        ns        = shiny::NS(id),

        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_uk_alpha_rates"),
          label   = "Pension Rates",
          min     = 0,
          max     = 10,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * c(
            analysePay::uk_settings$pension$alpha_rate_1,
            analysePay::uk_settings$pension$alpha_rate_2,
            analysePay::uk_settings$pension$alpha_rate_3,
            analysePay::uk_settings$pension$alpha_rate_4
          )
        ), br(),

        shiny::numericInput(
          inputId = shiny::NS(id, "select_uk_alpha_value_1"),
          label = "Alpha Threshold Lower",
          value = analysePay::uk_settings$pension$alpha_value_1
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "select_uk_alpha_value_2"),
          label = "Alpha Threshold Middle",
          value = analysePay::uk_settings$pension$alpha_value_2
        ),
        shiny::numericInput(
          inputId = shiny::NS(id, "select_uk_alpha_value_3"),
          label = "Alpha Threshold Upper",
          value = analysePay::uk_settings$pension$alpha_value_3
        )
      )
    ),

    # Insurance ----
    bslib::accordion_panel(
      title = tags$strong("Insurance"),
      value = "accordion-insurance",
      icon  = icon("house-chimney-crack", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Insurance - Mandatory',]$col }")),

      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_uk_ni_rates"),
        label   = "National Insurance Rates",
        min     = 0,
        max     = 15,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * c(
          analysePay::uk_settings$insurance$rate_1,
          analysePay::uk_settings$insurance$rate_3,
          analysePay::uk_settings$insurance$rate_2
        )
      ), br(),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_ni_value_1"),
        label = "Threshold Lower",
        value = analysePay::uk_settings$insurance$value_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_ni_value_2"),
        label = "Threshold Upper",
        value = analysePay::uk_settings$insurance$value_2
      )
    ),

    # Tax ----
    bslib::accordion_panel(
      title = tags$strong("Tax"),
      value = "accordion-tax",
      icon  = icon("money-bill-wave", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Tax',]$col }")),

      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_uk_tax_rates"),
        label   = "Income Tax Rates",
        min     = 0,
        max     = 50,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * c(
          analysePay::uk_settings$tax$rate_1,
          analysePay::uk_settings$tax$rate_2,
          analysePay::uk_settings$tax$rate_3,
          analysePay::uk_settings$tax$rate_4
        )
      ), br(),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_tax_value_1"),
        label = "Threshold Lower",
        value = analysePay::uk_settings$tax$value_1
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_tax_value_2"),
        label = "Threshold Middle",
        value = analysePay::uk_settings$tax$value_2
      ),
      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_tax_value_3"),
        label = "Threshold Upper",
        value = analysePay::uk_settings$tax$value_3
      )
    ),

    # Student Loan Plan 2 ----
    bslib::accordion_panel(
      title = tags$strong("Student Loan Plan 2"),
      value = "accordion-slp2",
      icon  = icon("credit-card", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Student Loan',]$col }")),

      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_uk_slp2_rate"),
        label   = "Contribution Rate",
        min     = 0,
        max     = 15,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * analysePay::uk_settings$sl_plan2$rate
      ), br(),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_slp2_value"),
        label = "Threshold",
        value = analysePay::uk_settings$sl_plan2$value
      )
    ),

    # Student Loan Plan 3 ----
    bslib::accordion_panel(
      title = tags$strong("Student Loan Plan 3"),
      value = "accordion-slp3",
      icon  = icon("credit-card", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Student Loan',]$col }")),

      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_uk_slp3_rate"),
        label   = "Contribution Rate",
        min     = 0,
        max     = 15,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * analysePay::uk_settings$sl_plan3$rate
      ), br(),

      shiny::numericInput(
        inputId = shiny::NS(id, "select_uk_slp3_value"),
        label = "Threshold",
        value = analysePay::uk_settings$sl_plan3$value
      )
    )
  )
}

ukSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    reactive({list(
      "pension" = list(
        "alpha_scheme"  = input$select_extra_settings_uk,
        "rate"          = input$select_uk_pension_rate / 100,
        "alpha_rate_1"  = input$select_uk_alpha_rates[[1]] / 100,
        "alpha_rate_2"  = input$select_uk_alpha_rates[[2]] / 100,
        "alpha_rate_3"  = input$select_uk_alpha_rates[[3]] / 100,
        "alpha_rate_4"  = input$select_uk_alpha_rates[[4]] / 100,
        "alpha_value_1" = input$select_uk_alpha_value_1,
        "alpha_value_2" = input$select_uk_alpha_value_2,
        "alpha_value_3" = input$select_uk_alpha_value_3
      ),

      "insurance" = list(
        # Note that 2<->3 because the additional rate is only 2%
        "rate_1"  = input$select_uk_ni_rates[[1]] / 100,
        "rate_2"  = input$select_uk_ni_rates[[3]] / 100,
        "rate_3"  = input$select_uk_ni_rates[[2]] / 100,
        "value_1" = input$select_uk_ni_value_1,
        "value_2" = input$select_uk_ni_value_2
      ),

      "tax" = list(
        "standard_tax" = TRUE, # NOT IN USE
        "rate_1"       = input$select_uk_tax_rates[[1]] / 100,
        "rate_2"       = input$select_uk_tax_rates[[2]] / 100,
        "rate_3"       = input$select_uk_tax_rates[[3]] / 100,
        "rate_4"       = input$select_uk_tax_rates[[4]] / 100,
        "value_1"      = input$select_uk_tax_value_1,
        "value_2"      = input$select_uk_tax_value_2,
        "value_3"      = input$select_uk_tax_value_3
      ),

      "sl_plan2" = list(
        "rate"  = input$select_uk_slp2_rate / 100,
        "value" = input$select_uk_slp2_value
      ),

      "sl_plan3" = list(
        "rate"  = input$select_uk_slp3_rate / 100,
        "value" = input$select_uk_slp3_value
      )
    )})
  })
}
