plSettingsUserUI <- function(id) {

  bslib::accordion(
    multiple = FALSE,
    width    = "100%",
    open     = FALSE,

    # Pension ----
    bslib::accordion_panel(
      title = tags$strong("Pension"),
      value = "accordion-pension",
      icon  = icon("piggy-bank", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Pension - Mandatory',]$col }")),

      bslib::layout_columns(
        col_widths = c(12, 6, 6),
        tags$h4("Contribution Rates"),
        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_sk_emerytalna_rate"),
          label   = "State Pension",
          min     = 0,
          max     = 15,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * analysePay::pl_settings$pension$sk_emerytalna$rate
        ),
        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_sk_ppk_rate"),
          label   = "PPK",
          min     = 0,
          max     = 10,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * analysePay::pl_settings$pension$ppk$rate
        )
      )
    ),

    # Insurance ----
    bslib::accordion_panel(
      title = tags$strong("Insurance"),
      value = "accordion-insurance",
      icon  = icon("house-chimney-crack", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Insurance - Mandatory',]$col }")),

      tags$h4("Contribution Rates"),
      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_sk_rentowa_rate"),
        label   = "Social Insurance (Rentowa)",
        min     = 0,
        max     = 5,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * analysePay::pl_settings$insurance$sk_rentowa$rate
      ),
      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_sk_chorobowa_rate"),
        label   = "Ilness Insurance (Chorobowa)",
        min     = 0,
        max     = 5,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * analysePay::pl_settings$insurance$sk_chorobowa$rate
      ),
      shinyWidgets::noUiSliderInput(
        inputId = shiny::NS(id, "select_sk_zdrowotna_rate"),
        label   = "Health Insurance (Zdrowotna)",
        min     = 0,
        max     = 15,
        step    = 0.01,
        width   = "100%",
        format  = shinyWidgets::wNumbFormat(suffix = "%"),
        value   = 100 * analysePay::pl_settings$insurance$sk_zdrowotna$rate
      )
    ),

    # Tax ----
    bslib::accordion_panel(
      title = tags$strong("Tax"),
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

        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_pl_tax_rate"),
          label   = "Linear Tax Contribution Rate",
          min     = 0,
          max     = 35,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * analysePay::pl_settings$tax$liniowy$rate
        )
      ),

      shiny::conditionalPanel(
        condition = "input.select_extra_settings_pl == 1",
        ns        = shiny::NS(id),

        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_pl_tax_rates"),
          label   = "Linear Tax Contribution Rate",
          min     = 0,
          max     = 50,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * c(
            analysePay::pl_settings$tax$stopniowy$rate_1,
            analysePay::pl_settings$tax$stopniowy$rate_2,
            analysePay::pl_settings$tax$stopniowy$rate_3
          )
        ),

        bslib::layout_columns(
          col_widths = c(12, 6, 6),
          tags$h4("Repayment Thresholds"),
          shinyWidgets::autonumericInput(
            inputId                 = shiny::NS(id, "select_pl_tax_value_1"),
            label                   = "Allowance",
            value                   = analysePay::pl_settings$tax$stopniowy$value_1,
            currencySymbol          = "z\U0142",
            currencySymbolPlacement = "s",
            decimalCharacter        = ".",
            digitGroupSeparator     = ",",
            minimumValue            = 0,
            style                   = "text-align: center; width: 100%;"
          ),
          shinyWidgets::autonumericInput(
            inputId                 = shiny::NS(id, "select_pl_tax_value_2"),
            label                   = "Upper",
            value                   = analysePay::pl_settings$tax$stopniowy$value_2,
            currencySymbol          = "z\U0142",
            currencySymbolPlacement = "s",
            decimalCharacter        = ".",
            digitGroupSeparator     = ",",
            minimumValue            = 0,
            maximumValue            = 1000000,
            style                   = "text-align: center; width: 100%;"
          )
        )
      )
    ),

    # Student Loans ----
    bslib::accordion_panel(
      title = tags$strong("Student Loans"),
      value = "accordion-sl",
      icon  = icon("credit-card", style = glue::glue("color: { palette_cat_wide[palette_cat_wide$category == 'Student Loan',]$col }")),

      bslib::layout_columns(
        col_widths = c(12, 6, 6),
        tags$h4("Contribution Rates"),
        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_pl_slp2_rate"),
          label   = "Plan 2",
          min     = 0,
          max     = 15,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * analysePay::pl_settings$sl_plan2$rate
        ),
        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_pl_slp3_rate"),
          label   = "Plan 3",
          min     = 0,
          max     = 15,
          step    = 0.01,
          width   = "100%",
          format  = shinyWidgets::wNumbFormat(suffix = "%"),
          value   = 100 * analysePay::pl_settings$sl_plan3$rate
        )
      ),

      bslib::layout_columns(
        col_widths = c(12, 6, 6),
        tags$h4("Repayment Thresholds"),
        shinyWidgets::autonumericInput(
          inputId                 = shiny::NS(id, "select_pl_slp2_value"),
          label                   = "Plan 2",
          value                   = analysePay::pl_settings$sl_plan2$value,
          currencySymbol          = "z\U0142",
          currencySymbolPlacement = "s",
          decimalCharacter        = ".",
          digitGroupSeparator     = ",",
          minimumValue            = 0,
          maximumValue            = 1000000,
          style                   = "text-align: center; width: 100%;"
        ),
        shinyWidgets::autonumericInput(
          inputId                 = shiny::NS(id, "select_pl_slp3_value"),
          label                   = "Plan 3",
          value                   = analysePay::pl_settings$sl_plan3$value,
          currencySymbol          = "z\U0142",
          currencySymbolPlacement = "s",
          decimalCharacter        = ".",
          digitGroupSeparator     = ",",
          minimumValue            = 0,
          maximumValue            = 1000000,
          style                   = "text-align: center; width: 100%;"
        )
      )
    )
  )
}


plSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Validate the input
    iv <- shinyvalidate::InputValidator$new()

    # Tax ----
    observeEvent(c(input$select_pl_tax_value_1, input$select_pl_tax_value_2), {
      iv$add_rule(
        "select_pl_tax_value_1",
        function(value) {if (value >= input$select_pl_tax_value_2) "Must be smaller than the upper threshold!"}
      )
      iv$add_rule(
        "select_pl_tax_value_2",
        function(value) {if (value <= input$select_pl_tax_value_1) "Must be bigger than allowance!"}
      )
    })

    iv$enable()

    list(
      "iv"       = iv,
      "settings" = reactive({list(
        "pension" = list(
          "alpha_scheme"  = TRUE, # NOT IN USE FOR PL
          "sk_emerytalna" = list("rate" = input$select_sk_emerytalna_rate / 100),
          "ppk"           = list("rate" = input$select_sk_ppk_rate / 100)
        ),

        "insurance" = list(
          "sk_rentowa"   = list("rate" = input$select_sk_rentowa_rate / 100),
          "sk_chorobowa" = list("rate" = input$select_sk_chorobowa_rate / 100),
          "sk_zdrowotna" = list("rate" = input$select_sk_zdrowotna_rate / 100)
        ),

        "tax" = list(
          "standard_tax" = input$select_extra_settings_pl,
          "liniowy"      = list("rate" = input$select_pl_tax_rate / 100),
          "stopniowy"    = list(
            "rate_1"     = input$select_pl_tax_rates[[1]] / 100,
            "rate_2"     = input$select_pl_tax_rates[[2]] / 100,
            "rate_3"     = input$select_pl_tax_rates[[3]] / 100,
            "value_1"    = input$select_pl_tax_value_1,
            "value_2"    = input$select_pl_tax_value_2
          )
        ),

        "sl_plan2" = list(
          "rate"  = input$select_pl_slp2_rate / 100,
          "value" = input$select_pl_slp2_value
        ),

        "sl_plan3" = list(
          "rate"  = input$select_pl_slp3_rate / 100,
          "value" = input$select_pl_slp3_value
        )
      )})
    )

  })
}
