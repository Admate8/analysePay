#' Set Up Common PL Settings for autonumericInput
#'
#' @noRd
uk_autonumericInput <- function(
    inputId,
    label,
    value,
    ...
) {
  shinyWidgets::autonumericInput(
    inputId                 = inputId,
    label                   = label,
    value                   = value,
    currencySymbol          = analysePay::uk_settings$global$currencySymbol,
    currencySymbolPlacement = analysePay::uk_settings$global$currencySymbolPlacement,
    decimalCharacter        = analysePay::uk_settings$global$decimalCharacter,
    digitGroupSeparator     = analysePay::uk_settings$global$digitGroupSeparator,
    minimumValue            = analysePay::uk_settings$global$minimumValue,
    style                   = "text-align: center; width: 100%;",
    ...
  )
}


ukSettingsUserUI <- function(id) {

  tags$div(
    tags$div(
      style   = glue::glue("text-align-last: right;"),
      actionButton(
        inputId = shiny::NS(id, "restore_defaults_uk"),
        label   = NULL,
        icon    = shiny::icon("rotate-right", style = "font-size: 1.5rem;"),
        width   = "26px"
      ) |>
        bslib::tooltip("Restore default settings", id = shiny::NS(id, "tt_uk_settings"))
    ),

    bslib::accordion(
      id       = shiny::NS(id, "accordion"),
      multiple = FALSE,
      width    = "100%",
      open     = FALSE,

      # Pension ----
      bslib::accordion_panel(
        title = tags$strong("Pension"),
        value = "accordion-pension",
        icon  = icon("piggy-bank", style = glue::glue("color: { palette_global$categories$pension_color }")),

        shinyWidgets::materialSwitch(
          label     = "Alpha pension scheme?",
          inputId   = shiny::NS(id, "select_extra_settings_uk"),
          value     = TRUE,
          width     = "100%"
        ) |> div_with_icon(link = analysePay::uk_settings$pension$source_alpha),

        shiny::conditionalPanel(
          condition = "input.select_extra_settings_uk == 0",
          ns        = shiny::NS(id),

          tags$h5("Contribution Rate"),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_uk_pension_rate"),
            label   = NULL,
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

          tags$h5("Contribution Rates") |> div_with_icon(link = analysePay::uk_settings$pension$source_alpha_rates),
          br(),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_uk_alpha_rates"),
            label   = NULL,
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
          ), br(), br(),

          bslib::layout_columns(
            col_widths = c(12, 4, 4, 4),
            tags$h5("Thresholds"),
            uk_autonumericInput(
              inputId = shiny::NS(id, "select_uk_alpha_value_1"),
              label   = "Lower",
              value   = analysePay::uk_settings$pension$alpha_value_1,
              decimalPlaces = 0
            ),
            uk_autonumericInput(
              inputId = shiny::NS(id, "select_uk_alpha_value_2"),
              label   = "Middle",
              value   = analysePay::uk_settings$pension$alpha_value_2,
              decimalPlaces = 0
            ),
            uk_autonumericInput(
              inputId = shiny::NS(id, "select_uk_alpha_value_3"),
              label   = "Upper",
              value   = analysePay::uk_settings$pension$alpha_value_3,
              maximumValue  = 200000,
              decimalPlaces = 0
            )
          )
        )
      ),

      # Insurance ----
      bslib::accordion_panel(
        title = tags$strong("Insurance"),
        value = "accordion-insurance",
        icon  = icon("house-chimney-crack", style = glue::glue("color: { palette_global$categories$insurance_color }")),

        tags$h4("National Insurance") |> div_with_icon(link = analysePay::uk_settings$insurance$source),
        tags$h5("Contribution Rates"), br(),
        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_uk_ni_rates"),
          label   = NULL,
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
        ), br(), br(),

        bslib::layout_columns(
          col_widths = c(12, 6, 6),
          tags$h5("Thresholds"),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_ni_value_1"),
            label   = "Allowance",
            value   = analysePay::uk_settings$insurance$value_1
          ),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_ni_value_2"),
            label   = "Basic",
            value   = analysePay::uk_settings$pension$alpha_value_2,
            maximumValue  = 200000
          )
        )
      ),

      # Tax ----
      bslib::accordion_panel(
        title = tags$strong("Income Tax"),
        value = "accordion-tax",
        icon  = icon("money-bill-wave", style = glue::glue("color: { palette_global$categories$tax_color }")),

        tags$h4("Income Tax") |> div_with_icon(link = analysePay::uk_settings$tax$source),
        tags$h5("Contribution Rates"), br(),
        shinyWidgets::noUiSliderInput(
          inputId = shiny::NS(id, "select_uk_tax_rates"),
          label   = NULL,
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
        ), br(), br(),

        bslib::layout_columns(
          col_widths = c(12, 4, 4, 4),
          tags$h5("Thresholds"),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_tax_value_1"),
            label   = "Allowance",
            value   = analysePay::uk_settings$tax$value_1,
            decimalPlaces = 0
          ),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_tax_value_2"),
            label   = "Basic",
            value   = analysePay::uk_settings$tax$value_2,
            decimalPlaces = 0
          ),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_tax_value_3"),
            label   = "Upper",
            value   = analysePay::uk_settings$pension$alpha_value_3,
            decimalPlaces = 0,
            maximumValue  = 200000
          )
        )
      ),

      # Student Loans ----
      bslib::accordion_panel(
        title = tags$strong("Student Loans"),
        value = "accordion-sl",
        icon  = icon("credit-card", style = glue::glue("color: { palette_global$categories$sl_plan2_color }")),

        bslib::layout_columns(
          col_widths = c(12, 6, 6),

          tags$h5("Contribution Rates") |> div_with_icon(link = analysePay::uk_settings$sl_plan2$source),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_uk_slp2_rate"),
            label   = "Plan 2",
            min     = 0,
            max     = 15,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::uk_settings$sl_plan2$rate
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_uk_slp3_rate"),
            label   = "Plan 3",
            min     = 0,
            max     = 15,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::uk_settings$sl_plan3$rate
          )
        ),

        bslib::layout_columns(
          col_widths = c(12, 6, 6),
          tags$h5("Repayment Thresholds"),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_slp2_value"),
            label   = "Plan 2",
            value   = analysePay::uk_settings$sl_plan2$value,
            maximumValue = 200000
          ),
          uk_autonumericInput(
            inputId = shiny::NS(id, "select_uk_slp3_value"),
            label   = "Plan 3",
            value   = analysePay::uk_settings$sl_plan3$value,
            maximumValue = 200000
          )
        )
      )
    )
  )
}

ukSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Validate the inputs ----
    iv <- shinyvalidate::InputValidator$new()

    ## Pension ----
    observeEvent(c(
      input$select_uk_alpha_value_1,
      input$select_uk_alpha_value_2,
      input$select_uk_alpha_value_3
    ), {
      iv$add_rule(
        "select_uk_alpha_value_1",
        function(value) {if (value >= input$select_uk_alpha_value_2) "Must be smaller than the middle threshold!"}
      )
      iv$add_rule(
        "select_uk_alpha_value_2",
        function(value) {
          if (value <= input$select_uk_alpha_value_1) "Must be bigger than the lower threshold!"
          else if (value >= input$select_uk_alpha_value_3) "Must be smaller than the upper threshold!"
        }
      )
      iv$add_rule(
        "select_uk_alpha_value_3",
        function(value) {if (value <= input$select_uk_alpha_value_2) "Must be bigger than the middle threshold!"}
      )
    })

    ## Insurance ----
    observeEvent(c(input$select_uk_ni_value_1, input$select_uk_ni_value_2), {
      iv$add_rule(
        "select_uk_ni_value_1",
        function(value) {if (value >= input$select_uk_ni_value_2) "Must be smaller than the basic threshold!"}
      )
      iv$add_rule(
        "select_uk_ni_value_2",
        function(value) {if (value <= input$select_uk_ni_value_1) "Must be bigger than allowance!"}
      )
    })

    ## Tax ----
    observeEvent(c(
      input$select_uk_tax_value_1,
      input$select_uk_tax_value_2,
      input$select_uk_tax_value_3
    ), {
      iv$add_rule(
        "select_uk_tax_value_1",
        function(value) {if (value >= input$select_uk_tax_value_2) "Must be smaller than the basic threshold!"}
      )
      iv$add_rule(
        "select_uk_tax_value_2",
        function(value) {
          if (value <= input$select_uk_tax_value_1) "Must be bigger than allowance!"
          else if (value >= input$select_uk_tax_value_3) "Must be smaller than the upper threshold!"
        }
      )
      iv$add_rule(
        "select_uk_tax_value_3",
        function(value) {if (value <= input$select_uk_tax_value_2) "Must be bigger than the basic threshold!"}
      )
    })

    iv$enable()

    # Reset default settings ----
    observeEvent(input$restore_defaults_uk, {
      ## Pension ----
      shinyWidgets::updateMaterialSwitch(session, "select_extra_settings_uk", value = TRUE)
      shinyWidgets::updateNoUiSliderInput(session, "select_uk_pension_rate", value = 100 * analysePay::uk_settings$pension$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_uk_alpha_rates", value = 100 * c(
        analysePay::uk_settings$pension$alpha_rate_1,
        analysePay::uk_settings$pension$alpha_rate_2,
        analysePay::uk_settings$pension$alpha_rate_3,
        analysePay::uk_settings$pension$alpha_rate_4
      ))
      shinyWidgets::updateAutonumericInput(session, "select_uk_alpha_value_1", value = analysePay::uk_settings$pension$alpha_value_1)
      shinyWidgets::updateAutonumericInput(session, "select_uk_alpha_value_2", value = analysePay::uk_settings$pension$alpha_value_2)
      shinyWidgets::updateAutonumericInput(session, "select_uk_alpha_value_3", value = analysePay::uk_settings$pension$alpha_value_3)

      ## Insurance ----
      shinyWidgets::updateNoUiSliderInput(session, "select_uk_ni_rates", value = 100 * c(
        analysePay::uk_settings$insurance$rate_1,
        analysePay::uk_settings$insurance$rate_3,
        analysePay::uk_settings$insurance$rate_2
      ))
      shinyWidgets::updateAutonumericInput(session, "select_uk_ni_value_1", value = analysePay::uk_settings$insurance$value_1)
      shinyWidgets::updateAutonumericInput(session, "select_uk_ni_value_2", value = analysePay::uk_settings$insurance$value_2)

      ## Tax ----
      shinyWidgets::updateNoUiSliderInput(session, "select_uk_tax_rates", value = 100 * c(
        analysePay::uk_settings$tax$rate_1,
        analysePay::uk_settings$tax$rate_2,
        analysePay::uk_settings$tax$rate_3,
        analysePay::uk_settings$tax$rate_4
      ))
      shinyWidgets::updateAutonumericInput(session, "select_uk_tax_value_1", value = analysePay::uk_settings$tax$value_1)
      shinyWidgets::updateAutonumericInput(session, "select_uk_tax_value_2", value = analysePay::uk_settings$tax$value_2)
      shinyWidgets::updateAutonumericInput(session, "select_uk_tax_value_3", value = analysePay::uk_settings$tax$value_3)

      # #Student Loans ----
      shinyWidgets::updateNoUiSliderInput(session, "select_uk_slp2_rate", value = 100 * analysePay::uk_settings$sl_plan2$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_uk_slp3_rate", value = 100 * analysePay::uk_settings$sl_plan3$rate)
      shinyWidgets::updateAutonumericInput(session, "select_uk_slp2_value", value = analysePay::uk_settings$sl_plan2$value)
      shinyWidgets::updateAutonumericInput(session, "select_uk_slp3_value", value = analysePay::uk_settings$sl_plan3$value)
    })


    # Return reactives ----
    list(
      "iv"       = iv,
      "settings" = reactive({list(
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
          # Note that [[2]] <-> [[3]] because the additional rate is only 2%
          # which is smaller than the base rate of 8% and at the time of developing
          # the code, noUiSliderInput doesn't allow 'unconditional' behaviour.
          "rate_1"  = input$select_uk_ni_rates[[1]] / 100,
          "rate_2"  = input$select_uk_ni_rates[[3]] / 100,
          "rate_3"  = input$select_uk_ni_rates[[2]] / 100,
          "value_1" = input$select_uk_ni_value_1,
          "value_2" = input$select_uk_ni_value_2
        ),

        "tax" = list(
          "standard_tax" = TRUE, # NOT IN USE FOR UK
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
        ),

        # Global non-reactive values
        "global" = list(
          "full_name" = uk_settings$global$full_name,
          "short_cut" = uk_settings$global$short_cut,
          "currency"  = uk_settings$global$currency,
          "locale"    = uk_settings$global$locale
        ),
        "earning_deciles" = list(
          "10th"   = uk_settings$earning_deciles$`10th`,
          "20th"   = uk_settings$earning_deciles$`20th`,
          "25th"   = uk_settings$earning_deciles$`25th`,
          "30th"   = uk_settings$earning_deciles$`30th`,
          "40th"   = uk_settings$earning_deciles$`40th`,
          "50th"   = uk_settings$earning_deciles$`50th`,
          "60th"   = uk_settings$earning_deciles$`60th`,
          "70th"   = uk_settings$earning_deciles$`70th`,
          "75th"   = uk_settings$earning_deciles$`75th`,
          "80th"   = uk_settings$earning_deciles$`80th`,
          "90th"   = uk_settings$earning_deciles$`90th`,
          "95th"   = uk_settings$earning_deciles$`95th`
        )
      )})
    )
  })
}
