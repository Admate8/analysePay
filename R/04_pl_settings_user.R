#' Set Up Common PL Settings for autonumericInput
#'
#' @noRd
pl_autonumericInput <- function(
    inputId,
    label,
    value,
    ...
  ) {
  shinyWidgets::autonumericInput(
    inputId                 = inputId,
    label                   = label,
    value                   = value,
    currencySymbol          = analysePay::pl_settings$global$currencySymbol,
    currencySymbolPlacement = analysePay::pl_settings$global$currencySymbolPlacement,
    decimalCharacter        = analysePay::pl_settings$global$decimalCharacter,
    digitGroupSeparator     = analysePay::pl_settings$global$digitGroupSeparator,
    minimumValue            = analysePay::pl_settings$global$minimumValue,
    maximumValue            = analysePay::pl_settings$global$maximumValue,
    style                   = "text-align: center; width: 100%;",
    ...
  )
}


plSettingsUserUI <- function(id) {

  tags$div(
    tags$div(
      style   = glue::glue("text-align-last: right;"),
      actionButton(
        inputId = shiny::NS(id, "restore_defaults_pl"),
        label   = NULL,
        icon    = shiny::icon("wrench", style = "font-size: 1.5rem;"),
        width   = "26px"
      ) |>
        bslib::tooltip("Restore default settings", id = shiny::NS(id, "tt_pl_settings"))
    ),

    bslib::accordion(
      multiple = FALSE,
      width    = "100%",
      open     = FALSE,

      # Pension ----
      bslib::accordion_panel(
        title = tags$strong("Pension"),
        value = "accordion-pension",
        icon  = icon("piggy-bank", style = glue::glue("color: { palette_global$categories$pension_color }")),

        bslib::layout_columns(
          col_widths = c(12, 12, 6, 6),
          tags$h5("Contribution Rates"),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_emerytalna_rate"),
            label   = label_with_popover(
              "State Pension",
              glue::glue("<a href={ analysePay::pl_settings$pension$sk_emerytalna$source } target='_blank'>Find out more!</a>")
            ),
            min     = 0,
            max     = 15,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$pension$sk_emerytalna$rate
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_ppk_rate"),
            label   = label_with_popover(
              "PPK Pension",
              glue::glue("<a href={ analysePay::pl_settings$pension$ppk$source } target='_blank'>Find out more!</a>")
            ),
            min     = 0,
            max     = 4,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$pension$ppk$rate
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_ppk_emp_rate"),
            label   = label_with_popover(
              "PPK Employer",
              glue::glue("<a href={ analysePay::pl_settings$pension$ppk$source } target='_blank'>Find out more!</a>")
            ),
            min     = 0,
            max     = 5,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$pension$ppk$rate_employer
          )
        )
      ),

      # Insurance ----
      bslib::accordion_panel(
        title = tags$strong("Insurance"),
        value = "accordion-insurance",
        icon  = icon("house-chimney-crack", style = glue::glue("color: { palette_global$categories$insurance_color }")),

        title_with_popover(
          "Contribution Rates", size = 5,
          glue::glue("<a href={ analysePay::pl_settings$insurance$sk_rentowa$source } target='_blank'>Find out more!</a>")
        ),
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
          label   = "Illness Insurance (Chorobowa)",
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
        title = tags$strong("Income Tax"),
        value = "accordion-tax",
        icon  = icon("money-bill-wave", style = glue::glue("color: { palette_global$categories$tax_color }")),

        shinyWidgets::materialSwitch(
          label     = label_with_popover(
            "Step tax?",
            glue::glue("<a href={ analysePay::pl_settings$tax$stopniowy$source } target='_blank'>Find out more!</a>")
          ),
          inputId   = shiny::NS(id, "select_extra_settings_pl"),
          value     = TRUE,
          width     = "100%"
        ),

        shiny::conditionalPanel(
          condition = "input.select_extra_settings_pl == 0",
          ns        = shiny::NS(id),

          tags$h5("Contribution Rate"),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_pl_tax_rate"),
            label   = NULL,
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

          tags$h5("Contribution Rate"), br(),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_pl_tax_rates"),
            label   = NULL,
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
          ), br(), br(),

          bslib::layout_columns(
            col_widths = c(12, 6, 6),
            tags$h5("Thresholds"),
            pl_autonumericInput(
              inputId = shiny::NS(id, "select_pl_tax_value_1"),
              label   = "Allowance",
              value   = analysePay::pl_settings$tax$stopniowy$value_1
            ),
            pl_autonumericInput(
              inputId = shiny::NS(id, "select_pl_tax_value_2"),
              label   = "Upper",
              value   = analysePay::pl_settings$tax$stopniowy$value_2
            )
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
          title_with_popover(
            "Contribution Rates", size = 5,
            glue::glue("<a href={ analysePay::uk_settings$sl_plan2$source } target='_blank'>Find out more!</a>")
          ),
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
          tags$h5("Repayment Thresholds"),
          pl_autonumericInput(
            inputId = shiny::NS(id, "select_pl_slp2_value"),
            label   = label_with_popover(
              "Plan 2",
              glue::glue("<a href={ analysePay::pl_settings$sl_plan2$source } target='_blank'>Find out more!</a>")
            ),
            value   = analysePay::pl_settings$sl_plan2$value
          ),
          pl_autonumericInput(
            inputId = shiny::NS(id, "select_pl_slp3_value"),
            label   = label_with_popover(
              "Plan 3",
              glue::glue("<a href={ analysePay::pl_settings$sl_plan3$source } target='_blank'>Find out more!</a>")
            ),
            value   = analysePay::pl_settings$sl_plan3$value
          )
        )
      )
    )
  )
}


plSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Validate the inputs ----
    iv <- shinyvalidate::InputValidator$new()

    ## Tax ----
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

    # Reset default settings ----
    observeEvent(input$restore_defaults_pl, {
      ## Pension ----
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_emerytalna_rate", value = 100 * analysePay::pl_settings$pension$sk_emerytalna$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_ppk_rate", value = 100 * analysePay::pl_settings$pension$ppk$rate)

      ## Insurance ----
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_rentowa_rate", value = 100 * analysePay::pl_settings$insurance$sk_rentowa$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_chorobowa_rate", value = 100 * analysePay::pl_settings$insurance$sk_chorobowa$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_zdrowotna_rate", value = 100 * analysePay::pl_settings$insurance$sk_zdrowotna$rate)

      ## Tax ----
      shinyWidgets::updateMaterialSwitch(session, "select_extra_settings_pl", value = TRUE)
      shinyWidgets::updateNoUiSliderInput(session, "select_pl_tax_rate", value = 100 * analysePay::pl_settings$tax$liniowy$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_pl_tax_rates", value = 100 * c(
        analysePay::pl_settings$tax$stopniowy$rate_1,
        analysePay::pl_settings$tax$stopniowy$rate_2,
        analysePay::pl_settings$tax$stopniowy$rate_3
      ))
      shinyWidgets::updateAutonumericInput(session, "select_pl_tax_value_1", value = analysePay::pl_settings$tax$stopniowy$value_1)
      shinyWidgets::updateAutonumericInput(session, "select_pl_tax_value_2", value = analysePay::pl_settings$tax$stopniowy$value_2)

      ## Student Loans ----
      shinyWidgets::updateNoUiSliderInput(session, "select_pl_slp2_rate", value = 100 * analysePay::pl_settings$sl_plan2$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_pl_slp3_rate", value = 100 * analysePay::pl_settings$sl_plan3$rate)
      shinyWidgets::updateAutonumericInput(session, "select_pl_slp2_value", value = analysePay::pl_settings$sl_plan2$value)
      shinyWidgets::updateAutonumericInput(session, "select_pl_slp3_value", value = analysePay::pl_settings$sl_plan3$value)
    })

    # Return reactives ----
    list(
      "iv"       = iv,
      "settings" = reactive({list(
        "pension" = list(
          "alpha_scheme"  = TRUE, # NOT IN USE FOR PL
          "sk_emerytalna" = list("rate" = input$select_sk_emerytalna_rate / 100),
          "ppk"           = list(
            "rate"          = input$select_sk_ppk_rate / 100,
            "rate_employer" = input$select_sk_ppk_emp_rate / 100
          )
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
        ),

        # Global non-reactive values
        "global" = list(
          "full_name" = pl_settings$global$full_name,
          "short_cut" = pl_settings$global$short_cut,
          "currency"  = pl_settings$global$currency,
          "locale"    = pl_settings$global$locale
        ),
        "earning_deciles" = list(
          "10th"   = pl_settings$earning_deciles$`10th`,
          "20th"   = pl_settings$earning_deciles$`20th`,
          "30th"   = pl_settings$earning_deciles$`30th`,
          "40th"   = pl_settings$earning_deciles$`40th`,
          "50th"   = pl_settings$earning_deciles$`50th`,
          "60th"   = pl_settings$earning_deciles$`60th`,
          "70th"   = pl_settings$earning_deciles$`70th`,
          "80th"   = pl_settings$earning_deciles$`80th`,
          "90th"   = pl_settings$earning_deciles$`90th`,
          "95th"   = pl_settings$earning_deciles$`95th`
        )
      )})
    )

  })
}
