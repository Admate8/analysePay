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
      style = "display: flex; justify-content: space-between;",
      tags$div(
        class = "left-text",
        tags$span(
          shinyWidgets::switchInput(
            inputId = shiny::NS(id, "select_pl_tax_system"),
            size      = "mini",
            value     = TRUE,
            onLabel   = "Step Tax",
            offStatus = "primary",
            offLabel  = "Linear Tax",
            inline    = TRUE
          ),
          uiOutput(shiny::NS(id, "pl_tax_switch"), inline = TRUE, style = "padding-left: 10px")
        )
      ),

      tags$div(
        class = "right-text",
        actionButton(
          inputId = shiny::NS(id, "restore_defaults_pl"),
          label   = NULL,
          icon    = shiny::icon("rotate-right", style = "font-size: 1.5rem;"),
          width   = "26px"
        ) |>
          bslib::tooltip("Restore default settings", id = shiny::NS(id, "tt_pl_settings"))
      )
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

        shiny::conditionalPanel(
          condition = "input.select_pl_tax_system == 1",
          ns        = shiny::NS(id),

          bslib::layout_columns(
            col_widths = c(12, 12, 6, 6),
            tags$h5("Contribution Rates"),
            shinyWidgets::noUiSliderInput(
              inputId = shiny::NS(id, "select_sk_emerytalna_rate"),
              label   = "State Pension" |> div_with_icon(link = analysePay::pl_settings$pension$sk_emerytalna$source),
              min     = 0,
              max     = 15,
              step    = 0.01,
              width   = "100%",
              format  = shinyWidgets::wNumbFormat(suffix = "%"),
              value   = 100 * analysePay::pl_settings$pension$sk_emerytalna$rate
            ),
            shinyWidgets::noUiSliderInput(
              inputId = shiny::NS(id, "select_sk_ppk_rate"),
              label   = "PPK Pension" |> div_with_icon(link = analysePay::pl_settings$pension$ppk$source),
              min     = 0,
              max     = 4,
              step    = 0.01,
              width   = "100%",
              format  = shinyWidgets::wNumbFormat(suffix = "%"),
              value   = 100 * analysePay::pl_settings$pension$ppk$rate
            ),
            shinyWidgets::noUiSliderInput(
              inputId = shiny::NS(id, "select_sk_ppk_emp_rate"),
              label   = "PPK Employer" |> div_with_icon(link = analysePay::pl_settings$pension$ppk$source),
              min     = 0,
              max     = 5,
              step    = 0.01,
              width   = "100%",
              format  = shinyWidgets::wNumbFormat(suffix = "%"),
              value   = 100 * analysePay::pl_settings$pension$ppk$rate_employer
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.select_pl_tax_system == 0",
          ns        = shiny::NS(id),

          tags$h5("Contribution Rate"),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_emerytalna_rate_linear"),
            label   = "State Pension" |> div_with_icon(link = analysePay::pl_settings$pension$sk_emerytalna$source),
            min     = 15,
            max     = 25,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$pension$sk_emerytalna$rate_linear
          )
        )
      ),

      # Insurance ----
      bslib::accordion_panel(
        title = tags$strong("Insurance"),
        value = "accordion-insurance",
        icon  = icon("house-chimney-crack", style = glue::glue("color: { palette_global$categories$insurance_color }")),

        shiny::conditionalPanel(
          condition = "input.select_pl_tax_system == 1",
          ns        = shiny::NS(id),

          tags$h5("Contribution Rates") |> div_with_icon(link = analysePay::pl_settings$insurance$sk_rentowa$source),
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
        shiny::conditionalPanel(
          condition = "input.select_pl_tax_system == 0",
          ns        = shiny::NS(id),

          tags$h5("Contribution Rates") |> div_with_icon(link = analysePay::pl_settings$insurance$sk_rentowa$source),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_rentowa_rate_linear"),
            label   = "Social Insurance (Rentowa)",
            min     = 5,
            max     = 15,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$insurance$sk_rentowa$rate_linear
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_chorobowa_rate_linear"),
            label   = "Illness Insurance (Chorobowa)",
            min     = 0,
            max     = 5,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$insurance$sk_chorobowa$rate_linear
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_zdrowotna_rate_linear"),
            label   = "Health Insurance (Zdrowotna)",
            min     = 0,
            max     = 10,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$insurance$sk_zdrowotna$rate_linear
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_wypadkowa_rate_linear"),
            label   = "Accident Insurance (Wypadkowa)" |> div_with_icon(link = analysePay::pl_settings$insurance$sk_wypadkowa$source),
            min     = 0.67,
            max     = 3.33,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$insurance$sk_wypadkowa$rate
          ),
          shinyWidgets::noUiSliderInput(
            inputId = shiny::NS(id, "select_sk_fpfs_rate_linear"),
            label   = "FP, FS and FG\U015AP" |> div_with_icon(link = analysePay::pl_settings$insurance$sk_fpfs$source),
            min     = 2,
            max     = 3,
            step    = 0.01,
            width   = "100%",
            format  = shinyWidgets::wNumbFormat(suffix = "%"),
            value   = 100 * analysePay::pl_settings$insurance$sk_fpfs$rate
          )
        )
      ),

      # Tax ----
      bslib::accordion_panel(
        title = tags$strong("Income Tax"),
        value = "accordion-tax",
        icon  = icon("money-bill-wave", style = glue::glue("color: { palette_global$categories$tax_color }")),

        shiny::conditionalPanel(
          condition = "input.select_pl_tax_system == 1",
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
        ),

        shiny::conditionalPanel(
          condition = "input.select_pl_tax_system == 0",
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
            label   = div_with_icon("Plan 2", link = analysePay::pl_settings$sl_plan2$source, flex = "block"),
            value   = analysePay::pl_settings$sl_plan2$value
          ),
          pl_autonumericInput(
            inputId = shiny::NS(id, "select_pl_slp3_value"),
            label   = div_with_icon("Plan 3", link = analysePay::pl_settings$sl_plan3$source, flex = "block"),
            value   = analysePay::pl_settings$sl_plan3$value
          )
        )
      )
    )
  )
}


plSettingsUserServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Links on tax system ----
    output$pl_tax_switch <- renderUI({
      if (input$select_pl_tax_system == TRUE) {
        tags$a(
          href = analysePay::pl_settings$tax$stopniowy$source,
          target = "_blank",
          shiny::icon(
            "info-circle",
            style = glue::glue("font-size: 1rem; color: { palette_global$body_color };")
          )
        ) |> bslib::tooltip("What is it?", id = "tt_pl_tax")
      } else {
        tags$a(
          href = analysePay::pl_settings$tax$liniowy$source,
          target = "_blank",
          shiny::icon(
            "info-circle",
            style = glue::glue("font-size: 1rem; color: { palette_global$body_color };")
          )
        ) |> bslib::tooltip(
          shiny::HTML(glue::glue(
            "The minimum base for social deductions in financial year 2024/25 is <br>
            { scales::comma(
              analysePay::pl_settings$tax$liniowy$social_deductions_base,
              suffix = \"z\U0142\", decimal.mark = \",\", big.mark = \" \", accuracy = 0.01
            ) }. <br><br>Click to find out more!
            "
          ))
        )
      }
    })


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
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_emerytalna_rate_linear", value = 100 * analysePay::pl_settings$pension$sk_emerytalna$rate_linear)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_ppk_rate", value = 100 * analysePay::pl_settings$pension$ppk$rate)

      ## Insurance ----
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_rentowa_rate", value = 100 * analysePay::pl_settings$insurance$sk_rentowa$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_rentowa_rate_linear", value = 100 * analysePay::pl_settings$insurance$sk_rentowa$rate_linear)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_chorobowa_rate", value = 100 * analysePay::pl_settings$insurance$sk_chorobowa$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_chorobowa_rate_linear", value = 100 * analysePay::pl_settings$insurance$sk_chorobowa$rate_linear)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_zdrowotna_rate", value = 100 * analysePay::pl_settings$insurance$sk_zdrowotna$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_zdrowotna_rate_linear", value = 100 * analysePay::pl_settings$insurance$sk_zdrowotna$rate_linear)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_wypadkowa_rate_linear", value = 100 * analysePay::pl_settings$insurance$sk_wypadkowa$rate)
      shinyWidgets::updateNoUiSliderInput(session, "select_sk_fpfs_rate_linear", value = 100 * analysePay::pl_settings$insurance$sk_fpfs$rate)

      ## Tax ----
      shinyWidgets::updateMaterialSwitch(session, "select_pl_tax_system", value = TRUE)
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
          "sk_emerytalna" = list(
            "rate"        = input$select_sk_emerytalna_rate / 100,
            "rate_linear" = input$select_sk_emerytalna_rate_linear / 100
          ),
          "ppk"           = list(
            "rate"          = input$select_sk_ppk_rate / 100,
            "rate_employer" = input$select_sk_ppk_emp_rate / 100
          )
        ),

        "insurance" = list(
          "sk_rentowa"   = list(
            "rate"        = input$select_sk_rentowa_rate / 100,
            "rate_linear" = input$select_sk_rentowa_rate_linear / 100
          ),
          "sk_chorobowa" = list(
            "rate"        = input$select_sk_chorobowa_rate / 100,
            "rate_linear" = input$select_sk_chorobowa_rate_linear / 100
          ),
          "sk_zdrowotna" = list(
            "rate"        = input$select_sk_zdrowotna_rate / 100,
            "rate_linear" = input$select_sk_zdrowotna_rate_linear / 100
          ),
          "sk_wypadkowa" = list("rate" = input$select_sk_wypadkowa_rate_linear / 100),
          "sk_fpfs"      = list("rate" = input$select_sk_fpfs_rate_linear / 100)
        ),

        "tax" = list(
          "standard_tax" = input$select_pl_tax_system,
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
          "full_name"               = pl_settings$global$full_name,
          "short_cut"               = pl_settings$global$short_cut,
          "currency"                = pl_settings$global$currency,
          "locale"                  = pl_settings$global$locale,
          "currencySymbol"          = pl_settings$global$currencySymbol,
          "currencySymbolPlacement" = pl_settings$global$currencySymbolPlacement,
          "decimalCharacter"        = pl_settings$global$decimalCharacter,
          "digitGroupSeparator"     = pl_settings$global$digitGroupSeparator
        ),
        "earning_deciles" = tibble::tribble(
          ~decile, ~value,
          10, with(pl_settings$earning_deciles, value[decile == 10]),
          20, with(pl_settings$earning_deciles, value[decile == 20]),
          30, with(pl_settings$earning_deciles, value[decile == 30]),
          40, with(pl_settings$earning_deciles, value[decile == 40]),
          50, with(pl_settings$earning_deciles, value[decile == 50]),
          60, with(pl_settings$earning_deciles, value[decile == 60]),
          70, with(pl_settings$earning_deciles, value[decile == 70]),
          80, with(pl_settings$earning_deciles, value[decile == 80]),
          90, with(pl_settings$earning_deciles, value[decile == 90]),
          95, with(pl_settings$earning_deciles, value[decile == 95])
        )
      )})
    )

  })
}
