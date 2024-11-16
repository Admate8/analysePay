uk_settings_user_ui <- list(

)

ukSettingsUserUI <- function(id) {
  htmltools::tagList(
    # Pension ----
    shiny::numericInput(
      inputId = shiny::NS(id, "select_pension_rate"),
      label = "Pension Rate",
      value = uk_settings$pension$rate
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_rate_1"),
      label = "Alpha Rate Lower",
      value = uk_settings$pension$alpha_rate_1
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_rate_2"),
      label = "Alpha Rate Middle",
      value = uk_settings$pension$alpha_rate_2
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_rate_3"),
      label = "Alpha Rate Upper",
      value = uk_settings$pension$alpha_rate_3
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_rate_4"),
      label = "Alpha Rate Additional",
      value = uk_settings$pension$alpha_rate_4
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_value_1"),
      label = "Alpha Threshold Lower",
      value = uk_settings$pension$alpha_value_1
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_value_2"),
      label = "Alpha Threshold Middle",
      value = uk_settings$pension$alpha_value_2
    ),
    shiny::numericInput(
      inputId = shiny::NS(id, "select_alpha_value_3"),
      label = "Alpha Threshold Upper",
      value = uk_settings$pension$alpha_value_3
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
      )
    )})

  })
}
