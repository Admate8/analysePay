#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Preserve inputs ----
  settings_from <- NULL
  settings_to   <- NULL
  makeReactiveBinding("settings_from")
  makeReactiveBinding("settings_to")
  makeReactiveBinding("iv_from")
  makeReactiveBinding("iv_to")
  ns_from <- reactiveVal("1")
  ns_to   <- reactiveVal("1")

  # Update country_from and country_to settings depending on the user's selection
  observeEvent(c(input$select_country_from, input$select_country_to), {

    # If country_from == country_to we need to make sure both setting options
    # are independent, i.e. they have different namespaces. Otherwise, it does
    # not matter
    if (input$select_country_from == input$select_country_to) {
      ns_from("1")
      ns_to("2")
    }

    # Update the UI
    ui_settings_from_name <- paste0(input$select_country_from, "SettingsUserUI")
    ui_settings_to_name   <- paste0(input$select_country_to, "SettingsUserUI")

    output$ui_settings <- renderUI({
      bslib::layout_columns(
        col_widths = c(-2, 4, 4, -2),
        base::get(ui_settings_from_name)(ns_from()),
        base::get(ui_settings_to_name)(ns_to())
      )
    })

    # Update server-global settings
    # Define universal settings to avoid multiple if-else conditions
    # As long as {country}SettingsUserServer has the same name, we can do this.
    server_settings_from_name <- paste0(input$select_country_from, "SettingsUserServer")
    server_settings_to_name   <- paste0(input$select_country_to, "SettingsUserServer")

    settings_from <<- base::get(server_settings_from_name)(ns_from())$settings
    settings_to   <<- base::get(server_settings_to_name)(ns_to())$settings

    iv_from <<- base::get(server_settings_from_name)(ns_from())$iv
    iv_to   <<- base::get(server_settings_to_name)(ns_to())$iv
  })

  # Commit settings button ----
  observe({
    if (all(iv_from$is_valid(), iv_to$is_valid())) {
      shinyjs::enable("commit_input_data")
      output$commit_button_text <- renderText({"Analyse!"})
    } else {
      shinyjs::disable("commit_input_data")
      output$commit_button_text <- renderText({"Invalid Settings!"})
    }
  })




  df_deductions <- eventReactive(input$commit_input_data, {

    function_from_name <- paste0("calc_", input$select_country_from, "_deductions")
    function_to_name   <- paste0("calc_", input$select_country_to, "_deductions")

    list(
      "from_wide" = base::get(function_from_name)(
        annual_earnings = c(50000, 55000, 90000),
        alpha_scheme = settings_from()$pension$alpha_scheme,
        standard_tax = settings_from()$tax$standard_tax,
        user_data = settings_from()
      )$df_deductions_category_wide,

      "to_wide" = base::get(function_to_name)(
        annual_earnings = c(50000, 55000, 60000),
        alpha_scheme = settings_to()$pension$alpha_scheme,
        standard_tax = settings_to()$tax$standard_tax,
        user_data = settings_to()
      )$df_deductions_category_wide
    )
  })

  output$test_output1 <- renderText({paste0(unlist(settings_from(), recursive = TRUE), collapse = ", ")})
  output$test_output2 <- renderText({paste0(unlist(settings_to(), recursive = TRUE), collapse = ", ")})

  output$test_table1 <- reactable::renderReactable({reactable::reactable(df_deductions()$from_wide)})
  output$test_table2 <- reactable::renderReactable({reactable::reactable(df_deductions()$to_wide)})

  output$test_iv_from <- renderText({iv_from$is_valid()})
  output$test_iv_to <- renderText({iv_to$is_valid()})
  output$test_both_ivs <- renderText({all(iv_from$is_valid(), iv_to$is_valid())})
}
