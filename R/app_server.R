#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  uk_settings_user <- ukSettingsUserServer("1")
  pl_settings_user <- plSettingsUserServer("2")

  output$test_table <- renderTable({
    calc_uk_deductions(
      annual_earnings = c(45000, 50000, 55000, 60000),
      user_data = uk_settings_user()
    )$df_deductions_category_wide
  })

  output$test_table2 <- renderTable({
    calc_pl_deductions(
      annual_earnings = c(60000, 70000, 80000, 90000),
      user_data = pl_settings_user()
    )$df_deductions_category_wide
  })

  output$test_selection_from <- renderText({input$select_country_from})
}
