#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  uk_settings_user <- ukSettingsUserServer("1")

  output$test_table <- renderTable({
    calc_uk_deductions(
      annual_earnings = c(45000, 50000, 55000, 60000),
      user_data = uk_settings_user()
    )$df_deductions_category_wide
  })
}
