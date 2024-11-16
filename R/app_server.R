#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  uk_settings_user <- ukSettingsUserServer("1")

  output$test_text <- renderText({
    paste(unlist(uk_settings_user(), recursive = TRUE), collapse = ", ")
  })
}
