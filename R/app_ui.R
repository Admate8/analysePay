#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_fluid(
      h1("analysePay"),

      shiny::selectInput(
        inputId = "select_country_from",
        label = "Base",
        choices = c("United Kingdom" = "uk", "Poland" = "pl"),
        selected = NULL
      ),
      shiny::selectInput(
        inputId = "select_country_to",
        label = "Target",
        choices = c("United Kingdom" = "uk", "Poland" = "pl"),
        selected = NULL
      ),
      uiOutput("ui_settings"),

      actionButton(
        inputId = "commit_input_data",
        label   = "Analyse!"
      ),

      # tableOutput("test_table"),
      # br(),
      # tableOutput("test_table2"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        textOutput("test_output1"),
        textOutput("test_output2")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        tableOutput("test_table1"),
        tableOutput("test_table2")
      )
    )
  )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "analysePay"
    )
  )
}
