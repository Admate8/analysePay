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
      theme = app_theme,
      h1("analysePay"),

      bslib::layout_columns(
        col_widths = c(6, 6),

        shinyWidgets::pickerInput(
          inputId    = "select_country_from",
          label      = "Base",
          selected   = NULL,
          choices    = c("United Kingdom" = "uk", "Poland" = "pl"),
          choicesOpt = list(content = purrr::map2(
            c("fi fi-gb", "fi fi-pl"),
            c("United Kingdom", "Poland"),
            picker_options_with_flags
          ))
        ),
        shinyWidgets::pickerInput(
          inputId    = "select_country_to",
          label      = "Target",
          selected   = NULL,
          choices    = c("United Kingdom" = "uk", "Poland" = "pl"),
          choicesOpt = list(content = purrr::map2(
            c("fi fi-gb", "fi fi-pl"),
            c("United Kingdom", "Poland"),
            picker_options_with_flags
          ))
        )
      ),
      uiOutput("ui_settings"),

      actionButton(
        inputId = "commit_input_data",
        label   = "Analyse!"
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),
        textOutput("test_output1"),
        textOutput("test_output2")
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        reactable::reactableOutput("test_table1"),
        reactable::reactableOutput("test_table2")
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
