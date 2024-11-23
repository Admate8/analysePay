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

      tags$div(
        class = "header-welcome-page",
        tags$div(
          style = "width: 50%;",
          h1("analysePay", class = "display-1"), br(), br(),
          stringi::stri_rand_lipsum(1)
        ),

        tags$span(class = "add-attribution", "Background by SVGBackgrounds.com")
      ),


      bslib::layout_columns(
        col_widths = c(-2, 4, 4, -2),

        bslib::layout_columns(
          col_widths = c(-2, 8, -2),
          shinyWidgets::pickerInput(
            inputId    = "select_country_from",
            label      = tags$h1("Base", class = "display-6"),
            selected   = NULL,
            choices    = c("United Kingdom" = "uk", "Poland" = "pl"),
            choicesOpt = list(content = purrr::map2(
              c("fi fi-gb", "fi fi-pl"),
              c("United Kingdom", "Poland"),
              picker_options_with_flags
            ))
          ) |>
            tags$div(style = "text-align: center;")
        ),
        bslib::layout_columns(
          col_widths = c(-2, 8, -2),
          shinyWidgets::pickerInput(
            inputId    = "select_country_to",
            label      = tags$h1("Target", class = "display-6"),
            selected   = NULL,
            choices    = c("United Kingdom" = "uk", "Poland" = "pl"),
            choicesOpt = list(content = purrr::map2(
              c("fi fi-gb", "fi fi-pl"),
              c("United Kingdom", "Poland"),
              picker_options_with_flags
            ))
          ) |>
            tags$div(style = "text-align: center;")
        )
      ),
      uiOutput("ui_settings"),

      bslib::layout_columns(
        col_widths = c(-5, 2, -5),
        actionButton(
          inputId = "commit_input_data",
          label   = textOutput("commit_button_text"),
          style   = "font-size: 1.5rem; padding-right: 11px; padding-left: 11px;"
        )
      ),

      # bslib::layout_columns(
      #   col_widths = c(-2, 4, 4, -2),
      #   textOutput("test_output1"),
      #   textOutput("test_output2")
      # ),
      br(),
      bslib::layout_columns(
        col_widths = c(-1, 10, -1),
        tags$div(
          hr(),
          tags$h2("Earnings & Deductions by Deciles", class = "display-6")
        )

      ),

      uiOutput("ui_categories_table"),





      echarts4r::echarts4rOutput("test_plot", height = "600px")

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
    ),
    shinyjs::useShinyjs()
  )
}
