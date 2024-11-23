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
        id = "fullpage", # match with the JS id - this is the body content
        style = "width: calc(100% + 24px);",

        # Page 1 ----
        tags$div(
          class = "section welcome-page-content",

          bslib::layout_columns(
            class    = "add-left-right-margins",
            fillable = FALSE,
            fill = FALSE,
            col_widths = c(5, 7),
            tags$div(
              style = "margin-right: 20px;",
              h1("analysePay", class = "display-1"),
              # Make some room after the title
              tags$div(style = "margin-top: 100px;"),
              paste(stringi::stri_rand_lipsum(2), collapse = ". ")
            ),

            bslib::layout_columns(
              col_widths = c(6, 6, 12),
              fillable = TRUE,
              bslib::layout_columns(
                col_widths = c(-1, 10, -1),
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
                col_widths = c(-1, 10, -1),
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
              ),
              uiOutput("ui_settings")
            ) |>
              tags$div(
                class = "h-100 d-flex align-items-center",
                tags$span(class = "add-attribution", "Background by SVGBackgrounds.com")
              )

          ),
          br(),
          bslib::layout_columns(
            col_widths = c(-4, 4, -4),
            actionButton(
              inputId = "commit_input_data",
              label   = textOutput("commit_button_text"),
              style   = "font-size: 1.5rem;"
            )
          )
        ),

        # Page 2 ----
        tags$div(
          class = "section",

          br(),
          bslib::layout_columns(
            col_widths = c(6, 6, 12),
            class = "add-left-right-margins",

            tags$div(
              class = "h-100 d-flex align-items-center flex-wrap",
              tags$h2(tags$strong("Overview"), class = "display-6"),
              tags$h2("Earnings & Deductions by Deciles", class = "display-6")
            ),
            bslib::card(
              uiOutput("ui_categories_table"),
              class = "custom-card"
            ),
            echarts4r::echarts4rOutput("plot_earnings_decile_dist", height = "440px")
          )
        ),

        # Page 3 ----
        tags$div(
          class = "section",

          "Something"
        )

      ),
      # Navigation buttons ----
      tags$div(
        class = "add-navigation",
        tags$span(
          actionButton(
            inputId = "move_down",
            label   = NULL,
            icon    = shiny::icon("angle-down"),
            style   = "padding-right: 5px; padding-left: 5px; padding-top: 0; padding-bottom: 0; font-size: 1rem;"
          ),
          actionButton(
            inputId = "move_up",
            label   = NULL,
            icon    = shiny::icon("angle-up"),
            style   = "padding-right: 5px; padding-left: 5px; padding-top: 0; padding-bottom: 0; font-size: 1rem;"
          ),
          actionButton(
            inputId = "move_top",
            label   = NULL,
            icon    = shiny::icon("angles-up"),
            style   = "padding-right: 5px; padding-left: 5px; padding-top: 0; padding-bottom: 0; font-size: 1rem;"
          )
        )
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
    ),
    shinyjs::useShinyjs(),

    # Import and add the fullPage.js files
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/fullPage.js/4.0.20/fullpage.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/fullPage.js/4.0.20/fullpage.min.js")
  )
}
