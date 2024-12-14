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
              tags$div(style = "margin-top: 80px;"),
              tags$h1("Picture this:"),
              tags$ul(
                tags$li(class = "text-justify", style = "margin-bottom: 10px;", "The National Insurance rates are tweaked again, and headlines scream about how the \"average worker\" will save \U00A3xxx. But wait... you don't earn the median salary. So, what does it actually mean for your pocket?"),
                tags$li(class = "text-justify", style = "margin-bottom: 10px;", "The student loan repayment threshold stays frozen for another year. How much more will you be shelling out so the Treasury can balance its books? (Hint: probably more than you'd like!)"),
                tags$li(class = "text-justify", "Now, let's spice it up - you get a job offer abroad (just Poland for now, but stay with me here). How do you figure out if the move makes sense financially? What does life after tax and costs look like in another country compared to the UK?")
              ),
              br(),
              tags$h3("That's where analysePay steps in!"),
              tags$p(class = "text-justify", "Play around with policy changes, compare earnings at home and abroad, and finally uncover just how much of your paycheck goes straight to taxes..."),
              br(),
              bslib::layout_columns(
                col_widths = c(-2, 8, -2),
                actionButton(
                  inputId = "commit_input_data",
                  label   = textOutput("commit_button_text"),
                  style   = "font-size: 1.5rem;"
                )
              )
            ),

            bslib::layout_columns(
              col_widths = c(6, 6, 12, -2, 8, -2),
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
              uiOutput("ui_settings"),

              tags$div(
                style = "position: relative;",
                tags$div(
                  style = "position: absolute; right: 0; top: 0; margin-top: 10px; margin-right: 10px;",
                  actionButton(
                    inputId = "restore_defaults_earnings",
                    label   = NULL,
                    icon    = shiny::icon("rotate-right", style = "font-size: 1.2rem;"),
                    width   = "26px"
                  ) |>
                    bslib::tooltip("Restore default settings")
                ),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  class = "h-100 d-flex align-items-center custom-card",

                  tags$div(
                    shinyWidgets::switchInput(
                      inputId   = "select_percentile_or_earnings",
                      size      = "mini",
                      value     = TRUE,
                      onLabel   = "Percentile",
                      offStatus = "primary",
                      offLabel  = "Earnings",
                      inline    = TRUE
                    ),
                    conditionalPanel(
                      condition = "input.select_percentile_or_earnings == 1",
                      shinyWidgets::autonumericInput(
                        inputId                 = "provide_percentile",
                        label                   = "Base earning percentile\U2800" |>
                          div_with_icon(link = NULL, tt_text = "If your earnings are in the nth percentile,
                                        n% of the working population earns less than or the same amount as you."),
                        value                   = 50,
                        currencySymbol          = "th",
                        currencySymbolPlacement = "s",
                        decimalPlaces           = 0,
                        style                   = "text-align: center; width: 100%;"
                      )
                    ),
                    conditionalPanel(
                      condition = "input.select_percentile_or_earnings == 0",
                      shinyWidgets::autonumericInput(
                        inputId                 = "provide_annual_earnings",
                        label                   = "Base annual earnings\U2800" |>
                          div_with_icon(link = NULL, tt_text = "As not all earning percentiles are published,
                            the annual earnings you provide will be mapped onto an approximated percentile,
                            giving slightly different earnings."),
                        value                   = 34632,
                        currencySymbol          = "\U00A3",
                        currencySymbolPlacement = "p",
                        decimalCharacter        = ".",
                        digitGroupSeparator     = ",",
                        style                   = "text-align: center; width: 100%;"
                      )
                    )
                  ),
                  tags$div(
                    style = "display: flex; justify-content: center;",
                    shinyWidgets::radioGroupButtons(
                      inputId    = "select_calc_period",
                      label      = "Show results by",
                      choices    = c("Year" = "year", "Month" = "month", "Week" = "week"),
                      selected   = "year",
                      individual = TRUE,
                      size       = "sm",
                      justified  = TRUE
                    )
                  )
                )
              )
            ) |>
              tags$div(
                class = "h-100 d-flex align-items-center",
                tags$span(class = "add-attribution", "Background by SVGBackgrounds.com")
              )
          )
        ),

        # Page 2 ----
        tags$div(
          class = "section",

          ## Slide 1 NEW ----
          tags$div(
            class = "slide",
            bslib::layout_columns(
              class = "add-left-right-margins",
              col_widths = c(5, 7),
              tags$div(
                tags$h2("Earnings & Deductions", class = "display-6"),
                br(),
                uiOutput("ui_earnings_cards")
              )
            )
          ),

          tags$div(
            class = "slide",
            "METHODOLOGY"
          ),

          ## Slide 1 ----
          tags$div(
            class = "slide",
            br(),
            bslib::layout_columns(
              col_widths = c(6, 6, 12),
              class = "add-left-right-margins",

              tags$div(
                class = "h-100 d-flex align-items-center flex-wrap",
                tags$h2(tags$strong("Overview"), class = "display-6"),
                br(),
                tags$h2("Earnings & Deductions by Percentiles", class = "display-6")
              ),
              bslib::card(
                uiOutput("ui_categories_table"),
                class = "custom-card"
              ),
              echarts4r::echarts4rOutput("plot_earnings_percentile_dist", height = "27.5rem") |> custom_spinner()
            )
          ),
          ## Slide 2 ----
          tags$div(
            class = "slide",
            bslib::layout_columns(
              col_widths = c(8, 4),
              class = "add-left-right-margins",

              tags$div(
                style = "position: relative;",
                echarts4r::echarts4rOutput("plot_int_earnings_percentile_dist", height = "48rem") |> custom_spinner(),

                tags$div(
                  style = "position: absolute; left: 0; top: 0; z-index: 20;",
                  uiOutput("ui_earnings_sources")
                )
              ),



              bslib::layout_columns(
                col_widths = c(12, 12, 12),
                tags$div(
                  class = "h-100 d-flex align-items-center flex-wrap",
                  tags$h2("Interpolated Earnings", class = "display-6")
                ),

                echarts4r::echarts4rOutput("plot_radar_perc", height = "30rem")
              )
            )
          )
        ),

        # Last page ----
        tags$div(
          class = "section close-page-content",

          bslib::layout_columns(
            class = "add-left-right-margins",
            col_widths = c(6, 6),
            tags$div(
              class = "h-100 d-flex align-items-center",

              tags$div(
                tags$h1("Hi", class = "display-1"),
                tags$h1("Thanks for popping in! ", class = "display-6"),
                tags$div(style = "margin-top: 10rem;"),
                tags$p(class = "text-justify",
                  "I created analysePay because, at the time, I couldn't find any
                tools that offered the functionality I needed while
                navigating some potentially life-changing decisions.
                I truly hope you found it insightful and engaging!
                If you know of any fascinating data sources that could
                be integrated into the app, or if you have ideas for improvement,
                I'd love to hear from you! :)"
                ),
                br(), br(),
                tags$span(
                  class = "d-flex justify-content-center align-items-center",
                  tags$a(
                    href = "mailto:adrian9.wisnios@gmail.com",
                    shiny::icon("square-envelope", style = "font-size: 3rem; padding-right: 10px;"),
                    target = "_blank"
                  ),
                  tags$a(
                    href = "https://www.linkedin.com/in/adrian-wisnios-022408215/",
                    shiny::icon("linkedin", style = "font-size: 3rem; padding-right: 10px;"),
                    target = "_blank"
                  ),
                  tags$a(
                    href = "https://github.com/Admate8",
                    icon("square-github", style = "font-size: 3rem; padding-right: 10px;"),
                    target = "_blank"
                  ),
                  tags$a(
                    href = "https://www.instagram.com/admate8/",
                    icon("square-instagram", style = "font-size: 3rem; padding-right: 10px;"),
                    target = "_blank"
                  ),
                  shinyWidgets::downloadBttn(
                    outputId = "cv_download",
                    label    = tags$strong("CV", style = "font-size: 1.31rem;"),
                    icon     = NULL,
                    style    = "simple",
                    size     = "md"
                  )
                )
              )
            ),

            tags$div(
              tags$div(
                class = "d-flex justify-content-center align-items-center image-me",
                tags$img(src = "../www/me.jpg")
              ),
              tags$h1("About me", class = "display-4"),
              tags$p(class = "text-justify", "I'm Adrian, and I'm a Data Analyst specialising in the higher education sector in England. I create, maintain and improve the student loan borrower simulation models and assess the impact of policy and macroeconomic determinants on loan repayments."),
              tags$p(class = "text-justify", "I'm also an R enthusiast who is passionate about everything related to this versatile language. Ever since I was introduced to R during my undergraduate studies, I've been hooked! I love exploring its endless possibilities, from experimenting with innovative approaches to data processing and visualisation to effectively communicating insights to non-technical audiences."),
              tags$p(class = "text-justify", "Staying motivated and open-minded, I constantly seek opportunities for further personal development. I believe that positive change originates in firm and well-rounded character, and by improving yourself as a human being, you impact the broader community, which leads to meaningful experiences.")
            )
          )
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
