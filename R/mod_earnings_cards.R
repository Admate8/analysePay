earningsCardUI <- function(id) {
  uiOutput(shiny::NS(id, "card"), inline = TRUE)
}

#' Create Server Module for the Card
#'
#' @param id NS ID.
#' @param selected_percentile selected_percentile() reactive.
#' @param period Either "year", "month" or "week".
#' @param df df_main() - reactive.
#' @noRd
earningsCardServer <- function(id, selected_percentile, period, df) {
  moduleServer(
    id,
    function(input, output, session) {

      output$card <- renderUI({

        country_from <- purrr::discard(unique(df$country_from), is.na)
        country_to   <- purrr::discard(unique(df$country_to), is.na)

        country_from_name <- base::get(paste0(country_from, "_settings"))$global$full_name
        country_to_name   <- base::get(paste0(country_to, "_settings"))$global$full_name

        bslib::layout_columns(
          col_widths = c(5, 2, 5),

          bslib::card(
            class = "custom-card-base",
            bslib::card_header(tags$h6(tags$strong(country_from_name)), class = "text-center"),
            bslib::card_body(
              class   = "text-center small",
              gap     = "0.3rem",
              padding = "5px",

              shiny::HTML(paste0(
                tags$span(
                  tags$strong(
                    df |>
                      dplyr::filter(percentile == selected_percentile) |>
                      dplyr::pull(earnings_from) |>
                      prep_display_currency(country_from, period)
                  ),
                  " gross per ", period
                ),

                tags$span("of which", class = "small"),

                tags$span(tags$span(
                  tags$strong(
                    df |>
                      dplyr::filter(percentile == selected_percentile) |>
                      dplyr::pull(net_income_from) |>
                      prep_display_currency(country_from, period)
                  ),
                  " net", paste0(
                    "(",
                    round(
                      100 * df |>
                        dplyr::filter(percentile == selected_percentile) |>
                        dplyr::pull(net_income_perc_from), 2
                    ), "%)"
                  )
                ))
              ))
            )
          ),

          tags$div(
            class = "h-100 d-flex flex-wrap align-items-center justify-content-center",
            shiny::icon("arrow-right-long", style = "font-size: 2rem;")
          ),

          bslib::card(
            class = "custom-card-target",
            bslib::card_header(tags$h6(tags$strong(country_to_name)), class = "text-center"),
            bslib::card_body(
              class   = "text-center small",
              gap     = "0.3rem",
              padding = "5px",

              shiny::HTML(paste0(
                tags$span(
                  tags$strong(df |>
                    dplyr::filter(percentile == selected_percentile) |>
                    dplyr::pull(earnings_to) |>
                    prep_display_currency(country_to, period)),
                  " gross per ", period, br()
                ),

                tags$span("of which", class = "small"),

                tags$span(tags$span(
                  tags$strong(
                    df |>
                      dplyr::filter(percentile == selected_percentile) |>
                      dplyr::pull(net_income_to) |>
                      prep_display_currency(country_to, period)
                  ),
                  " net", paste0(
                    "(",
                    round(
                      100 * df |>
                        dplyr::filter(percentile == selected_percentile) |>
                        dplyr::pull(net_income_perc_to), 2
                    ), "%)"
                  )
                ))
              ))
            )
          )
        )
      })
    }
  )
}
