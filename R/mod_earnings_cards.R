cardsUI <- function(id) {
  uiOutput(shiny::NS(id, "card"), inline = TRUE)
}

#' Create Server Module for the Card
#'
#' @param id NS ID.
#' @param selected_percentile selected_percentile() reactive.
#' @param period Either "year", "month" or "week".
#' @param df df_main() - reactive.
#' @param mode In the interest of re-usability. Either "gross" or "net".
#' @noRd
cardsServer <- function(id, selected_percentile, period, df, mode) {
  moduleServer(
    id,
    function(input, output, session) {

      stopifnot(mode %in% c("gross", "net"))
      output$card <- renderUI({

        country_from <- purrr::discard(unique(df$country_from), is.na)
        country_to   <- purrr::discard(unique(df$country_to), is.na)

        country_from_name <- base::get(paste0(country_from, "_settings"))$global$full_name
        country_to_name   <- base::get(paste0(country_to, "_settings"))$global$full_name

        df <- df |> dplyr::filter(percentile == selected_percentile)

        if (mode == "gross") {

          base_val_top_bold    <- df |> dplyr::pull(earnings_from) |> prep_display_currency(country_from, period)
          base_val_bottom_bold <- df |> dplyr::pull(net_income_from) |> prep_display_currency(country_from, period)
          base_text_top        <- "gross"
          base_text_bottom     <- "net"
          base_perc            <- round(100 * df |> dplyr::pull(net_income_perc_from), 2)

          target_val_top_bold    <- df |> dplyr::pull(earnings_to) |> prep_display_currency(country_to, period)
          target_val_bottom_bold <- df |> dplyr::pull(net_income_to) |> prep_display_currency(country_to, period)
          target_text_top        <- "gross"
          target_text_bottom     <- "net"
          target_perc            <- round(100 * df |> dplyr::pull(net_income_perc_to), 2)

        }
        else if (mode == "net") {

          base_val_net         <- df |> dplyr::pull(net_income_from)
          base_val_top_bold    <- prep_display_currency(base_val_net, country_from, period)
          base_val_bottom_bold <- (base_val_net * (1 - df |> dplyr::pull(interpolated_values_from_perc_tot))) |> prep_display_currency(country_from, period)
          base_text_top        <- "net"
          base_text_bottom     <- "after spent"
          base_perc            <- round(100 * (1 - df |> dplyr::pull(interpolated_values_from_perc_tot)), 2)

          target_val_net         <- df |> dplyr::pull(net_income_to)
          target_val_top_bold    <- prep_display_currency(target_val_net, country_to, period)
          target_val_bottom_bold <- (target_val_net * (1 - df |> dplyr::pull(interpolated_values_to_perc_tot))) |> prep_display_currency(country_to, period)
          target_text_top        <- "net"
          target_text_bottom     <- "after spent"
          target_perc            <- round(100 * (1 - df |> dplyr::pull(interpolated_values_to_perc_tot)), 2)
        }

        bslib::layout_columns(
          col_widths = c(5, 2, 5),

          bslib::card(
            class = "custom-card-base",
            bslib::card_header(tags$h6(tags$strong(country_from_name)), class = "text-center"),
            bslib::card_body(
              class   = "text-center small",
              gap     = "0.3rem",
              padding = "10px",

              shiny::HTML(paste0(
                tags$span(tags$strong(base_val_top_bold), base_text_top, "per", period),
                tags$span("of which", class = "small"),
                tags$span(tags$span(tags$strong(base_val_bottom_bold), base_text_bottom, paste0("(", base_perc, "%)")))
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
              padding = "10px",

              shiny::HTML(paste0(
                tags$span(tags$strong(target_val_top_bold), target_text_top, "per", period),
                tags$span("of which", class = "small"),
                tags$span(tags$span(tags$strong(target_val_bottom_bold), target_text_bottom, paste0("(", target_perc, "%)")))
              ))
            )
          )
        )
      })
    }
  )
}
