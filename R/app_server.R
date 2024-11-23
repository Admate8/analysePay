#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Preserve inputs ----
  settings_from <- NULL
  settings_to   <- NULL
  makeReactiveBinding("settings_from")
  makeReactiveBinding("settings_to")
  makeReactiveBinding("iv_from")
  makeReactiveBinding("iv_to")
  ns_from <- reactiveVal("1")
  ns_to   <- reactiveVal("1")

  # Update country_from and country_to settings depending on the user's selection
  observeEvent(c(input$select_country_from, input$select_country_to), {

    # If country_from == country_to we need to make sure both setting options
    # are independent, i.e. they have different namespaces. Otherwise, it does
    # not matter
    if (input$select_country_from == input$select_country_to) {
      ns_from("1")
      ns_to("2")
    }

    # Update the UI
    ui_settings_from_name <- paste0(input$select_country_from, "SettingsUserUI")
    ui_settings_to_name   <- paste0(input$select_country_to, "SettingsUserUI")

    output$ui_settings <- renderUI({
      bslib::layout_columns(
        col_widths = c(-2, 4, 4, -2),
        base::get(ui_settings_from_name)(ns_from()),
        base::get(ui_settings_to_name)(ns_to())
      )
    })

    # Update server-global settings
    # Define universal settings to avoid multiple if-else conditions
    # As long as {country}SettingsUserServer has the same name, we can do this.
    server_settings_from_name <- paste0(input$select_country_from, "SettingsUserServer")
    server_settings_to_name   <- paste0(input$select_country_to, "SettingsUserServer")

    # Define the following variables as global
    settings_from <<- base::get(server_settings_from_name)(ns_from())$settings
    settings_to   <<- base::get(server_settings_to_name)(ns_to())$settings

    iv_from <<- base::get(server_settings_from_name)(ns_from())$iv
    iv_to   <<- base::get(server_settings_to_name)(ns_to())$iv
  })

  # Commit settings button ----
  observe({
    if (all(iv_from$is_valid(), iv_to$is_valid())) {
      shinyjs::enable("commit_input_data")
      output$commit_button_text <- renderText({"Analyse!"})
    } else {
      shinyjs::disable("commit_input_data")
      output$commit_button_text <- renderText({"Invalid Settings!"})
    }
  })

  # Observe the "Analyse!" button - this is the main data in the app
  df_main <- eventReactive(input$commit_input_data, {
    get_df_earnings_dist(
      settings_from = settings_from(),
      settings_to   = settings_to()
    )$df_main
  })

  df_categories <- eventReactive(input$commit_input_data, {
    get_df_earnings_dist(
      settings_from = settings_from(),
      settings_to   = settings_to()
    )$df_cat_table |>
      # Join in the colours from the global options
      cbind(col = c(
        palette_global$categories$pension_color,
        palette_global$categories$pension_color_vol,
        palette_global$categories$insurance_color,
        palette_global$categories$insurance_color_vol,
        palette_global$categories$tax_color,
        palette_global$categories$sl_plan2_color,
        palette_global$categories$sl_plan3_color,
        palette_global$categories$net_color
      ))
  })
  output$table_categories <- reactable::renderReactable({
    df_categories() |>
      reactable::reactable(
        sortable      = FALSE,
        resizable     = FALSE,
        compact       = TRUE,
        showSortIcon  = FALSE,
        defaultColDef = reactable::colDef(html = TRUE),
        columns       = list(
          split = reactable::colDef(
            name  = "",
            cell  = reactablefmtr::pill_buttons(
              data = df_categories(),
              color_ref           = "col",
              bold_text           = TRUE,
              brighten_text_color = palette_global$body_color,
              text_color          = palette_global$body_bg
            ),
            width = 220
          ),
          col = reactable::colDef(show = FALSE)
        ),
        theme = custom_reactable_theme()
      )
  })

  # output$test_output1 <- renderText({paste0(unlist(settings_from(), recursive = TRUE), collapse = ", ")})
  # output$test_output2 <- renderText({paste0(unlist(settings_to(), recursive = TRUE), collapse = ", ")})


  output$test_plot <- echarts4r::renderEcharts4r({

    get_gradient <- function(col, factor) {
      shaded_col <- get_hex_colour_shade(col, factor)
      htmlwidgets::JS(paste0(
        "new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                { offset: 0, color: '", col, "' },
                { offset: 0.4, color: '", shaded_col, "' },
                { offset: 0.6, color: '", shaded_col, "' },
                { offset: 1, color: '", col, "' }
            ])"
      ))
    }
    mtcars |>
      dplyr::group_by(carb) |>
      dplyr::summarise(
        sum1 = sum(disp),
        sum2 = sum(hp),
        .groups = "drop"
      ) |>
      dplyr::mutate(carb = as.character(carb)) |>
      echarts4r::e_chart(x = carb) |>
      echarts4r::e_bar(serie = sum1, stack = "1",
        itemStyle = list(color = get_gradient("#0077CC", 0.2))
      ) |>
      echarts4r::e_bar(serie = sum2, stack = "1",
        itemStyle = list(color = get_gradient("#00A6FB", 0.2))
      ) |>
      echarts4r::e_bar(serie = sum1, stack = "1",
         itemStyle = list(color = get_gradient("#FF5733", 0.2))
      ) |>
      echarts4r::e_bar(serie = sum2, stack = "1",
        itemStyle = list(color = get_gradient("#FF8C42", 0.2))
      ) |>
      echarts4r::e_bar(serie = sum1, stack = "1",
        itemStyle = list(color = get_gradient("#E63946", 0.2))
      ) |>
      echarts4r::e_bar(serie = sum2, stack = "1",
        itemStyle = list(color = get_gradient("#9B4DCA", 0.2))
      ) |>
      echarts4r::e_bar(serie = sum1, stack = "1",
        itemStyle = list(color = get_gradient("#F8C630", 0.3))
      )

  })
}
