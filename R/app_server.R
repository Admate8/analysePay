#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Page 1 ----
  ## Preserve inputs
  settings_from <- NULL
  settings_to   <- NULL
  makeReactiveBinding("settings_from")
  makeReactiveBinding("settings_to")
  makeReactiveBinding("iv_from")
  makeReactiveBinding("iv_to")
  ns_from <- reactiveVal("1")
  ns_to   <- reactiveVal("1")

  ## Update country_from and country_to settings depending on the user's selection
  observeEvent(c(input$select_country_from, input$select_country_to), {

    # If country_from == country_to we need to make sure both setting options
    # are independent, i.e. they have different namespaces. Otherwise, it does
    # not matter
    if (input$select_country_from == input$select_country_to) {
      ns_from("1")
      ns_to("2")
    }

    ### Update the UI
    ui_settings_from_name <- paste0(input$select_country_from, "SettingsUserUI")
    ui_settings_to_name   <- paste0(input$select_country_to, "SettingsUserUI")

    output$ui_settings <- renderUI({
      bslib::layout_columns(
        col_widths = c(6, 6),
        base::get(ui_settings_from_name)(ns_from()),
        base::get(ui_settings_to_name)(ns_to())
      )
    })

    ### Update server-global settings
    # Define universal settings to avoid multiple if-else conditions
    # As long as {country}SettingsUserServer has the same name, we can do this.
    server_settings_from_name <- paste0(input$select_country_from, "SettingsUserServer")
    server_settings_to_name   <- paste0(input$select_country_to, "SettingsUserServer")

    ### Define the following variables as global
    settings_from <<- base::get(server_settings_from_name)(ns_from())$settings
    settings_to   <<- base::get(server_settings_to_name)(ns_to())$settings

    iv_from <<- base::get(server_settings_from_name)(ns_from())$iv
    iv_to   <<- base::get(server_settings_to_name)(ns_to())$iv
  })

  ### Commit settings button
  observe({
    if (all(iv_from$is_valid(), iv_to$is_valid())) {
      shinyjs::enable("commit_input_data")
      output$commit_button_text <- renderText({"Analyse!"})
    } else {
      shinyjs::disable("commit_input_data")
      output$commit_button_text <- renderText({"Invalid Settings!"})
    }
  })

  ## Set the main data
  # Observe the "Analyse!" button - this is the main data in the app
  df_main <- eventReactive(input$commit_input_data, {
    get_df_earnings_dist(
      settings_from = settings_from(),
      settings_to   = settings_to()
    )$df_main
  })

  # Page 2 ----
  ## Render the categories table
  df_categories <- eventReactive(input$commit_input_data, {
    get_df_earnings_dist(
      settings_from = settings_from(),
      settings_to   = settings_to()
    )$df_cat_table |>
      ### Join in the colours from the global options
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
  }, ignoreNULL = FALSE)


  output$table_categories <- reactable::renderReactable({
    df_categories() |>
      reactable::reactable(
        sortable      = FALSE,
        resizable     = FALSE,
        compact       = TRUE,
        showSortIcon  = FALSE,
        defaultColDef = reactable::colDef(
          html        = TRUE,
          style       = "font-size: 0.75rem;",
          headerStyle = "font-size: 0.80rem;"
        ),
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

  output$ui_categories_table <- renderUI({
    tags$div(
      style = "position: relative;",
      reactable::reactableOutput("table_categories") |> custom_spinner(),
      tags$div(
        style = "position: absolute; left: 0; top: 0; z-index: 20;",
        tags$span(
          shiny::icon("asterisk", style = "font-size: 1rem;") |>
            bslib::tooltip("Deducted before the Income Tax"),
          shiny::HTML("&nbsp&nbsp"),
          shiny::icon("percentage", style = "font-size: 1rem;") |>
            bslib::tooltip(shiny::HTML(
              "<b>Bracket Percentages</b><br><br>
                  Sometimes, deductions consist of multiple country-specific,
                  smaller deductions. In such cases, a percentage split is
                  displayed next to each, indicating its contribution to the
                  total deduction. That enables you to recover their original
                  values."
            ))
        )
      )
    )
  })

  output$plot_earnings_decile_dist <- echarts4r::renderEcharts4r({plot_earnings_decile_dist(df_main())})

  # Page 3 ----
  ## Render reactive ui (country-dependent) to get user's annual earnings
  output$ui_provide_annual_earnings <- renderUI({

    country_from <- purrr::discard(unique(df_main()$country_from), is.na)

    base::get(paste0(country_from, "_autonumericInput"))(
      inputId = "provide_annual_earnings",
      label   = "Set annual earnings",
      value   = base::get(paste0(country_from, "_settings"))$earning_deciles$`50th`
    )
  })

  ## Render the interpolated distribution with nominal deductions breakdown plot
  output$plot_int_earnings_decile_dist <- echarts4r::renderEcharts4r({plot_int_earnings_decile_dist(df_main(), input$select_calc_period)})

  ## Update the plot with mark lines
  observeEvent(c(input$provide_annual_earnings, input$select_calc_period), {

    ### Make sure we don't pass unreasonable bounds
    lower_bound   <- min(df_main()$earnings_from, na.rm = TRUE)
    upper_bound   <- max(df_main()$earnings_from, na.rm = TRUE)
    pass_earnings <- ifelse(
      input$provide_annual_earnings < lower_bound, lower_bound,
      ifelse(
        input$provide_annual_earnings > upper_bound, upper_bound,
        input$provide_annual_earnings
      ))

    proxy_int_earnings_decile_dist(
      plot            = echarts4r::echarts4rProxy("plot_int_earnings_decile_dist", data = NULL),
      annual_earnings = pass_earnings,
      df              = df_main(),
      period          = input$select_calc_period
    )
  })

  observeEvent(c(input$provide_annual_earnings, df_main()), {
    req(df_main())
    req(input$provide_annual_earnings)
    ## Render the radar plot showing percentage deduction comparison
    output$plot_radar_perc <- echarts4r::renderEcharts4r({plot_radar_perc(input$provide_annual_earnings, df_main())})
  })

  ## Validate the annual earnings input
  iv_provide_annual_earnings <- shinyvalidate::InputValidator$new()
  iv_provide_annual_earnings$add_rule(
    "provide_annual_earnings",
    function(value) {
      if (is.null(value)) "Supply annual earnings..."
      else if (value > max(df_main()$earnings_from, na.rm = TRUE)) "Try a smaller value..."
      else if (value < min(df_main()$earnings_from, na.rm = TRUE)) "Try a bigger value..."
    }
  )
  iv_provide_annual_earnings$enable()

  ## Render deciles & earnings sources
  output$ui_earnings_sources <- renderUI({

    country_from <- purrr::discard(unique(df_main()$country_from), is.na)
    country_to   <- purrr::discard(unique(df_main()$country_to), is.na)

    #base::get(paste0(country_from, "_settings"))$decile_source
    tags$span(
      bslib::popover(
        trigger = list(shiny::icon("sourcetree", style = "font-size: 1rem;")),
        tags$a(
          shiny::HTML("Data Source:<br>Earnings by Deciles (Base)"),
          target = "_blank",
          href = get(paste0(country_from, "_settings"))$decile_source
        )
      ),
      shiny::HTML("&nbsp&nbsp"),
      bslib::popover(
        trigger = list(shiny::icon("sourcetree", style = "font-size: 1rem;")),
        tags$a(
          shiny::HTML("Data Source:<br>Earnings by Deciles (Target)"),
          target = "_blank",
          href = get(paste0(country_to, "_settings"))$decile_source
        )
      ),
      shiny::HTML("&nbsp&nbsp"),
      bslib::tooltip(
        trigger = list(shiny::icon("info-circle", style = "font-size: 1rem;")),
        "As the continuous distribution of the earnings by deciles is not
        published, the unavailable data has been interpolated by fitting a
        spline. The scatter series in the plot below represents the actual
        data from the sources."
      )
    )
  })

  # output$test_output1 <- renderText({paste0(unlist(settings_from(), recursive = TRUE), collapse = ", ")})
  # output$test_output2 <- renderText({paste0(unlist(settings_to(), recursive = TRUE), collapse = ", ")})

}
