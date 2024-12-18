#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  ## CV Downloader
  output$cv_download <- downloadHandler(
    filename = "CV Adrian Wisnios.pdf",
    content = function(file){
      file.copy(file.path(here::here(), "inst/extdata/CV.pdf"), file)
    }
  )

  # Page 1 ----
  ## Hints ----
  ## Show the hint on opening accordion with settings
  observeEvent(input$accordion_first_time_open, {
    if (input$accordion_first_time_open) {
      showNotification(
        tags$div(
          tags$div(
            style = "text-align: center; padding-bottom: 10px;",
            tags$h5("Hint!")
          ),
          tags$p(class = "text-center",
                 "You can control the precision of the slider selections by using the
        arrow keys on your keyboard!"
          )
        ),
        duration = 10,
        type     = "default"
      )
    }
  })

  ## Show the hint on opening the student loan accordeon panel for the first time
  observeEvent(input$sl_accordion_opened, {
    showNotification(
      tags$div(
        tags$div(
          style = "text-align: center; padding-bottom: 10px;",
          tags$h5("Hint!")
        ),
        tags$p(class = "text-center",
        "If you don't have to repay any Student Loans, set their contribution
        rates to zero!")
      ),
      duration = 7,
      type     = "default"
    )
  })

  ## Utils: User inputs ----
  ### Preserve vars
  settings_from <- NULL
  settings_to   <- NULL
  makeReactiveBinding("settings_from")
  makeReactiveBinding("settings_to")
  makeReactiveBinding("iv_from")
  makeReactiveBinding("iv_to")
  ns_from <- reactiveVal("1")
  ns_to   <- reactiveVal("1")

  ### Update country_from and country_to settings depending on the user's selection
  observeEvent(c(input$select_country_from, input$select_country_to), {

    # If country_from == country_to we need to make sure both setting options
    # are independent, i.e. they have different namespaces. Otherwise, it does
    # not matter
    if (input$select_country_from == input$select_country_to) {
      ns_from("1")
      ns_to("2")
    }

    ### Update the UI accordeons and settings
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

    settings_from <<- base::get(server_settings_from_name)(ns_from())$settings
    settings_to   <<- base::get(server_settings_to_name)(ns_to())$settings

    iv_from <<- base::get(server_settings_from_name)(ns_from())$iv
    iv_to   <<- base::get(server_settings_to_name)(ns_to())$iv
  })

  ## Utils: Validate remaining inputs ----
  iv_provide_annual_earnings <- shinyvalidate::InputValidator$new()
  iv_provide_percentile      <- shinyvalidate::InputValidator$new()
  observeEvent(settings_from(), {
    req(settings_from())

    ## Update the numerical input with the base currency formatting
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "provide_annual_earnings",
      value   = with(settings_from()$earning_deciles, value[decile == 50]),
      options = list(
        currencySymbol          = settings_from()$global$currencySymbol,
        currencySymbolPlacement = settings_from()$global$currencySymbolPlacement,
        decimalCharacter        = settings_from()$global$decimalCharacter,
        digitGroupSeparator     = settings_from()$global$digitGroupSeparator
      )
    )

    ## Validate the annual earnings input
    iv_provide_annual_earnings$add_rule(
      "provide_annual_earnings",
      function(value) {
        if (is.null(value)) "Supply annual earnings..."
        else if (value > with(settings_from()$earning_deciles, value[decile == 95])) "Try a smaller value..."
        else if (value < with(settings_from()$earning_deciles, value[decile == 10])) "Try a bigger value..."
      }
    )
    iv_provide_annual_earnings$enable()

    ## Validate the percentile input
    iv_provide_percentile$add_rule(
      "provide_percentile",
      function(value) {
        if (is.null(value)) "Provide a value..."
        else if (value > 95) "Must be \U2264 95!"
        else if (value < 10) "Must be \U2265 10!"
      }
    )
    iv_provide_percentile$enable()
  })

  ## Button: Restore default settings ----
  observeEvent(input$restore_defaults_earnings, {
    shinyWidgets::updateAutonumericInput(
      session = session,
      inputId = "provide_annual_earnings",
      value   = with(settings_from()$earning_deciles, value[decile == 50]),
      options = list(
        currencySymbol          = settings_from()$global$currencySymbol,
        currencySymbolPlacement = settings_from()$global$currencySymbolPlacement,
        decimalCharacter        = settings_from()$global$decimalCharacter,
        digitGroupSeparator     = settings_from()$global$digitGroupSeparator
      )
    )
    shinyWidgets::updateRadioGroupButtons(inputId = "select_calc_period", selected = "year")
    shinyWidgets::updateAutonumericInput(inputId = "provide_percentile", value = 50)
    shinyWidgets::updateSwitchInput(inputId = "select_percentile_or_earnings", value = TRUE)
  })

  ## Utils: Update percentile suffix ----
  observeEvent(input$provide_percentile, {
    shinyWidgets::updateAutonumericInput(
      inputId = "provide_percentile",
      options = list(currencySymbol = update_percentile_suffix(input$provide_percentile))
    )
  })


  ## Button: Commit settings ----
  observe({
    if (all(iv_from$is_valid(), iv_to$is_valid(), iv_provide_annual_earnings$is_valid(), iv_provide_percentile$is_valid())) {
      shinyjs::enable("commit_input_data")
      output$commit_button_text <- renderText({"Analyse!"})
    } else {
      shinyjs::disable("commit_input_data")
      output$commit_button_text <- renderText({"Invalid Settings!"})
    }
  })

  ## Data: df_main() ----
  # Observe the "Analyse!" button - this is the main data in the app
  df_main <- eventReactive(input$commit_input_data, {
    get_df_earnings_dist(
      settings_from = settings_from(),
      settings_to   = settings_to()
    )$df_main
  })

  ## Utils: Unify earnings/percentile selection ----
  selected_percentile <- reactiveVal(50)
  observeEvent(c(
    input$select_percentile_or_earnings,
    input$provide_percentile,
    input$provide_annual_earnings
  ), {
    if (input$select_percentile_or_earnings) selected_percentile(input$provide_percentile)
    else selected_percentile(map_percentiles(input$provide_annual_earnings, df_main())$point_from[1])
  })


  # Page 2 ----
  ## Slide 1 ----
  ### Card: Base/Target cards ----
  observeEvent(c(
    selected_percentile(),
    input$select_calc_period,
    df_main()
  ), {
    req(selected_percentile())
    req(input$select_calc_period)
    req(df_main())

    earningsCardServer("1", selected_percentile(), input$select_calc_period, df_main())
    output$ui_earnings_cards <- renderUI({
      htmltools::tagList(
        tags$p(HTML(paste0(
          "Approximately ", tags$strong(paste0(selected_percentile(), "%")), " of the working population in ",
          tags$strong(
            style = paste0("color: ", palette_global$categories$base_color, ";"),
            settings_from()$global$full_name
          ), " earns ",
          tags$strong(
            df_main() |>
              dplyr::filter(percentile == selected_percentile()) |>
              dplyr::pull(earnings_from) |>
              prep_display_currency(settings_from()$global$short_cut, "year")
          ),
          " annually. Based on your settings selection, that translates to..."
        ))),
        br(),
        earningsCardUI("1")
      )
    })
  })

  ### Table: Deduction components ----
  df_categories <- eventReactive(input$commit_input_data, {
    req(input$commit_input_data)

    get_df_earnings_dist(
      settings_from = settings_from(),
      settings_to   = settings_to()
    )$df_cat_table |>
      # Join in the colours from the global options - currently not in use
      cbind(col = c(
        palette_global$categories$pension_color,
        palette_global$categories$pension_color_vol,
        palette_global$categories$insurance_color,
        palette_global$categories$insurance_color_vol,
        palette_global$categories$tax_color,
        palette_global$categories$sl_plan2_color,
        palette_global$categories$sl_plan3_color
      ))
  }, ignoreNULL = FALSE)

  output$table_components <- reactable::renderReactable({
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
            style = list(fontWeight = "bold", fontSize = "0.75rem"),
            # Pills distort the page balance... but leave if I change my mind
            # cell  = reactablefmtr::pill_buttons(
            #   data = df_categories(),
            #   color_ref           = "col",
            #   bold_text           = TRUE,
            #   brighten_text_color = palette_global$body_color,
            #   text_color          = palette_global$body_bg
            # ),
            width = 220
          ),
          col = reactable::colDef(show = FALSE)
        ),
        theme = custom_reactable_theme()
      )
  })


  observeEvent(c(selected_percentile(), df_main(), input$select_calc_period), {
    req(df_main())
    req(selected_percentile())
    req(input$select_calc_period)

    ### Plot: Earnings by percentiles ----
    output$plot_earnings_by_percentiles <- echarts4r::renderEcharts4r({plot_earnings_by_percentiles(selected_percentile(), df_main(), input$select_calc_period)})

    ### Plot: Deductions ----
    output$plot_all_deductions       <- echarts4r::renderEcharts4r({plot_all_deductions(selected_percentile(), df_main())})
    output$plot_deductions_breakdown <- echarts4r::renderEcharts4r({plot_deductions_breakdown(selected_percentile(), df_main())})
  })


  # Page 3 ----
  ## Slide 1 ----

  ### Data: Expend data ----
  df_expend <- reactive({
    req(df_main())
    get_df_expend(df_main())
  })

  ### Button: Restore defaults expend num inputs ----
  observeEvent(input$restore_expend_inputs, {
    shinyWidgets::updateAutonumericInput("expend_num_input_food", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_drinks", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_clothing", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_housing", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_household", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_health", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_transport", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_comms", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_recreation", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_education", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_restaurants", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_misc", value = 0, session = session)
    shinyWidgets::updateAutonumericInput("expend_num_input_other", value = 0, session = session)
  })

  ### Plot: Expenditure plots
  observeEvent(c(df_expend(), selected_percentile()), {

    output$plot_expend_breakdown <- echarts4r::renderEcharts4r({plot_expend_breakdown(selected_percentile(), df_expend())})
  })

  # output$test_output1 <- renderText({paste0(unlist(settings_from(), recursive = TRUE), collapse = ", ")})
  # output$test_output2 <- renderText({paste0(unlist(settings_to(), recursive = TRUE), collapse = ", ")})
}
