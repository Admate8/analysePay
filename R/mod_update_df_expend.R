
updateDfExpendUI <- function(id) {
  tags$div(
    class = "d-flex flex-column align-items-center",
    style = "margin-top: 27%;",
    actionButton(
      inputId = shiny::NS(id, "restore_expend_inputs"),
      label   = NULL,
      icon    = shiny::icon("rotate-right", style = "font-size: 1.2rem; margin-bottom: 10%;"),
      width   = "100%"
    ),
    get_expend_num_input(shiny::NS(id, "expend_num_input_food")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_drinks")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_clothing")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_housing")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_household")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_health")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_transport")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_comms")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_recreation")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_education")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_restaurants")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_misc")),
    get_expend_num_input(shiny::NS(id, "expend_num_input_other"))
  )
}

updateDfExpendServer <- function(id, df_expend) {
  moduleServer(
    id,
    function(input, output, session) {

      # Data: Initiate updated df_expend (grossing_factor left for debugging)
      df_expend_update <- reactiveValues(
        data = df_expend |>
          dplyr::mutate(grossing_factor = 1)
      )

      # Button: Restore defaults expend num inputs ----
      observeEvent(input$restore_expend_inputs, {

        ## Restore the data to the original df_expend
        df_expend_update$data <- df_expend |>
          dplyr::mutate(grossing_factor = 1)

        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_food")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_drinks")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_clothing")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_housing")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_household")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_health")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_transport")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_comms")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_recreation")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_education")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_restaurants")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_misc")
        shinyWidgets::updateAutonumericInput(session = session, value = 0, inputId = "expend_num_input_other")
      })

      # Data: Update df_expend() ----
      ## Ideally, updating one value should redraw only a single bar, no the whole
      # series, but I don't think it's possible. Instead, we dynamically update
      # all series at once.
      observeEvent(c(
        input$expend_num_input_food,
        input$expend_num_input_drinks,
        input$expend_num_input_clothing,
        input$expend_num_input_housing,
        input$expend_num_input_household,
        input$expend_num_input_health,
        input$expend_num_input_transport,
        input$expend_num_input_comms,
        input$expend_num_input_recreation,
        input$expend_num_input_education,
        input$expend_num_input_restaurants,
        input$expend_num_input_misc,
        input$expend_num_input_other
      ), {

        ## Make sure that the data does not stack the value, but resets to df_enpend
        ## after each interaction with the inputs
        df_expend_update$data <- df_expend |>
          dplyr::mutate(grossing_factor = 1)

        df_helper <- data.frame(
          # They can be hard-coded as the categories must not change
          expenditure = c(
            "Food & non-alcoholic drinks",
            "Alcoholic drinks, tobacco & narcotics",
            "Clothing & footwear",
            "Housing (net), fuel & power",
            "Household goods & services",
            "Health",
            "Transport",
            "Communication",
            "Recreation & culture",
            "Education",
            "Restaurants & hotels",
            "Miscellaneous goods & services",
            "Other expenditure items"
          ),
          grossing_factor = 1 + c(
            input$expend_num_input_food,
            input$expend_num_input_drinks,
            input$expend_num_input_clothing,
            input$expend_num_input_housing,
            input$expend_num_input_household,
            input$expend_num_input_health,
            input$expend_num_input_transport,
            input$expend_num_input_comms,
            input$expend_num_input_recreation,
            input$expend_num_input_education,
            input$expend_num_input_restaurants,
            input$expend_num_input_misc,
            input$expend_num_input_other
          ) / 100
        )

        df_updated <- df_expend_update$data |>
          dplyr::select(-grossing_factor) |>
          dplyr::left_join(df_helper, by = "expenditure") |>
          dplyr::mutate(dplyr::across(
            c(
              "actual_values_from_perc",
              "interpolated_values_from_perc",
              "avg_from_perc",
              "actual_values_to_perc",
              "interpolated_values_to_perc",
              "avg_to_perc"
            ),
            ### Scale the columns using the grossing factor - check for lower & upper bounds
            ~ ifelse(.x * grossing_factor < 0, 0, ifelse(
              .x * grossing_factor > 1, 1, .x * grossing_factor
            ))
          ))

        ## Update the data
        df_expend_update$data <- df_updated
      })

      # Add the total columns for convenience

      return(reactive({df_expend_update$data}))
    }
  )
}
