
get_expend_num_input <- function(id_val) {
  shinyWidgets::autonumericInput(
    inputId = id_val,
    label   = NULL,
    value   = 0,
    width   = 90,
    align   = "center",
    currencySymbol          = "%",
    allowDecimalPadding     = FALSE,
    currencySymbolPlacement = "s",
    minimumValue            = -50,
    maximumValue            = 50,
    emptyInputBehavior      = "zero",
    decimalPlaces           = 2,
    style = glue::glue(
      "font-size: 0.75rem;
       border-radius: 25px;
       padding-top: 8.25%;
       padding-bottom: 8.25%;
       color: {palette_global$body_color_secondary};
       border-color: transparent;
       background-color: {palette_global$body_tertiary_bg};
       box-shadow: 0 0 2px {palette_global$body_tertiary_bg};
       transition: box-shadow 0.3s ease-in-out;"
    ),
    wheelStep        = 5,
    showPositiveSign = TRUE,
    onInvalidPaste   = "ignore"
  )
}
