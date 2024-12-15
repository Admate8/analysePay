#' Define Custom Spinner Animation
#'
#' @param ui_element UI to attache the spinner to.
#' @param col Spinner colour.
#'
custom_spinner <- function(ui_element, col = palette_global$body_color) {
  shinycssloaders::withSpinner(ui_element, color = col, size = 1.5, type = 6)
}


#' Shade Hex Colour
#'
#' @param hex_color String: hex colour.
#' @param factor Shading factor: negative/positive values will darken/lighten the colour, respectively.
#'
#' @return Shaded hex colour.
get_hex_colour_shade <- function(hex_color, factor = 0.2) {
  if (factor == 0) hex_color
  else {
    rgb_values <- grDevices::col2rgb(hex_color) / 255
    rgb_values <- rgb_values + (1 - rgb_values) * factor
    rgb_values <- base::pmin(base::pmax(rgb_values, 0), 1)
    grDevices::rgb(rgb_values[1], rgb_values[2], rgb_values[3])
  }
}


#' Inverted versions of %in%
#' @noRd
`%notin%` <- Negate(`%in%`)


#' Add Flags to the pickerInput Options
#'
#' @param flag_class Class name, e.g. 'fi fi-pl'.
#' @param country Country name.
picker_options_with_flags <- function(flag_class, country) {
  shiny::HTML(paste(
    tags$span(class = flag_class, width = 30, height = 22),
    country
  ))
}


#' Insert Div with Icon and Link
#'
#' @param ... Any UI element.
#' @param link Link to the website.
#' @param icon_size Icon size.
#' @param tt_text Text in tooltip.
#'
#' @return Div.
#' @export
div_with_icon <- function(..., link, icon_size = "1rem", tt_text = "Find out more!") {
  tags$div(
    class = "w-100",
    style = "display: flex; justify-content: space-between;",
    tags$div(
      class = "left-text",
      ...
    ),
    tags$div(
      class = "right-text",
      tags$a(
        href = link,
        target = "_blank",
        shiny::icon(
          "info-circle",
          style = glue::glue("font-size: { icon_size }; color: { palette_global$body_color };")
        )
      ) |>
        bslib::tooltip(tt_text)
    )
  )
}


#' Customise the Reactable Tables
#'
#' @noRd
custom_reactable_theme <- function() {
  reactable::reactableTheme(
    color           = palette_global$body_color,
    backgroundColor = palette_global$body_bg,
    borderColor     = palette_global$body_bg,
    rowStyle = list(
      borderBottom = paste0("0.5px solid ", palette_global$body_tertiary_bg)
    ),
    headerStyle = list(
      borderBottom = paste0("2px solid ", palette_global$body_tertiary_bg),
      textAlign = "center"
    )
  )
}


#' Prepare Display Currency
#'
#' @param value Value to prepare.
#' @param country Either "uk" or "pl" (for now).
#' @param period Either "year", "month" or "week" to scale the value.
#'
#' @return Value formatted in the country standard way (as a string).
prep_display_currency <- function(value, country, period) {
  stopifnot(
    country %in% c("uk", "pl"),
    is.numeric(value), period %in% c("year", "month", "week")
  )

  value    <- value / ifelse(period == "year", 1, ifelse(period == "month", 12, 52.1429))
  settings <- base::get(paste0(country, "_settings"))

  full_name         <- settings$global$full_name
  suffix_or_preffix <- settings$global$currencySymbolPlacement
  currency_symbol   <- settings$global$currencySymbol
  big_mark          <- settings$global$digitGroupSeparator
  decimal_mark      <- settings$global$decimalCharacter

  display_prefix    <- ifelse(suffix_or_preffix == "p", currency_symbol, "")
  display_suffix    <- ifelse(suffix_or_preffix == "s", paste0(" ", currency_symbol), "")

  scales::comma(
    value,
    accuracy     = 0.01,
    prefix       = display_prefix,
    suffix       = display_suffix,
    big.mark     = big_mark,
    decimal.mark = decimal_mark
  )
}
