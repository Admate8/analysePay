#' Define Custom Spinner Animation
#'
#' @param ui_element UI to attache the spinner to.
#' @param col Spinner colour.
#'
custom_spinner <- function(ui_element, col = palette_global$body_tertiary_bg) {
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


#' Add a Popover Icon to a Text
#'
#' @param title Ttile.
#' @param message Popover message.
#' @param size Either 4 and 5 - text size.
#' @param icon Icon to include after the label.
#'
#' @noRd
title_with_popover <- function(title, message, size = 5, icon = "info-circle") {
  stopifnot(size %in% c(4, 5))
  if (size == 4) {
    tags$span(
      tags$h4(
        title,
        bslib::popover(
          trigger = shiny::icon(icon),
          shiny::HTML(message)
        )
      )
    )
  } else {
    tags$span(
      tags$h5(
        title,
        bslib::popover(
          trigger = shiny::icon(icon),
          shiny::HTML(message)
        )
      )
    )
  }
}


#' Add a Popover to Item's Label
#'
#' @param label Label name.
#' @param message Popover message.
#' @param icon Icon to include after the label.
#'
#' @noRd
label_with_popover <- function(label, message, icon = "info-circle") {
  bslib::popover(
    trigger = list(
      label,
      shiny::icon(icon)
    ),
    shiny::HTML(message)
  )
}


#' Customise the Reactable Tables
#'
#' @noRd
custom_reactable_theme <- function() {
  reactable::reactableTheme(
    color           = palette_global$body_color,
    backgroundColor = palette_global$body_tertiary_bg,
    borderColor     = palette_global$body_secondary_bg
  )
}
