#' Get Customised Echarts Styling
#'
#' @param col Colour.
#' @param factor Shade factor.
#' @param reverse Logical - should the shades be reversed?
#'
#' @noRd
get_gradient <- function(col, factor, reverse = FALSE) {
  shaded_col <- get_hex_colour_shade(col, factor)

  if (reverse == FALSE) {
    htmlwidgets::JS(paste0(
      "new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                { offset: 0, color: '", col, "' },
                { offset: 0.4, color: '", shaded_col, "' },
                { offset: 0.6, color: '", shaded_col, "' },
                { offset: 1, color: '", col, "' }
            ])"
    ))
  } else {
    htmlwidgets::JS(paste0(
      "new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                { offset: 0, color: '", shaded_col, "' },
                { offset: 0.4, color: '", col, "' },
                { offset: 0.6, color: '", col, "' },
                { offset: 1, color: '", shaded_col, "' }
            ])"
    ))
  }
}


#' Create Custom Echart Tooltip
#'
#' Customise tooltip depending on the country currency.
#'
#' @param country Country short cut name.
#'
#' @noRd
get_echart_tooltip <- function(country = "uk") {

  settings <- paste0(country, "_settings")
  locale   <- base::get(settings)$global$locale
  currency <- base::get(settings)$global$currency

  htmlwidgets::JS(paste0(
    "function (value) {
        if (value === undefined || value === null || value === '') {
          return '-';
        }
        return parseFloat(value).toLocaleString('", locale, "', {
        style: 'currency', currency: '", currency,"', maximumFractionDigits: 0
        });
      }"
  ))
}
