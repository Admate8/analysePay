#' Get Customised Echarts e_bar Styling
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


#' Draw a Custom Bar Serie
#'
#' @param ... Other \code{echarts4r::e_bar_} arguments.
#' @param serie Serie name.
#' @param color Serie colour.
#' @param stack Stack group.
#' @param country Country short cut name.
#'
#' @noRd
draw_echart_bar_serie <- function(..., serie, color, stack, country = "uk") {

  col_gradient      <- get_gradient(color, 0.3)
  col_gradient_emph <- get_gradient(color, 0.3, TRUE)
  col               <- palette_global$categories[[color]]
  name              <- paste0(toupper(country), ": ", stringr::str_to_title(gsub("_", " ", gsub("_perc|_from|_to", "", serie))))

  echarts4r::e_bar_(
    serie     = serie,
    name      = name,
    color     = col,
    tooltip   = list(valueFormatter = htmlwidgets::JS(
      "function(value) {
        return parseFloat(value) + '%';
      }"
    )),
    stack     = stack,
    itemStyle = list(color = col_gradient),
    emphasis  = list(itemStyle = list(color = col_gradient_emph), focus = "series"),
    ...
  )
}

#' Draw a Custom Scatter Serie
#'
#' @param ... Other \code{echarts4r::e_bar_} arguments.
#' @param serie Serie name.
#' @param index Y-axis index.
#' @param country Country short cut name.
#'
#' @noRd
draw_echart_scatter_serie <- function(..., serie, index, country = "uk") {

  col     <- palette_global$categories$earnings_color
  name    <- paste0(toupper(country), ": ", stringr::str_to_title(gsub("_", " ", gsub("_perc|_from|_to", "", serie))))
  tooltip <- list(valueFormatter = get_echart_tooltip(country))

  echarts4r::e_scatter_(
    serie       = serie,
    name        = name,
    color       = col,
    stack       = stack,
    symbol_size = 10,
    tooltip     = tooltip,
    y_index     = index,
    emphasis    = list(focus = "series"),
    ...
  )
}


#' Plot Earnings by Decile Deductions Breakdown
#'
#' @param df Data returned by \code{get_df_earnings_dist()$df_main}
#'
#' @noRd
plot_earnings_decile_dist <- function(df) {

  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  df |>
    # Show only available deciles from the sources
    dplyr::filter(actuals_from == 1 & actuals_to == 1) |>
    dplyr::mutate(
      deciles = paste0(as.factor(deciles), "th"),
      dplyr::across(dplyr::contains("perc"), ~ round(.x * 100, 2))
    ) |>
    echarts4r::e_chart(x = deciles) |>
    draw_echart_bar_serie(serie = "net_income_perc_from",          color = palette_global$categories$net_color,           stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "student_loan_plan_3_perc_from", color = palette_global$categories$sl_plan3_color,      stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "student_loan_plan_2_perc_from", color = palette_global$categories$sl_plan2_color,      stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "income_tax_perc_from",          color = palette_global$categories$tax_color,           stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "insurance_voluntary_perc_from", color = palette_global$categories$insurance_color_vol, stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "insurance_mandatory_perc_from", color = palette_global$categories$insurance_color,     stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "pension_voluntary_perc_from",   color = palette_global$categories$pension_color_vol,   stack = "0", country = country_from) |>
    draw_echart_bar_serie(serie = "pension_mandatory_perc_from",   color = palette_global$categories$pension_color,       stack = "0", country = country_from) |>

    draw_echart_bar_serie(serie = "net_income_perc_to",            color = palette_global$categories$net_color,           stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "student_loan_plan_3_perc_to",   color = palette_global$categories$sl_plan3_color,      stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "student_loan_plan_2_perc_to",   color = palette_global$categories$sl_plan2_color,      stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "income_tax_perc_to",            color = palette_global$categories$tax_color,           stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "insurance_voluntary_perc_to",   color = palette_global$categories$insurance_color_vol, stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "insurance_mandatory_perc_to",   color = palette_global$categories$insurance_color,     stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "pension_voluntary_perc_to",     color = palette_global$categories$pension_color_vol,   stack = "1", country = country_to) |>
    draw_echart_bar_serie(serie = "pension_mandatory_perc_to",     color = palette_global$categories$pension_color,       stack = "1", country = country_to) |>

    draw_echart_scatter_serie(serie = "earnings_from", index = 1, country = country_from) |>
    draw_echart_scatter_serie(serie = "earnings_to",   index = 2, country = country_to) |>
    echarts4r::e_grid(right = "5%", left = "5%", bottom = "5%", top = "9%") |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 0,
      max           = 100.1,
      alignTicks    = TRUE,
      name          = "Percentage\nBreakdown",
      nameTextStyle = list(align = "left"),
      axisLabel     = list(formatter = '{value}%')
    ) |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 1,
      position      = "right",
      alignTicks    = TRUE,
      axisLabel     = list(rotate = 45, formatter = get_echart_tooltip(country_from)),
      name          = "Annual\nEarnings",
      nameTextStyle = list(align = "right")
    ) |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 2,
      position      = "right",
      axisLabel     = list(rotate = 45, formatter = get_echart_tooltip(country_to)),
      alignTicks    = TRUE,
      offset        = 20
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_tooltip(axisPointer = list(type = "shadow"))
}
