#' Return Corresponding Decile Value from the Target Distribution
#'
#' Because we interpolated decile values, and deciles are a sequence from 10 to 95
#' by 0.1, it is not guaranteed that a custom earnings will have its decile. This
#' function checks this and return the point the closest to the supplied value.
#' For example, if 25000 does not have it's decile (but say, 25024 and 24900
#' are the 10.2th and 10th decile receptively), the function will return a point
#' c(10.2, 25024) because 25024 is closer to 25000 than 24900.
#'
#' @param annual_earnings Annual earnings in the "country from" (base) currency.
#' @param df Data returned by \code{get_df_earnings_dist()$df_main}.
#'
#' @return Two points - one with the closest value and decile from the base
#' distribution and the other corresponding decile with its value from the
#' target distribution.
#'
#' @noRd
map_deciles <- function(annual_earnings, df) {

  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  # Get the base and target distributions
  dist_from <- df$earnings_from
  dist_to   <- df$earnings_to

  # Find the closest value
  closest_index <- which.min(abs(dist_from - annual_earnings))

  corresponding_decile     <- df$deciles[closest_index]
  corresponding_value_from <- df$earnings_from[closest_index]
  corresponding_value_to   <- df$earnings_to[closest_index]

  # Return as points
  return(list(
    "point_from" = c(corresponding_decile, corresponding_value_from),
    "point_to"   = c(corresponding_decile, corresponding_value_to)
  ))
}


#' Plot the Interpolated Earnings with Nominal Deductions Breakdown by Deciles
#'
#' @param df Data returned by \code{get_df_earnings_dist()$df_main}
#' @param period Either "year", "month" or "week" to scale the values
#'
#' @noRd
plot_int_earnings_decile_dist <- function(df, period) {
  stopifnot(period %in% c("year", "month", "week"))

  # Define global settings ----
  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  currency_from <- base::get(paste0(country_from, "_settings"))$global$currency
  currency_to   <- base::get(paste0(country_to, "_settings"))$global$currency
  locale_from   <- base::get(paste0(country_from, "_settings"))$global$locale
  locale_to     <- base::get(paste0(country_to, "_settings"))$global$locale

  # Define a set of objects for the echart ----
  set_of_series <- paste0(c(
    "earnings_",
    "actual_earnings_",
    "net_income_",
    "student_loan_plan_3_",
    "student_loan_plan_2_",
    "income_tax_",
    "insurance_voluntary_",
    "insurance_mandatory_",
    "pension_voluntary_",
    "pension_mandatory_"
  ), c(rep("from", 10), rep("to", 10)))

  set_of_names <- unname(sapply(set_of_series, function(x) stringr::str_to_title(gsub("_", " ", gsub("_perc|_from|_to", "", x)))))

  # Must match the order above
  set_of_colors <- rep(c(
    palette_global$body_color,
    palette_global$body_color,
    palette_global$categories$net_color,
    palette_global$categories$sl_plan3_color,
    palette_global$categories$sl_plan2_color,
    palette_global$categories$tax_color,
    palette_global$categories$insurance_color_vol,
    palette_global$categories$insurance_color,
    palette_global$categories$pension_color_vol,
    palette_global$categories$pension_color
  ), 2)

  set_of_indexes  <- c(rep(1, 10), rep(0, 10))
  set_of_stacks   <- c(rep("1", 10), rep("0", 10))

  set_of_gradients      <- stringr::str_remove_all(unname(sapply(set_of_colors, get_gradient, 0.3)), "\n")
  set_of_gradients_emph <- stringr::str_remove_all(unname(sapply(set_of_colors, get_gradient, 0.3, TRUE)), "\n")

  set_of_types <- c(
    "line", "scatter", rep("area", 8),
    "line", "scatter", rep("area", 8)
  )

  # Define a function to draw custom echart series ----
  draw_echart_area_serie <- function(
    e_chart_object, serie, name, color, index, stack, col_gradient,
    col_gradient_emph, type
  ) {
    stopifnot(type %in% c("area", "line", "scatter"))

    if (type == "area") {
      e_chart_object |>
        echarts4r::e_line_(
          serie     = serie,
          name      = name,
          symbol    = "none",
          lineStyle = list(width = 0.5, color = color),
          color     = color,
          y_index   = index,
          x_index   = index,
          stack     = stack,
          itemStyle = list(color = color),
          emphasis  = list(areaStyle = list(color = htmlwidgets::JS(col_gradient_emph)), focus = "series"),
          areaStyle = list(color = htmlwidgets::JS(col_gradient))
        )
    } else if (type == "line") {
      e_chart_object |>
        echarts4r::e_line_(
          serie     = serie,
          name      = name,
          symbol    = "none",
          lineStyle = list(width = 2, color = color),
          color     = color,
          y_index   = index,
          x_index   = index,
          itemStyle = list(color = color)
        )
    } else {
      e_chart_object |>
        echarts4r::e_scatter_(
          serie       = serie,
          zlevel      = 1,
          name        = name,
          symbol_size = 10,
          color       = color,
          y_index     = index,
          x_index     = index,
          itemStyle   = list(opacity = 1),
          legend      = list(show = FALSE),
          tooltip     = list(show = FALSE)
        )
    }
  }


  # Scale values based on the period argument ----
  scale_factor <- ifelse(period == "year", 1, ifelse(period == "month", 12, 52.1429))
  df <- df |>
    dplyr::mutate(dplyr::across(
      -c(dplyr::contains("actuals"), dplyr::contains("perc"), dplyr::contains("country"), "deciles"),
      function(x) x / scale_factor
    ))

  purrr::reduce(
    .x = seq_along(set_of_series),

    # Start with an initialized chart
    .init = df |>
      dplyr::mutate(
        deciles = paste0(as.factor(deciles), "th"),
        actual_earnings_from = ifelse(actuals_from == 0, NA, earnings_from),
        actual_earnings_to   = ifelse(actuals_to == 0, NA, earnings_to)
      ) |>
      echarts4r::e_chart(x = deciles),

    # Apply the function with various settings from the lists
    .f = ~ draw_echart_area_serie(
      .x,
      set_of_series[.y],
      set_of_names[.y],
      set_of_colors[.y],
      set_of_indexes[.y],
      set_of_stacks[.y],
      set_of_gradients[.y],
      set_of_gradients_emph[.y],
      set_of_types[.y]
    )
  ) |>
    echarts4r::e_grid(top = "22%", right = "1.5%", left = "7.5%", height = "35%") |>
    echarts4r::e_grid(top = "62%", right = "1.5%", left = "7.5%", height = "35%") |>
    echarts4r::e_y_axis(
      gridIndex = 1,         index = 0, position = "left",
      max       = "dataMax",  name = "Target Country",
      nameTextStyle = list(
        align      = "left",
        color      = palette_global$body_color_secondary,
        fontWeight = "bold"
      ),
      axisLabel = list(formatter = get_echart_tooltip(country_to)),
      splitLine = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_y_axis(
      gridIndex = 0,         index = 1, position = "left",
      max       = "dataMax", name  = "Base Country",
      nameTextStyle = list(
        align      = "left",
        color      = palette_global$body_color_secondary,
        fontWeight = "bold"
      ),
      axisLabel = list(formatter = get_echart_tooltip(country_from)),
      splitLine = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_x_axis(
      gridIndex     = 1,
      index         = 0,
      #name          = "Deciles",
      nameLocation  = "middle",
      nameTextStyle = list(fontWeight = "bold", padding = 15),
      axisLabel     = list(interval = 100 - 1)
    ) |>
    echarts4r::e_x_axis(
      gridIndex   = 0,
      index       = 1,
      axisLabel   = list(show = FALSE, interval = 100 - 1)
    ) |>
    echarts4r::e_tooltip(
      trigger         = "axis",
      backgroundColor = palette_global$body_tertiary_bg,
      borderColor     = palette_global$body_tertiary_bg,
      textStyle       = list(color = palette_global$body_color),
      borderRadius    = 25,
      axisPointer     = list(
        label = list(formatter = htmlwidgets::JS(
          "function(params) {
          return 'Decile ' + parseFloat(params.value).toFixed(1) + 'th';
        }"
        ))
      ),
      formatter = htmlwidgets::JS(sprintf(
        "function(params) {
        let isTopGrid = params[0].axisIndex === 1;
        let tooltip = '<table>';

        tooltip += '<tr><th colspan=\"4\" style=\"text-align: center;\">' + params[0].axisValueLabel + '</th></tr>';
        tooltip += '<tr><td colspan=\"4\"><hr></td></tr>';

        for (let i = 0; i < params.length / 2; i++) {

          let valueFrom = new Intl.NumberFormat('%s', {
            style: 'currency',
            currency: '%s',
            maximumFractionDigits: 0
          }).format(params[isTopGrid ? i : i + params.length / 2].value[1]);

          let valueTo = new Intl.NumberFormat('%s', {
            style: 'currency',
            currency: '%s',
            maximumFractionDigits: 0
          }).format(params[isTopGrid ? i + params.length / 2 : i].value[1]);

          tooltip += '<tr>';
          tooltip += '<td style=\"padding-right: 20px;\">' + params[isTopGrid ? i : i + params.length / 2].marker + ' ' + params[isTopGrid ? i : i + params.length / 2].seriesName + '</td>';
          tooltip += '<td style=\"padding-right: 10px;\">' + valueFrom + '</td>';
          tooltip += '<td style=\"padding-right: 10px;\">\U2794</td>';
          tooltip += '<td>' + valueTo + '</td>';
          tooltip += '</tr>';
        }
        tooltip += '</table>';

        return tooltip; }"
        , locale_from, currency_from, locale_to, currency_to
      ))
    ) |>
    echarts4r::e_axis_pointer(link = list(xAxisIndex = c(1, 0))) |>
    echarts4r::e_legend(
      top               = "top",
      left              = "center",
      height            = "60px",
      orient            = "vertical",
      padding           = 15,
      backgroundColor   = palette_global$body_secondary_bg,
      borderColor       = palette_global$body_secondary_bg,
      textStyle         = list(color = palette_global$body_color),
      lineStyle         = list(inactiveColor = get_hex_colour_shade(palette_global$body_color_secondary, -0.5)),
      inactiveColor     = get_hex_colour_shade(palette_global$body_color_secondary, -0.5),
      borderRadius      = 25,
      selectorLabel     = list(
        fontWeight      = "bold",
        color           = palette_global$body_color,
        backgroundColor = palette_global$body_bg,
        borderColor     = palette_global$body_bg,
        padding         = 10
      ),
      selectorButtonGap = 20,
      # This will generate a warning, but can be replaced with a list of actual series
      selected          = stats::setNames(
        rep(FALSE, length(setdiff(set_of_names, c("Earnings", "Actual Earnings")))),
        setdiff(set_of_names, c("Earnings", "Actual Earnings"))
      ),
      selector          = list(
        list(type = "all", title = "Select All"),
        list(type = "inverse", title = "Inverse")
      )
    )
}



proxy_int_earnings_decile_dist <- function(plot, annual_earnings, df, period) {

  # Define global settings ----
  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)


  # Define points ----
  points       <- map_deciles(annual_earnings, df)
  scale_factor <- ifelse(period == "year", 1, ifelse(period == "month", 12, 52.1429))
  ## Scale the y-coordinates of the points
  points$point_from[2] <- points$point_from[2] / scale_factor
  points$point_to[2]   <- points$point_to[2] / scale_factor
  max_dist_to          <- (max(df$earnings_to, na.rm = TRUE) / scale_factor) - 1


  plot |>
    echarts4r::e_mark_p(
      type        = "line",
      serie_index = 1,
      emphasis    = list(disabled = TRUE),
      lineStyle   = list(color = "red"),
      data        = list(
        list(xAxis = "0th", yAxis = points$point_from[[2]], symbolSize = 6),
        list(xAxis = paste0(points$point_from[[1]], "th"), yAxis = points$point_from[[2]], symbol = "none")
      )
    ) |>
    echarts4r::e_mark_p(
      type        = "line",
      serie_index = 1,
      emphasis    = list(disabled = TRUE),
      lineStyle   = list(color = "red"),
      data        = list(
        list(xAxis = paste0(points$point_from[[1]], "th"), yAxis = points$point_from[[2]], symbol = "none"),
        list(xAxis = paste0(points$point_from[[1]], "th"), yAxis = 0, symbol = "none")
      )
    ) |>

    echarts4r::e_mark_p(
      type        = "line",
      serie_index = 11,
      emphasis    = list(disabled = TRUE),
      lineStyle   = list(color = "red"),
      data        = list(
        list(xAxis = paste0(points$point_to[[1]], "th"), yAxis = max_dist_to, symbol = "none"),
        list(xAxis = paste0(points$point_to[[1]], "th"), yAxis = points$point_to[[2]], symbol = "none")
      )
    ) |>
    echarts4r::e_mark_p(
      type        = "line",
      serie_index = 11,
      emphasis    = list(disabled = TRUE),
      lineStyle   = list(color = "red"),
      data        = list(
        list(xAxis = paste0(points$point_to[[1]], "th"), yAxis = points$point_to[[2]], symbol = "none"),
        list(xAxis = "0th", yAxis = points$point_to[[2]], symbolSize = 6)
      )
    ) |>
    echarts4r::e_merge()

}


plot_radar_perc <- function(annual_earnings, df) {

  country_from  <- purrr::discard(unique(df$country_from), is.na)
  country_to    <- purrr::discard(unique(df$country_to), is.na)
  points_decile <- map_deciles(annual_earnings, df)$point_from[1]

  # Prepare the data for the Radar chart
  df_plot <- df |>
    dplyr::filter(deciles == points_decile) |> # Filter for a single decile
    dplyr::select(dplyr::contains("perc")) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::mutate(
      value       = ifelse(is.na(value), 0L, value),
      destination = ifelse(grepl("_perc_from$", name), "from", "to"),
      name        = stringr::str_replace_all(sub("_perc_(from|to)$", "", name), "_", " "),
      name        = stringr::str_to_title(name),
      value       = round(100 * value, 2)
    ) |>
    tidyr::pivot_wider(
      names_from  = "destination",
      values_from = "value"
    ) |>
    #cbind("max" = c(15, 10, 10, 5, 25, 10, 10, 100)) |>
    dplyr::arrange(dplyr::desc(dplyr::row_number()))

  # Define the radar chart options with different max values for each axis
  radar_options <- list(
    list(name = "Net\nIncome",          max = 100, color = palette_global$categories$net_color),
    list(name = "Student\nLoan\nPlan 3", max = 10,  color = palette_global$categories$sl_plan3_color),
    list(name = "Student\nLoan\nPlan 2", max = 10,  color = palette_global$categories$sl_plan2_color),
    list(name = "Income\nTax",          max = 25,  color = palette_global$categories$tax_color),
    list(name = "Insurance\nVoluntary", max = 5 ,  color = palette_global$categories$insurance_color_vol),
    list(name = "Insurance\nMandatory", max = 15,  color = palette_global$categories$insurance_color),
    list(name = "Pension\nVoluntary",   max = 10,  color = palette_global$categories$pension_color_vol),
    list(name = "Pension\nMandatory",   max = 15,  color = palette_global$categories$pension_color)
  )

  # Create radar chart
  df_plot |>
    echarts4r::e_charts(name) |>
    echarts4r::e_radar(
      from,
      name      = "Base (%)",
      areaStyle = list(opacity = 0.2),
      lineStyle = list(width = 0.5),
      symbol    = "diamond"
    ) |>
    echarts4r::e_radar(to, name = "Target (%)", areaStyle = list()) |>
    echarts4r::e_color(color = c("#FCEC52", "#F44174")) |>
    echarts4r::e_radar_opts(
      indicator = radar_options,
      nameGap   = 20,
      radius    = "62%",
      axisName  = list(
        backgroundColor = palette_global$body_tertiary_bg,
        padding         = 10,
        borderRadius    = 15,
        fontSize        = 10,
        fontWeight      = "bold"
      ),
      #axisTick = list(show = TRUE, color = palette_global$body_color_secondary, width = 0.5),
      axisLabel = list(
        show         = TRUE,
        formatter    = "{value}%",
        showMinLabel = FALSE,
        fontSize     = "0.55rem"
      ),
      splitArea = list(
        show = TRUE,
        areaStyle = list(
          color = htmlwidgets::JS(paste0(
            "new echarts.graphic.RadialGradient(0.5, 0.5, 1, [
              { offset: 0, color: '", palette_global$body_color_secondary, "' },
              { offset: 0.5, color: '", palette_global$body_bg, "' }
            ])
          "
          ))
        ),
        lineStyle = list(color = palette_global$body_color_secondary, width = 0.5)
      ),
      splitLine = list(show = FALSE)
    ) |>
    echarts4r::e_tooltip(
      backgroundColor = palette_global$body_tertiary_bg,
      borderColor     = palette_global$body_tertiary_bg,
      textStyle       = list(color = palette_global$body_color),
      borderRadius    = 25
    ) |>
    echarts4r::e_legend(
      textStyle = list(color = palette_global$body_color_secondary),
      right = 0
    ) |>
    echarts4r::e_title(
      text = "Deductions Breakdown",
      textStyle = list(color = palette_global$body_color_secondary)
    )
}
