#' Plot Earnings & Deductions Distribution by Percentile
#'
#' @param selected_decile Numeric value between 10 and 95. Constrained user selection.
#' @param df df_main().
#' @param period Either "year", "month" or "week". Constrained user selection.
#'
#' @noRd
plot_earnings_by_percentiles <- function(selected_decile, df, period) {

  # Define global settings ----
  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  currency_from <- base::get(paste0(country_from, "_settings"))$global$currency
  currency_to   <- base::get(paste0(country_to, "_settings"))$global$currency
  locale_from   <- base::get(paste0(country_from, "_settings"))$global$locale
  locale_to     <- base::get(paste0(country_to, "_settings"))$global$locale

  # Draw Custom Bar Series ----
  draw_echart_bar_series <- function(e_chart_object, serie, name, color, stack, index, country = "uk") {
    e_chart_object |>
      echarts4r::e_line_(
        serie     = serie,
        name      = name,
        color     = palette_global$categories[[color]],
        stack     = stack,
        y_index   = index,
        x_index   = index,
        itemStyle = list(color = get_gradient(color, 0.3)),
        emphasis  = list(
          itemStyle = list(color = get_gradient(color, 0.3, TRUE)),
          areaStyle = list(color = get_gradient(color, 0.3, TRUE))
        ),
        symbol    = "none",
        areaStyle = list(color = get_gradient(color, 0.3))
      )
  }

  # Draw Custom Scatter Series ----
  draw_echart_scatter_series <- function(..., serie, name, x_index, y_index, country = "uk") {
    echarts4r::e_scatter_(
      serie       = serie,
      name        = name,
      color       = palette_global$categories$earnings_color,
      stack       = stack,
      symbol_size = 5,
      y_index     = y_index,
      x_index     = x_index,
      itemStyle   = list(opacity = 1),
      emphasis    = list(focus = "series"),
      legend      = list(show = FALSE),
      ...
    )
  }

  # Define a set of objects for the echart ----
  set_of_series <- paste0(c(
    "net_income_perc_",
    "pension_mandatory_perc_",
    "pension_voluntary_perc_",
    "insurance_mandatory_perc_",
    "insurance_voluntary_perc_",
    "income_tax_perc_",
    "student_loan_plan_2_perc_",
    "student_loan_plan_3_perc_"
  ), c(rep("from", 8), rep("to", 8)))
  set_of_names <- unname(sapply(set_of_series, function(x) stringr::str_to_title(gsub("_", " ", gsub("_perc|_from|_to", "", x)))))

  # Must match the order above
  set_of_colors <- rep(c(
    palette_global$categories$net_color,
    palette_global$categories$pension_color,
    palette_global$categories$pension_color_vol,
    palette_global$categories$insurance_color,
    palette_global$categories$insurance_color_vol,
    palette_global$categories$tax_color,
    palette_global$categories$sl_plan2_color,
    palette_global$categories$sl_plan3_color
  ), 2)

  set_of_stacks    <- c(rep("0", 8), rep("1", 8))
  set_of_indexes   <- c(rep(1, 8), rep(0, 8))
  set_of_countries <- c(rep(country_from, 8), rep(country_to, 8))

  # Scale values based on the period argument ----
  scale_factor <- ifelse(period == "year", 1, ifelse(period == "month", 12, 52.1429))
  df <- df |>
    dplyr::mutate(dplyr::across(
      -c(dplyr::contains("actuals"), dplyr::contains("perc"), dplyr::contains("country"), "percentile"),
      function(x) x / scale_factor
    ))


  purrr::reduce(
    .x = seq_along(set_of_series),

    # Start with an initialized chart
    .init = df |>
      dplyr::mutate(
        percentile = paste0(as.factor(percentile), "th"),
        actual_earnings_from = ifelse(actuals_from == 0, NA, earnings_from),
        actual_earnings_to   = ifelse(actuals_to == 0, NA, earnings_to),
        dplyr::across(dplyr::contains("perc") & !dplyr::contains("percentile"), ~.x * 100)
      ) |>
      echarts4r::e_chart(x = percentile),

    # Apply the function with various settings from the lists
    .f = ~ draw_echart_bar_series(
      .x,
      set_of_series[.y],
      set_of_names[.y],
      set_of_colors[.y],
      set_of_stacks[.y],
      set_of_indexes[.y],
      set_of_countries[.y]
    )
  ) |>
    draw_echart_scatter_series(serie = "earnings_from", name = "Earnings", x_index = 1, y_index = 2, country = country_from) |>
    draw_echart_scatter_series(serie = "earnings_to",   name = "Earnings", x_index = 0, y_index = 3, country = country_to) |>
    echarts4r::e_grid(top = "23%", right = "5%", left = "3%", height = "35%") |>
    echarts4r::e_grid(top = "62%", right = "5%", left = "3%", height = "35%") |>
    echarts4r::e_y_axis(
      gridIndex  = 1,
      index      = 0,
      max        = 100.1,
      alignTicks = TRUE,
      position   = "left",
      axisLabel  = list(formatter = '{value}%'),
      splitLine  = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 1,
      alignTicks    = TRUE,
      max           = 100.1,
      position      = "left",
      name          = "Percentage\nBreakdown",
      nameTextStyle = list(align = "left"),
      axisLabel     = list(formatter = '{value}%'),
      splitLine     = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 2,
      position      = "right",
      alignTicks    = TRUE,
      name          = "Gross\nEarnings",
      nameTextStyle = list(align = "right"),
      axisLabel     = list(rotate = 45, formatter = get_echart_tooltip(country_from)),
      splitLine     = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_y_axis(
      gridIndex  = 1,
      index      = 3,
      position   = "right",
      alignTicks = TRUE,
      axisLabel  = list(rotate = 45, formatter = get_echart_tooltip(country_to)),
      splitLine  = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_x_axis(
      gridIndex     = 1,
      index         = 0,
      nameLocation  = "middle",
      nameTextStyle = list(fontWeight = "bold", padding = 15),
      axisTick      = list(alignWithLabel = TRUE),
      axisLabel     = list(interval = 10 - 1)
    ) |>
    echarts4r::e_x_axis(
      gridIndex   = 0,
      index       = 1,
      axisLabel   = list(show = FALSE),
      axisTick    = list(alignWithLabel = TRUE),
      axisLabel   = list(interval = 10 - 1, show = FALSE)
    ) |>
    echarts4r::e_tooltip(
      trigger         = "axis",
      backgroundColor = palette_global$body_tertiary_bg,
      borderColor     = palette_global$body_tertiary_bg,
      textStyle       = list(color = palette_global$body_color),
      borderRadius    = 25,
      axisPointer     = list(
        type = "shadow",
        label = list(formatter = htmlwidgets::JS(
          "function(params) {
          let value = parseInt(params.value); // Ensure the value is an integer
          let suffix = 'th'; // Default suffix

          // Handle special cases for 'st', 'nd', and 'rd'
          if (value % 10 === 1 && value % 100 !== 11) {
            suffix = 'st';
          } else if (value % 10 === 2 && value % 100 !== 12) {
            suffix = 'nd';
          } else if (value % 10 === 3 && value % 100 !== 13) {
            suffix = 'rd';
          }

          return value.toFixed(0) + suffix + ' Percentile';
        }
        "
        ))
      ),
      formatter = htmlwidgets::JS(sprintf(
        "function(params) {
        let isTopGrid = params[0].axisIndex === 1;
        let valueFrom, valueTo;

        // Initiate the tooltip
        let tooltip = '<table>';
        tooltip += '<tr><th colspan=\"4\" style=\"text-align: center; font-size: 1.5em; font-weight: bold;\">' + params[0].axisValueLabel + '</th></tr>';
        tooltip += '<tr><td colspan=\"4\" style=\"padding: 15px 0;\"></td></tr>';

        // Table headers
        tooltip += '<tr>';
        tooltip += '<td></td>';
        tooltip += '<td style=\"padding-right: 10px; text-align: center; color: #FFD166;\"><b>Base</b></td>';
        tooltip += '<td></td>';
        tooltip += '<td style=\"padding-right: 10px; text-align: center; color: #468189;\"><b>Target</b></td>';
        tooltip += '</tr>';
        tooltip += '<tr><td colspan=\"4\" style=\"padding: 5px 0;\"><hr style=\"margin: 2px 0; border: none; border-top: 1px solid #ccc;\"></td></tr>';

        // Check if the last available series has the name 'Earnings'
        // When this series is not available in the legend, this condition is always true

        let hasEarnings = params[params.length - 1].seriesName === 'Earnings';
        let earningsIndex = params.length / 2 - 1;

        // Format as currency
        earningsValueFrom = new Intl.NumberFormat('%s', {
          style: 'currency',
          currency: '%s',
          maximumFractionDigits: 2
        }).format(params[isTopGrid ? earningsIndex : earningsIndex + params.length / 2].value[1]);

        earningsValueTo = new Intl.NumberFormat('%s', {
          style: 'currency',
          currency: '%s',
          maximumFractionDigits: 2
        }).format(params[isTopGrid ? earningsIndex + params.length / 2 : earningsIndex].value[1]);

        // Table part 1: over the horizontal line
        tooltip += '<tr>';
        tooltip += '<td style=\"padding-right: 20px;\">' + params[isTopGrid ? earningsIndex : earningsIndex + params.length / 2].marker + ' <b>' + params[isTopGrid ? earningsIndex : earningsIndex + params.length / 2].seriesName + '</b></td>';
        tooltip += '<td style=\"padding-right: 10px;\"><b>' + earningsValueFrom + '</b></td>';
        tooltip += '<td style=\"padding-right: 10px;\"><b>➡</b></td>';
        tooltip += '<td><b>' + earningsValueTo + '</b></td>';
        tooltip += '</tr>';

        // Add horizontal line
        tooltip += '<tr><td colspan=\"4\" style=\"padding: 5px 0;\"><hr style=\"margin: 2px 0; border: none; border-top: 1px solid #ccc;\"></td></tr>';

        // Loop over the remaining series
        for (let i = 0; i < params.length / 2 - 1; i++) {

            // Format as percentages
            valueFrom = new Intl.NumberFormat(undefined, {
              style: 'percent',
              maximumFractionDigits: 2
            }).format(params[isTopGrid ? i : i + params.length / 2].value[1] / 100);

            valueTo = new Intl.NumberFormat(undefined, {
              style: 'percent',
              maximumFractionDigits: 2
            }).format(params[isTopGrid ? i + params.length / 2 : i].value[1] / 100);

           // Table part 2: under the horizontal line
          tooltip += '<tr>';
          tooltip += '<td style=\"padding-right: 20px;\">' + params[isTopGrid ? i : i + params.length / 2].marker + ' ' + params[isTopGrid ? i : i + params.length / 2].seriesName + '</td>';
          tooltip += '<td style=\"padding-right: 10px;\">' + valueFrom + '</td>';
          tooltip += '<td style=\"padding-right: 10px;\">➡</td>';
          tooltip += '<td>' + valueTo + '</td>';
          tooltip += '</tr>';
        }
        tooltip += '</table>';

        return tooltip; }",
        locale_from, currency_from, locale_to, currency_to
      ))

    ) |>
    echarts4r::e_axis_pointer(link = list(xAxisIndex = c(1, 0))) |>
    echarts4r::e_legend(
      top               = "top",
      left              = "center",
      height            = "70px",
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
      selector          = list(
        list(type = "all", title = "Select All"),
        list(type = "inverse", title = "Select Inverse")
      )
    )
}
