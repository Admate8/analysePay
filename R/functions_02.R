
#' Plot Earnings by Percentile Deductions Breakdown
#'
#' @param df Data returned by \code{get_df_earnings_dist()$df_main}
#'
#' @noRd
plot_earnings_percentile_dist <- function(df) {

  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)


  # Draw Custom Bar Serie ----
  draw_echart_bar_serie <- function(e_chart_object, serie, color, stack, country = "uk") {

    col_gradient      <- get_gradient(color, 0.3)
    col_gradient_emph <- get_gradient(color, 0.3, TRUE)
    col               <- palette_global$categories[[color]]
    name              <- paste0(toupper(country), ": ", stringr::str_to_title(gsub("_", " ", gsub("_perc|_from|_to", "", serie))))

    e_chart_object |>
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
        emphasis  = list(itemStyle = list(color = col_gradient_emph), focus = "series")
      )
  }

  # Draw Custom Scatter Serie
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
      itemStyle   = list(opacity = 1),
      emphasis    = list(focus = "series"),
      ...
    )
  }

  # Define a set of objects for the echart ----
  set_of_series <- paste0(c(
    "net_income_perc_",
    "student_loan_plan_3_perc_",
    "student_loan_plan_2_perc_",
    "income_tax_perc_",
    "insurance_voluntary_perc_",
    "insurance_mandatory_perc_",
    "pension_voluntary_perc_",
    "pension_mandatory_perc_"
  ), c(rep("from", 8), rep("to", 8)))

  # Must match the order above
  set_of_colors <- rep(c(
    palette_global$categories$net_color,
    palette_global$categories$sl_plan3_color,
    palette_global$categories$sl_plan2_color,
    palette_global$categories$tax_color,
    palette_global$categories$insurance_color_vol,
    palette_global$categories$insurance_color,
    palette_global$categories$pension_color_vol,
    palette_global$categories$pension_color
  ), 2)

  set_of_stacks    <- c(rep("0", 8), rep("1", 8))
  set_of_countries <- c(rep(country_from, 8), rep(country_to, 8))

  purrr::reduce(
    .x = seq_along(set_of_series),

    # Start with an initialized chart
    .init = df |>
      # Show only available deciles from the sources
      dplyr::filter(actuals_from == 1 & actuals_to == 1) |>
      dplyr::mutate(
        percentile = paste0(as.factor(percentile), "th"),
        dplyr::across(dplyr::contains("perc") & !dplyr::contains("percentile"), ~ round(.x * 100, 2))
      ) |>
      echarts4r::e_chart(x = percentile),

    # Apply the function with various settings from the lists
    .f = ~ draw_echart_bar_serie(
      .x,
      set_of_series[.y],
      set_of_colors[.y],
      set_of_stacks[.y],
      set_of_countries[.y]
    )
  ) |>
    draw_echart_scatter_serie(serie = "earnings_from", index = 1, country = country_from) |>
    draw_echart_scatter_serie(serie = "earnings_to",   index = 2, country = country_to) |>
    echarts4r::e_grid(right = "6%", left = "3%", bottom = "5%", top = "9%") |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 0,
      max           = 100.1,
      alignTicks    = TRUE,
      name          = "Percentage\nBreakdown",
      nameTextStyle = list(align = "left"),
      axisLabel     = list(formatter = '{value}%'),
      splitLine     = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 1,
      position      = "right",
      alignTicks    = TRUE,
      axisLabel     = list(rotate = 45, formatter = get_echart_tooltip(country_from)),
      name          = "Annual\nEarnings",
      nameTextStyle = list(align = "right"),
      splitLine     = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_y_axis(
      gridIndex     = 0,
      index         = 2,
      position      = "right",
      axisLabel     = list(rotate = 45, formatter = get_echart_tooltip(country_to)),
      alignTicks    = TRUE,
      offset        = 20,
      splitLine     = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_tooltip(
      axisPointer = list(type = "shadow"),
      backgroundColor = palette_global$body_tertiary_bg,
      textStyle       = list(color = palette_global$body_color),
      borderRadius    = 25,
      padding         = 15
    )
}
