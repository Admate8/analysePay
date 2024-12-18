#' Plot Deductions Breakdown
#'
#' @param selected_percentile selected_percentile() reactive.
#' @param df df_expend() reactive.
#'
#' @noRd
plot_expend_breakdown <- function(selected_percentile, df) {

  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  base_style   <- paste0("<span style=\"color: ", palette_global$categories$base_color, ";\">")
  target_style <- paste0("<span style=\"color: ", palette_global$categories$target_color, ";\">")

  df |>
    dplyr::filter(percentile == selected_percentile) |>
    dplyr::select(percentile, expenditure, base = interpolated_values_from_perc, target = interpolated_values_to_perc) |>
    # Add line breaks for cleaner display
    dplyr::mutate(
      expenditure = dplyr::case_when(
        expenditure == "Food & non-alcoholic drinks"           ~ "Food\n& non-alcoholic\ndrinks",
        expenditure == "Alcoholic drinks, tobacco & narcotics" ~ "Alcoholic drinks,\ntobacco\n& narcotics",
        expenditure == "Clothing & footwear"                   ~ "Clothing\n& footwear",
        expenditure == "Housing (net), fuel & power"           ~ "Housing (net),\nfuel & power",
        expenditure == "Household goods & services"            ~ "Household\ngoods\n& services",
        expenditure == "Health"                                ~ "Health",
        expenditure == "Transport"                             ~ "Transport",
        expenditure == "Communication"                         ~ "Communication",
        expenditure == "Recreation & culture"                  ~ "Recreation\n& culture",
        expenditure == "Education"                             ~ "Education",
        expenditure == "Restaurants & hotels"                  ~ "Restaurants\n& hotels",
        expenditure == "Miscellaneous goods & services"        ~ "Miscellaneous\ngoods & services",
        expenditure == "Other expenditure items"               ~ "Other\nexpenditure\nitems",
        expenditure == "Clothing & footwear"                   ~ "Clothing\n& footwear"
      ),
      base   = 100 * base,
      target = 100 * target,

      # Prepare fake bar lines for the lollipop chart
      fake_bar_1 = abs(base - target),
      fake_bar_2 = ifelse(base >= target, target, base)
    ) |>
    dplyr::arrange(factor(expenditure, levels = rev(c(
      "Food\n& non-alcoholic\ndrinks",
      "Alcoholic drinks,\ntobacco\n& narcotics",
      "Clothing\n& footwear",
      "Housing (net),\nfuel & power",
      "Household\ngoods\n& services",
      "Health",
      "Transport",
      "Communication",
      "Recreation\n& culture",
      "Education",
      "Restaurants\n& hotels",
      "Miscellaneous\ngoods & services",
      "Other\nexpenditure\nitems"
    )))) |>

    echarts4r::e_chart(x = expenditure) |>
    echarts4r::e_bar(
      serie    = fake_bar_2,
      stack    = "bar",
      barWidth = "1%",
      color    = "transparent",
      tooltip  = list(show = FALSE)
    ) |>
    echarts4r::e_bar(
      serie    = fake_bar_1,
      stack    = "bar",
      barWidth = "1%",
      color    = palette_global$categories$earnings_color,
      tooltip  = list(show = FALSE)
    ) |>
    echarts4r::e_scatter(
      serie       = base,
      symbol_size = 20,
      color       = palette_global$categories$base_color,
      itemStyle   = list(
        opacity     = 1,
        shadowBlur  = 20,
        shadowColor = palette_global$categories$base_color
      )
    ) |>
    echarts4r::e_scatter(
      serie       = target,
      symbol_size = 20,
      color       = palette_global$categories$target_color,
      itemStyle   = list(
        opacity     = 1,
        shadowBlur  = 20,
        shadowColor = palette_global$categories$target_color
      )
    ) |>
    echarts4r::e_y_axis(
      axisLabel  = list(formatter = '{value}%'),
      splitLine  = list(lineStyle = list(color = palette_global$body_tertiary_bg, width = 0.5))
    ) |>
    echarts4r::e_x_axis(
      axisTick  = list(alignWithLabel = TRUE),
      axisLabel = list(
        margin        = 100,
        align         = "left",
        verticalAlign = "center"
      )
    ) |>
    echarts4r::e_grid(left = "17%", right = "3.5%", bottom = "5%") |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_tooltip(
      backgroundColor = palette_global$body_tertiary_bg,
      borderColor     = palette_global$body_tertiary_bg,
      textStyle       = list(color = palette_global$body_color),
      borderRadius    = 25,
      padding         = 15,
      formatter       = htmlwidgets::JS(sprintf(
        "function(params) {
       let tooltip = '<b>';

        tooltip += Intl.NumberFormat(undefined, {
          style: 'percent',
          maximumFractionDigits: 2
        }).format(params.value[0] / 100);
        tooltip += '</b> of the '

        if (params.seriesName === 'base') {
          tooltip += '%s<b>' + params.seriesName + '</b></span>';
        } else {
          tooltip += '%s<b>' + params.seriesName + '</b></span>';
        }

        tooltip += ' net income'
        return tooltip;
      }
      ", base_style, target_style))
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title(
      text = "Expenditure Breakdown",
      left = 0,
      textStyle = list(
        color      = palette_global$body_color,
        fontWeight = "lighter",
        fontSize   = "1.5rem"
      )
    )
}
