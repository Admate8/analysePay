#' Plot Total Expenditure
#'
#' @param selected_percentile selected_percentile() reactive.
#' @param df df_expend_total() reactive.
#'
#' @noRd
plot_all_expend <- function(selected_percentile, df) {

  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  base_style   <- paste0("<span style=\"color: ", palette_global$categories$base_color, ";\">")
  target_style <- paste0("<span style=\"color: ", palette_global$categories$target_color, ";\">")

  df <- df |>
    dplyr::filter(percentile == selected_percentile) |>
    dplyr::select(dplyr::contains("perc")) |>
    dplyr::select(-percentile) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::mutate(
      name        = sub("_perc_tot", "", name),
      destination = sub(".*_", "", name)
    ) |>
    dplyr::group_by(destination) |>
    dplyr::summarise(
      value_sum = 100 * sum(value, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    tidyr::pivot_wider(names_from = "destination", values_from = "value_sum") |>
    dplyr::mutate(
      dummy_val = "dummy",
      fake_bar_1 = abs(from - to),
      fake_bar_2 = ifelse(from >= to, to, from)
    ) |>
    dplyr::rename(base = from, target = to)

  get_min <- 0
  get_max <- df$base + df$target

  df |>
    echarts4r::e_chart(x = dummy_val) |>
    echarts4r::e_bar(
      serie    = fake_bar_2,
      stack    = "bar",
      barWidth = "10%",
      color    = "transparent",
      tooltip  = list(show = FALSE)
    ) |>
    echarts4r::e_bar(
      serie    = fake_bar_1,
      stack    = "bar",
      barWidth = "10%",
      color    = palette_global$categories$earnings_color,
      tooltip  = list(show = FALSE)
    ) |>
    echarts4r::e_scatter(
      serie       = base,
      symbol_size = 40,
      color       = palette_global$categories$base_color,
      itemStyle   = list(
        opacity     = 1,
        shadowBlur  = 20,
        shadowColor = palette_global$categories$base_color
      ),
      label = list(
        show       = TRUE,
        position   = "bottom",
        distance   = 15,
        fontSize   = "0.9rem",
        color      = palette_global$categories$base_color,
        fontWeight = "bold",
        formatter  = htmlwidgets::JS(
          "function(params){
          return Intl.NumberFormat(undefined, {
            style: 'percent',
            maximumFractionDigits: 2
          }).format(params.value[0] / 100);
        }"
        )
      ),
      emphasis = list(label = list(show = FALSE))
    ) |>
    echarts4r::e_scatter(
      serie       = target,
      symbol_size = 40,
      color       = palette_global$categories$target_color,
      itemStyle   = list(
        opacity     = 1,
        shadowBlur  = 20,
        shadowColor = palette_global$categories$target_color
      ),
      label = list(
        show       = TRUE,
        position   = "top",
        distance   = 15,
        fontSize   = "0.9rem",
        color      = palette_global$categories$target_color,
        fontWeight = "bold",
        formatter  = htmlwidgets::JS(
          "function(params){
          return Intl.NumberFormat(undefined, {
            style: 'percent',
            maximumFractionDigits: 2
          }).format(params.value[0] / 100);
        }"
        )
      ),
      emphasis = list(label = list(show = FALSE))
    ) |>
    echarts4r::e_y_axis(show = FALSE, max = get_max, min = get_min) |>
    echarts4r::e_x_axis(show = FALSE) |>
    echarts4r::e_grid(left = "5%", right = "5%") |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_tooltip(
      backgroundColor = palette_global$body_tertiary_bg,
      borderColor     = palette_global$body_tertiary_bg,
      textStyle       = list(color = palette_global$body_color),
      borderRadius    = 25,
      padding         = 15,
      formatter       = htmlwidgets::JS(sprintf(
        "function(params) {
       let tooltip = 'Combined ';

        if (params.seriesName === 'base') {
          tooltip += '%s<b>' + params.seriesName + '</b></span>';
        } else {
          tooltip += '%s<b>' + params.seriesName + '</b></span>';
        }

        tooltip += ' expenditure constitute<br><b>'

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
      ", base_style, target_style, base_style, target_style))
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title(
      text = paste0(
        "Expenditure for the ",
        selected_percentile,
        update_percentile_suffix(selected_percentile),
        " Percentile"
      ),
      left = 0,
      textStyle = list(
        color      = palette_global$body_color,
        fontWeight = "lighter",
        fontSize   = "1.5rem"
      )
    )
}

