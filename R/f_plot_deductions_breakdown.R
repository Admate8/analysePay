#' Plot Deductions Breakdown
#'
#' @param selected_percentile selected_percentile() reactive.
#' @param df df_main() reactive.
#'
#' @noRd
plot_deductions_breakdown <- function(selected_percentile, df) {

  country_from <- purrr::discard(unique(df$country_from), is.na)
  country_to   <- purrr::discard(unique(df$country_to), is.na)

  base_style   <- paste0("<span style=\"color: ", palette_global$categories$base_color, ";\">")
  target_style <- paste0("<span style=\"color: ", palette_global$categories$target_color, ";\">")

  df |>
    dplyr::filter(percentile == selected_percentile) |>
    dplyr::select(dplyr::contains("perc")) |>
    dplyr::select(-percentile, -net_income_perc_from, -net_income_perc_to) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::mutate(
      destination = sub(".*_", "", name),
      name        = gsub("(_perc_from|_perc_to)", "", name),
      name        = gsub("_", " ", name),
      name        = stringr::str_to_title(name),
      value       = 100 * value
    ) |>
    tidyr::pivot_wider(names_from = "destination", values_from = "value") |>
    dplyr::mutate(
      # Prepare fake bar lines for the lollipop chart
      fake_bar_1 = ifelse(
        is.na(from) & is.na(to), 0,
        ifelse(
          is.na(from), to,
          ifelse(is.na(to), from, abs(from - to))
        )
      ),
      fake_bar_2 = ifelse(
        fake_bar_1 == 0 | is.na(from) | is.na(to), 0,
        ifelse(from >= to, to, from)
      )
    ) |>
    dplyr::rename(base = from, target = to) |>
    dplyr::mutate(name = dplyr::case_when(
      name == "Pension Mandatory"   ~ "Pension\nMandatory",
      name == "Pension Voluntary"   ~ "Pension\nVoluntary",
      name == "Insurance Mandatory" ~ "Insurance\nMandatory",
      name == "Insurance Voluntary" ~ "Insurance\nVoluntary",
      name == "Income Tax"          ~ "Income Tax",
      name == "Student Loan Plan 2" ~ "Student Loan\nPlan 2",
      name == "Student Loan Plan 3" ~ "Student Loan\nPlan 3"
    )) |>
    dplyr::arrange(factor(name, levels = rev(name))) |>

    # Scatters occasionally will throw warnings when there are NAs
    # That is fine as we don't want the series if it's NA.
    echarts4r::e_chart(x = name) |>
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
        margin = 50,
        align = "center",
        verticalAlign = "center"
        #backgroundColor = palette_global$body_tertiary_bg,
        #padding         = 10,
        #borderRadius    = 15,
        #fontWeight      = "bold"
        #color = palette_global$body_color_secondary
      )
    ) |>
    echarts4r::e_grid(left = "15%", right = "5%", bottom = "5%") |>
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

        tooltip += ' gross income'
        return tooltip;
      }
      ", base_style, target_style))
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title(
      text = "Deductions Breakdown",
      left = "5%",
      textStyle = list(
        color      = palette_global$body_color,
        fontWeight = "lighter",
        fontSize   = "1.5rem"
      )
    )
}



