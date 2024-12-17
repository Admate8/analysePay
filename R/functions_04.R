# df <- get_df_earnings_dist()$df_main
settings_from <- uk_settings
settings_to   <- pl_settings

country_from <- settings_from$global$short_cut
country_to   <- settings_to$global$short_cut

df_expend_from <- base::get(paste0(country_from, "_df_expend"))
df_expend_to   <- base::get(paste0(country_to, "_df_expend"))

df_plot <- dplyr::left_join(
  df_expend_from,
  df_expend_to,
  by = c("deciles", "expenditure"),
  suffix = c("_from", "_to")
) |>
  dplyr::left_join(
    df |> dplyr::select(deciles, net_income_from, net_income_to),
    by = "deciles"
  ) |>
  dplyr::filter(deciles != 100) |>
  dplyr::mutate(
    deciles          = paste0(deciles, "th"),
    expend_perc_from = value_from / net_income_from,
    expend_perc_to   = value_to / net_income_to,
    expenditure      = factor(expenditure, levels = rev(unique(df_expend_from$expenditure))),

    # Prepare fake bar lines for the lollipop chart
    fake_bar_1 = abs(expend_perc_from - expend_perc_to),
    fake_bar_2 = ifelse(expend_perc_from >= expend_perc_to, expend_perc_to, expend_perc_from)
  ) |>
  dplyr::arrange(expenditure, deciles)

df_plot |>
  dplyr::filter(deciles == "10th") |>
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
    tooltip  = list(show = FALSE)
  ) |>
  echarts4r::e_scatter(
    serie       = expend_perc_from,
    symbol_size = 15,
    itemStyle   = list(opacity = 1)
  ) |>
  echarts4r::e_scatter(
    serie       = expend_perc_to,
    symbol_size = 15,
    itemStyle   = list(opacity = 1)
  ) |>
  echarts4r::e_tooltip() |>
  echarts4r::e_legend(show = FALSE) |>
  echarts4r::e_flip_coords()


