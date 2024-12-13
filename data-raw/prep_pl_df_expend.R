# Note ----
# This script must be checked whenever the source data is updated because its
# shape might change, breaking the pipeline below. Sadly, GUS does not provide
# this data via API, and so until it does, one must rely on data extracts from
# their website...

# Data ----
# GUS (PL) publishes the average expenditure per person by socio-economic groups in CY
# Link: https://stat.gov.pl/obszary-tematyczne/warunki-zycia/dochody-wydatki-i-warunki-zycia-ludnosci/sytuacja-gospodarstw-domowych-w-2023-r-w-swietle-badania-budzetow-gospodarstw-domowych,3,23.html

# Aim ----
# Compare average expenditure per person per year by gross income decile

# Issues ----
# 1. PL data shows the average expend per person (i.e. no income decile split)
## Solution: Use the corresponding average in the UK to calculate the percentage
## change to the overall mean by expenditure. Use these values as a proxy to
## calculate PL expend per person per decile. (ASSUMPTION: expenditure in PL
## change by similar amount in respect to average as in the UK) No data to show
## that e.g. 8th decile in the UK spends more on food compared to the UK average
## than the 8th decile in PL compared to PL average.


pl_df_expend_raw <- openxlsx::read.xlsx(here::here("./data-raw/pl_df_expend_raw.xlsx"), sheet = "T.3")

pl_expend_total <- 12 * as.numeric(pl_df_expend_raw[7, 2])
# Check that the order hasn't changed !!!
pl_df_expend_perc <- pl_df_expend_raw |>
  dplyr::select(1, 2) |>
  dplyr::slice(-c(1:8)) |>
  # Select only the core expenditure
  dplyr::slice(3:6, 8:9, 13:17, 19, 21) |>
  dplyr::select(2) |>
  dplyr::mutate(perc = as.numeric(X2) / 100, .keep = "none") |>
  # The numbers don't add to 1 - lump the remaining values to the "Other expenditure items"
  dplyr::slice(-13) |>
  dplyr::pull(perc)
other_perc <- 1 - sum(pl_df_expend_perc)

# Compile the data
# uk_df_expend comes from `prep_uk_df_expend.R`
pl_df_expend <- uk_df_expend |>
  dplyr::select(decile, expenditure, perc_of_avg) |>
  dplyr::left_join(
    tibble::tibble(
      expenditure = unique(uk_df_expend$expenditure),
      value_avg   = c(pl_df_expend_perc, other_perc) * pl_expend_total
    ),
    by = "expenditure"
  ) |>
  dplyr::mutate(value = perc_of_avg * value_avg) |>
  dplyr::group_by(decile) |>
  dplyr::mutate(decile_perc = value / sum(value)) |>
  dplyr::ungroup() |>
  dplyr::select(decile, expenditure, value, value_avg, decile_perc, perc_of_avg)


# Save the data
usethis::use_data(pl_df_expend, overwrite = TRUE)
