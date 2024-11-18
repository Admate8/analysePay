app_theme <- bslib::bs_theme(
  "body-bg" = "#1A1A1A",
  "body-color" = "#E0E0E0"
) |>
  bslib::bs_add_rules(sass::sass_file("inst/app/www/custom_themes.scss"))

palette_cat_wide <- tibble::tribble(
  ~category,               ~col,
  "Earnings",              "black",
  "Pension - Mandatory",   "#a4d8c2",
  "Pension - Voluntary",   "#b8d2c7",
  "Insurance - Mandatory", "#c6b38e",
  "Insurance - Voluntary", "#dcc392",
  "Tax",                   "#e01f54",
  "Student Loan",          "#001852",
  "Net Income",            "#f5e8c8"
)
