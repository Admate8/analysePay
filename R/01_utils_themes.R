palette_global <- list(
  "body_bg"           = "#1A1A1A",
  "body_secondary_bg" = "#2A2A2A",
  "body_tertiary_bg"  = "#3A3A3A",

  "body_color"        = "#E0E0E0",
  "primary"           = "#45B7D1"
)

app_theme <- bslib::bs_theme(
  "body-bg"           = palette_global$body_bg,
  "body-secondary-bg" = palette_global$body_secondary_bg,
  "body-tertiary-bg"  = palette_global$body_tertiary_bg,
  "body-color"        = palette_global$body_color,
  "primary"           = palette_global$primary,
  "accordion-icon-active-color" = palette_global$body_color,

  "accordion-border-radius" = "25px"
) |>
  bslib::bs_add_rules(sass::sass_file("inst/app/www/custom_themes.scss")) |>
  bslib::bs_add_variables(
    "accordion-bg" = "$body-secondary-bg",
    .where = "declarations"
  )

palette_cat_wide <- tibble::tribble(
  ~category,               ~col,
  "Earnings",              "white",
  "Pension - Mandatory",   "#0077CC",
  "Pension - Voluntary",   "#00A6FB",
  "Insurance - Mandatory", "#FF5733",
  "Insurance - Voluntary", "#FF8C42",
  "Tax",                   "#E63946",
  "Student Loan",          "#9B4DCA",
  "Net Income",            "#F8C630"
)
