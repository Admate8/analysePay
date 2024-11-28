palette_global <- list(
  "body_bg"              = "#1A1A1A",
  "body_secondary_bg"    = "#2A2A2A",
  "body_tertiary_bg"     = "#3A3A3A",

  "body_color"           = "#E0E0E0",
  "body_color_secondary" = "#B0B0B0",
  #"primary"           = "#45B7D1"

  "categories"           = list(
    "earnings_color"      = "#E0E0E0",
    "pension_color"       = "#0077CC",
    "pension_color_vol"   = "#00A6FB",
    "insurance_color"     = "#FF5733",
    "insurance_color_vol" = "#FF8C42",
    "tax_color"           = "#E63946",
    "sl_plan2_color"      = "#9B4DCA",
    "sl_plan3_color"      = "#C774CF",
    "net_color"           = "#2D936C"
  )
)

app_theme <- bslib::bs_theme(
  "body-bg"                     = palette_global$body_bg,
  "body-secondary-bg"           = palette_global$body_secondary_bg,
  "body-tertiary-bg"            = palette_global$body_tertiary_bg,
  "body-color"                  = palette_global$body_color,
  "body-color-secondary"        = palette_global$body_color_secondary,
  "primary"                     = palette_global$body_tertiary_bg,
  "accordion-icon-active-color" = palette_global$body_color,
  "btn-close-color"             = palette_global$body_color,
  "link-color"                  = palette_global$body_color_secondary,
  "accordion-border-radius"     = "25px",
  "tooltip-border-radius"       = "25px",
  "tooltip-color"               = palette_global$body_color,
  "popover-bg"                  = palette_global$body_secondary_bg
) |>
  bslib::bs_add_rules(sass::sass_file("inst/app/www/custom_themes.scss")) |>
  bslib::bs_add_variables(
    "accordion-bg"  = "$body-secondary-bg",
    # Categories
    "pension-color"       = palette_global$categories$pension_color,
    "pension-color-vol"   = palette_global$categories$pension_color_vol,
    "insurance-color"     = palette_global$categories$insurance_color,
    "insurance-color-vol" = palette_global$categories$insurance_color_vol,
    "tax-color"           = palette_global$categories$tax_color,
    "sl-plan2-color"      = palette_global$categories$sl_plan2_color,
    "sl-plan3-color"      = palette_global$categories$sl_plan3_color,
    .where = "declarations"
  )
