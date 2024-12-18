#' Default UK Settings
#' @format Nested named list with policy attributes / economic data.
#' \describe{
#'   \item{pension}{Variables related to the pension contribution.}
#'   \item{national_insurance}{Variables related to the national insurance contribution.}
#'   \item{tax}{Variables related to the tax contribution}
#'   \item{sl_plan2}{Variables related to the Student Loan Plan 2 contribution.}
#'   \item{sl_plan3}{Variables related to the Student Loan Plan 3 (postgraduate loan) contribution.}
#'   \item{earning_deciles}{Variables related to the national earning deciles.}
#' }
"uk_settings"


#' Default PL Settings
#' @format Nested named list with policy attributes / economic data.
#' \describe{
#'   \item{pension}{Variables related to the pension contribution.}
#'   \item{national_insurance}{Variables related to the national insurance contribution.}
#'   \item{tax}{Variables related to the tax contribution}
#'   \item{sl_plan2}{Variables related to the Student Loan Plan 2 contribution.}
#'   \item{sl_plan3}{Variables related to the Student Loan Plan 3 (postgraduate loan) contribution.}
#'   \item{earning_deciles}{Variables related to the national earning deciles.}
#' }
"pl_settings"


#' UK Expenditure Data
#' @format Data frame. See \code{data-raw/prep_uk_df_expend.R} for detailed derivations.
#' \describe{
#'   \item{expenditure}{Expenditure category.}
#'   \item{percentile}{Percentile. Integer from 0 to 100.}
#'   \item{actual_values}{Actual published percentile values.}
#'   \item{interpolated_values}{Values interpolated using the spline method.}
#'   \item{avg}{Published average expenditure per adult.}
#'   \item{scaling_factor}{Derived column indicating the proportion expenditure change in respect to the average.
#'   Used to scale up expenditure in countries which do not publish detailed values.}
#' }
"uk_df_expend"


#' PL Expenditure Data
#' @format Data frame. See \code{data-raw/prep_pl_df_expend.R} for detailed derivations.
#' \describe{
#'   \item{expenditure}{Expenditure category.}
#'   \item{percentile}{Percentile. Integer from 0 to 100.}
#'   \item{actual_values}{Actual published percentile values.}
#'   \item{interpolated_values}{Values interpolated using the spline method.}
#'   \item{avg}{Published average expenditure per adult.}
#' }
"pl_df_expend"
