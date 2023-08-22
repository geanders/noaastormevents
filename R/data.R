#' FIPS Codes for Forecast Zones within United States
#'
#' A dataframe of county fips codes associated with forecast zones within the Unites States.
#'
#'
#' @format ## `forecast_zone_date`
#' A data frame with 4,762 rows and 4 columns:
#' \describe{
#'   \item{STATE}{Character string of Abbreviated State Name County is Located in}
#'   \item{NAME}{Character string with forecast zone name}
#'   \item{COUNTY}{Character string with County name}
#'   \item{FIPS}{Integer object with 5 Number FIPs Code for County}
#' }
#'
#'@examples
#' data("forecast_zone_data")
#' head(forecast_zone_data)
#'
"forecast_zone_data"
