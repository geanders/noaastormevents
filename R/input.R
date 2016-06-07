#' Find the file name for a specific year
#'
#' This function will find the name of the detailed file from Storm Events Database
#' for a specific year.
#'
#' @param year A character string giving the year.
#'
#'
#' @examples
#' find_file_name(year = 1999)
#'
#' find_file_name(year = 2003)
#'
#' @export
#'
find_file_name <- function(year = NULL, file_type = "detail") {
  page <-  XML::readHTMLTable(paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/",
                                     "stormevents/csvfiles/"))
  file_name <- page[[1]]$Name
  file_year <- paste0("_d",year,"_")
  file.name <- grep(file_type, grep(file_year, file_name, value = TRUE),
                    value = TRUE)
  return(file.name)
}
