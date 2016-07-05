#' Find the file name for a specific year
#'
#' This function will find the name of the detailed file from Storm Events Database
#' for a specific year.
#'
#' @param year A character string giving the year.
#' @param file_type The type of file you would like to pull. Choices include:
#'    "details" (the default), "fatalities", or "locations".
#'
#' @examples
#' find_file_name(year = 1999)
#' find_file_name(year = 2003, file_type = "fatalities")
#'
#' @export
#'
find_file_name <- function(year = NULL, file_type = "detail") {
  page <-  XML::readHTMLTable(paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/",
                                     "stormevents/csvfiles/"))
  all_file_names <- page[[1]]$Name
  file_year <- paste0("_d",year,"_")
  file_name <- grep(file_type, grep(file_year, all_file_names, value = TRUE),
                    value = TRUE)
  if(length(file_name) == 0){
    stop("No file found for that year and / or file type.")
  }
  return(file_name)
}
