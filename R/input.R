#' Find the file name for a specific year
#'
#' This function will find the name of the detailed file from Storm Events
#' Database for a specific year.
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
  url <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/",
                "stormevents/csvfiles/")
  page <- htmltab::htmltab(doc = url, which = 1, rm_nodata_cols = FALSE)
  all_file_names <- page$Name
  file_year <- paste0("_d",year,"_")
  file_name <- grep(file_type, grep(file_year, all_file_names, value = TRUE),
                    value = TRUE)
  if(length(file_name) == 0){
    stop("No file found for that year and / or file type.")
  }
  return(file_name)
}

#' Process inputs to main functions
#'
#' Processes some of the user's inputs for arguments for main
#' package functions, looks for any errors in input, and determines
#' elements like the year or years of storm data needed based on
#' user inputs.
#'
#' @return A list with date ranges and storm identification based on
#'    user inputs to arguments in a main package function.
#'
#' @inheritParams create_storm_data
process_input_args <- function(date_range = NULL, storm = NULL){

  if(!is.null(date_range)){
    date_range <- sort(lubridate::ymd(date_range))
    date_range_years <- lubridate::year(date_range)
  }
  if(!is.null(storm)){
    storm_year <- as.numeric(gsub("[^0-9]", "", storm))
    if(nchar(storm_year) != 4){
      stop("`storm` must fall the format `[storm name]-[4-digit storm year]`")
    }
  }
  if(!is.null(date_range) & !is.null(storm)){
    if(storm_year < date_range_years[1] &
       storm_year > date_range_years[2]){
      stop(paste0("If specifying both `date_range` and `storm`, the year of ",
                  "the storm must be within the date range."))
    }
  }

  return(list(date_range = date_range, storm = storm))
}
