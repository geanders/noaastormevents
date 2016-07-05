#' Download storm data file for a given year
#'
#' This function takes a year for which you want to download storm data,
#' checks to see if it's already been downloaded and cached, and, if not,
#' downloads and caches it from the NOAA's online storm events files.
#'
#' @inheritParams find_file_name
#'
#' @note
#' This function caches downloaded storm data into an object called \code{lst}
#' that persist throughout the R session but is deleted at the end of the R
#' session (as long as the R history is not saved at the end of the session).
#' This saves time if the user uses the storm data from the same year for
#' several commands.
download_storm_data <- function(year, file_type = "detail"){
  file_name <- find_file_name(year = year, file_type = file_type)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/",
                      "csvfiles/",file_name)

  if(!exists("lst")) {
    temp <- tempfile()
    download.file(path_name, temp)
    lst <<- list()
    lst[[as.character(year)]] <<-  suppressWarnings(read.csv(gzfile(temp),
                                                             as.is = TRUE))
    unlink(temp)
  } else if(is.null(lst[[as.character(year)]])) {
    temp <- tempfile()
    download.file(path_name, temp)
    lst[[as.character(year)]] <<-  suppressWarnings(read.csv(gzfile(temp),
                                                             as.is = TRUE))
    unlink(temp)
  }
  return(NULL)
}

#' Get storm data based on date range or storm name
#'
#' This function pulls storm events data based on a specified date range and /
#' or storm name. (Note: This function pulls full years' worth of data. Later
#' functions filter down to the exact date range desired.)
#'
#' @param date_range A character vector of length two with the start and end
#'    dates to pull data for (e.g., \code{c("1999-10-16", "1999-10-18")}).
#' @param storm A character string with the name of the storm to pull storm
#'    events data for. This string must follow the format
#'    "[storm-name]-[4-digit storm year]" (e.g., \code{"Floyd-1999"}).
#'    Currently, this functionality only works for storms included in the
#'    extended hurricane best tracks, which covers 1988 to 2014.
#' @inheritParams find_file_name
#'
#' @examples
#' floyd_data <- create_storm_data(date_range = c("1999-10-16", "1999-10-18"))
#' floyd_data2 <- create_storm_data(storm = "Floyd-1999")
#'
#' @export
create_storm_data <- function(date_range = NULL, storm = NULL,
                              file_type = "details") {

  # Process input arguments and check for input errors
  if(!is.null(date_range)){
    date_range <- lubridate::ymd(date_range)
    date_range_years <- lubridate::year(date_range)
    if(date_range[2] < date_range[1]){
      stop(paste0("The second date in `date_range` must",
                  " be after the first date."))
    }
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

  # If the user has included a date range, pull all data within that date range
  if(!is.null(date_range)){
    requested_years <- seq(from = date_range_years[1], to = date_range_years[2])
    for(i in 1:length(requested_years)){
      download_storm_data(year = requested_years[i], file_type = file_type)
      if(i == 1){
        storm_data <- lst[[as.character(requested_years[i])]]
      } else {
        storm_data <- rbind(storm_data,
                            lst[[as.character(requested_years[i])]])
      }
    }
  } else if (!is.null(storm)){ ## Otherwise, pull for the year of the storm
    download_storm_data(year = storm_year, file_type = file_type)
    storm_data <- lst[[as.character(year)]]
  } else {
    stop("You must specify either `date_range` or `storm`.")
  }

  return(storm_data)
}
