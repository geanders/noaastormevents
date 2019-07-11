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
download_storm_data <- function(year, file_type = "details"){
  file_name <- find_file_name(year = year, file_type = file_type)
  path_name <- paste0("https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/",
                      "csvfiles/",file_name)

  if(!exists("noaastormevents_package_env")) {
    temp <- tempfile()
    utils::download.file(path_name, temp)
    noaastormevents_package_env <<- new.env()
    noaastormevents_package_env$lst <- list()

    noaastormevents_package_env$lst[[as.character(year)]] <-
    suppressWarnings(utils::read.csv(gzfile(temp), as.is = TRUE))
    unlink(temp)
  } else if(is.null(noaastormevents_package_env$lst[[as.character(year)]])) {
    temp <- tempfile()
    utils::download.file(path_name, temp)
    noaastormevents_package_env$lst[[as.character(year)]] <-
    suppressWarnings(utils::read.csv(gzfile(temp), as.is = TRUE))
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
#'    extended hurricane best tracks, which covers 1988 to 2015.
#' @inheritParams find_file_name
#'
#' @examples \dontrun{
#' floyd_data <- create_storm_data(date_range = c("1999-10-16", "1999-10-18"))
#' floyd_data2 <- create_storm_data(storm = "Floyd-1999")
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
create_storm_data <- function(date_range = NULL, storm = NULL,
                              file_type = "details") {

  # If the user has included a date range, pull all data within that date range
  if(!is.null(date_range)){
    date_range_years <- lubridate::year(date_range)
    requested_years <- seq(from = date_range_years[1], to = date_range_years[2])
    lapply(requested_years, download_storm_data)
    for(i in 1:length(requested_years)){
      requested_year <- as.character(requested_years[i])
      if(i == 1){
        storm_data <- noaastormevents_package_env$lst[[requested_year]]
      } else {
        storm_data <- rbind(storm_data,
                            noaastormevents_package_env$lst[[requested_year]])
      }
    }
  } else if (!is.null(storm)){ ## Otherwise, pull for the year of the storm
    storm_year <- stringr::str_extract(storm, "\\-[0-9].+") %>%
      stringr::str_remove("\\-")
    download_storm_data(year = storm_year, file_type = file_type)
    storm_data <- noaastormevents_package_env$lst[[as.character(storm_year)]]
  } else {
    stop("You must specify either `date_range` or `storm`.")
  }

  storm_data <- dplyr::tbl_df(storm_data)

  return(storm_data)
}
