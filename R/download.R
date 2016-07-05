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
download_file <- function(year, file_type = "detail"){
  file_name <- find_file_name(year = year, file_type = file_type)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/",
                      "csvfiles/",file_name)

  if(!exists("lst")) {
    temp <- tempfile()
    download.file(path_name, temp)
    lst <<- list()
    lst[[as.character(Year)]] <<-  suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
    unlink(temp)
  } else if(is.null(lst[[as.character(Year)]])) {
    temp <- tempfile()
    download.file(path_name, temp)
    lst[[as.character(Year)]] <<-  suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
    unlink(temp)
  }

}

get_file <- function(first_date = NULL, last_date = NULL, storm = NULL) {

  if(!is.null(first_date) & !is.null(last_date)){
    first_date <- lubridate::ymd(first_date)
    last_date <- lubridate::ymd(last_date)
    if(last_date < first_date){
      stop(paste0("The `last_date` must be after the `first_date`."))
    }
  }

  if(!is.null(storm)){
    storm_year <- gsub("[^0-9]", "", storm)
    download_file(Year = storm_year)
    data <- lst[[as.character(Year)]]
  } else {
    if(lubridate::year(first_date) == lubridate::year(last_date)) {
    Year <- lubridate::year(first_date)
    download_file(Year = Year)
    data <- lst[[as.character(Year)]]
    } else {
      data <- NULL
      for (Year in
           as.numeric(as.character(lubridate::year(first_date))):
           as.numeric(as.character(lubridate::year(last_date)))) {
      download_file(Year = Year)
      data <- rbind(data, lst[[as.character(Year)]])
      }
    }
  }
  return(data)
}
