download_file <- function(Year = NULL){
  file_name <- find_file_name(Year)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/",file_name)

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

get_file <- function(date_range = c(NULL, NULL), storm = NULL) {
  first_date <-  date_range[1]
  last_date <- date_range[2]
  if(!is.null(first_date) & !is.null(last_date)){
    first_date <- lubridate::ymd(first_date)
    last_date <- lubridate::ymd(last_date)
    if(last_date < first_date){
      stop(paste0("The `last_date` must be after the `first_date`."))
    }
  }


  if(!is.null(storm)){
    Year <- gsub("[^0-9]", "", storm)
    download_file(Year = Year)
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
