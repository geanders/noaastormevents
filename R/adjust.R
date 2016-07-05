adjust_file <- function(date_range = c(NULL, NULL), ts_only = NULL,
                        dist_limit = NULL, storm = NULL, data = NULL){
  first_date <- lubridate::ymd(date_range[1])
  last_date <- lubridate::ymd(date_range[2])

  if(!is.null(dist_limit) & !is.null(storm)) {
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm & storm_dist <= dist_limit)
  } else if(is.null(dist_limit) & !is.null(storm)){
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm)
  } else if(!is.null(dist_limit) & is.null(storm)){
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_dist <= dist_limit)
  } else {
    distance_df <- hurricaneexposuredata::closest_dist
  }


if(!is.null(first_date) & !is.null(last_date)){
  if(!is.null(storm)){
    storm_first_date <- lubridate::ymd(min(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
    storm_last_date <-  lubridate::ymd(max(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
    storm_interval <- interval(storm_first_date, storm_last_date)
    if(!(first_date %within% storm_interval) & last_date %within% (storm_interval)){
      first_date <- storm_first_date
    } else if((first_date %within% storm_interval) & !(last_date %within% storm_interval)) {
      last_date <- storm_last_date
    } else if(!(first_date %within% storm_interval) & !(last_date %within% storm_interval)) {
      first_date <- storm_first_date
      last_date <- storm_last_date
    }
    data <- data %>%
      dplyr::mutate(begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                    end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
      dplyr::filter(!is.na(begin_date) &
                      lubridate::ymd(begin_date) %within% interval(first_date,last_date)) %>%
      dplyr::left_join(distance_df, by = "fips") %>%
      dplyr::filter_(~ !is.na(storm_dist))
      } else {
    data <- data %>%
      dplyr::mutate(begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                    end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
      dplyr::filter(!is.na(begin_date) &
                      lubridate::ymd(begin_date) %within% interval(first_date,last_date))
     }
  } else {
  first_date <- lubridate::ymd(min(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
  last_date <-  lubridate::ymd(max(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
  data <- dplyr::mutate(data, begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                              end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
    dplyr::filter(!is.na(begin_date) &
                    lubridate::ymd(begin_date) %within% lubridate::interval(first_date,last_date)) %>%
    dplyr::left_join(distance_df, by = "fips") %>%
    dplyr::filter_(~ !is.na(storm_dist))
  }

  if (ts_only) {
    ts_types <- c("Coastal Flood","Flash Flood" ,"Flood","Heavy Rain",
                  "High Surf","High Wind Hurricane (Typhoon)",
                  "Storm Surge/Tide","Strong Wind","Thunderstorm Wind",
                  "Tornado","Tropical Storm","Waterspout")
    data <- dplyr::filter(data, type %in% ts_types)
  }
  return(data)
}
