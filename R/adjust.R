#' Adjust storm data
#'
#' This function adjusts a set of storm data based on user selections on
#' date range, distance limit to a storm, etc.
#'
#' @param storm_data A dataset of storm data
#' @param ts_only Specifies if only storm events related to tropical storms
#'    should be included.
#' @param dist_limit If selected, the distance (in kilometers) that a county
#'    must be from the storm's path to be included.
#' @inheritParams create_storm_data
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
adjust_storm_data <- function(storm_data, date_range = NULL,
                        ts_only = FALSE, dist_limit = NULL, storm = NULL){

  # A bit more general cleaning of the data
  storm_data <- storm_data %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(BEGIN_DAY = sprintf("%02d", BEGIN_DAY),
                  END_DAY = sprintf("%02d", END_DAY),
                  CZ_FIPS = sprintf("%03d", CZ_FIPS)) %>%
    tidyr::unite_("begin_date", c("BEGIN_YEARMONTH", "BEGIN_DAY"), sep = "") %>%
    tidyr::unite_("end_date", c("END_YEARMONTH", "END_DAY"), sep = "") %>%
    tidyr::unite_("fips", c("STATE_FIPS", "CZ_FIPS"), sep = "") %>%
    dplyr::mutate(begin_date = lubridate::ymd(begin_date),
                  end_date = lubridate::ymd(end_date))

  # If a date range in include, filter only on that for date
  if(!is.null(date_range)){
    storm_data <- storm_data %>%
      dplyr::filter(!is.na(begin_date) &
                      begin_date %within% lubridate::interval(date_range[1],
                                                              date_range[2]))
  } else { ## Otherwise, use the storm dates from "closest_dates" to pick dates
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm)
   storm_closest_interval <- lubridate::interval(min(distance_df$closest_date),
                                                 max(distance_df$closest_date))
   storm_date <- storm_data %>%
     dplyr::filter(!is.na(begin_date) &
                     begin_date %within% storm_closest_interval)
  }

  # If a distance limit is specified, filter by that
  if(!is.null(dist_limit)){
    if(is.null(storm)){
      stop("To use `dist_limit`, `storm` must be specified.")
    }
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm & storm_dist <= dist_limit)
    storm_data <- storm_data %>%
      dplyr::filter_(~ fips %in% distance_df$fips)
  }

  # Limit to event types linked to tropical storms if requested
  if(ts_only) {
    ts_types <- c("Coastal Flood","Flash Flood" ,"Flood","Heavy Rain",
                  "High Surf","High Wind Hurricane (Typhoon)",
                  "Storm Surge/Tide","Strong Wind","Thunderstorm Wind",
                  "Tornado","Tropical Storm","Waterspout")
    storm_data <- storm_data %>%
      dplyr::filter_(~ type %in% ts_types)
  }

  return(storm_data)
}
