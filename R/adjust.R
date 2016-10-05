#' Adjust storm data
#'
#' This function adjusts a set of storm data based on user selections on
#' date range, distance limit to a storm, etc.
#'
#' @param storm_data A dataset of storm data.
#' @param event_type Specifies the types of storm events should be included.
#' @param dist_limit If selected, the distance (in kilometers) that a county
#'    must be from the storm's path to be included.
#' @inheritParams create_storm_data
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
adjust_storm_data <- function(storm_data, date_range = NULL,
                              event_type = NULL, dist_limit = NULL, storm = NULL) {

  data(county.regions, package = "choroplethrMaps")
  county.regions <- county.regions %>%
    tidyr::unite_("state_county_name", c("state.name", "county.name"), sep = " ")

  # A bit more general cleaning of the data
  storm_data_Z <- storm_data %>%
    dplyr::tbl_df() %>%
    dplyr::filter_(~ CZ_TYPE == "Z") %>%
    tidyr::unite_("state_county_name", c("STATE", "CZ_NAME"), sep = " ") %>%
    dplyr::mutate_(state_county_name = ~ tolower(state_county_name),
                   state_county_name = ~ gsub(" eastern ", " ",state_county_name),
                   state_county_name = ~ gsub(" southern ", " ",state_county_name),
                   state_county_name = ~ gsub(" western ", " ",state_county_name),
                   state_county_name = ~ gsub(" northern ", " ",state_county_name),
                   state_county_name = ~ gsub(" east ", " ",state_county_name),
                   state_county_name = ~ gsub(" south ", " ",state_county_name),
                   state_county_name = ~ gsub(" west ", " ",state_county_name),
                   state_county_name = ~ gsub(" north ", " ",state_county_name),
                   state_county_name = ~ gsub(" southeast ", " ",state_county_name),
                   state_county_name = ~ gsub(" northeast ", " ",state_county_name),
                   state_county_name = ~ gsub(" southwest ", " ",state_county_name),
                   state_county_name = ~ gsub(" northwest ", " ",state_county_name),
                   state_county_name = ~ gsub(" coastal ", " ",state_county_name),
                   state_county_name = ~ gsub(" lower ", " ",state_county_name),
                   state_county_name = ~ gsub(" upper ", " ",state_county_name),
                   state_county_name = ~ gsub(" central ", " ",state_county_name),
                   state_county_name = ~ gsub(" interior ", " ",state_county_name),
                   state_county_name = ~ gsub(" /.*$", " ", state_county_name)) %>%
    dplyr::left_join(county.regions, by = "state_county_name") %>%
    dplyr::select_(~ -region, ~ -state.fips.character) %>%
    dplyr::rename_(fips = ~ county.fips.character)

  storm_data <- storm_data %>%
    dplyr::tbl_df() %>%
    dplyr::filter_(~ CZ_TYPE == "C") %>%
    tidyr::unite_("state_county_name", c("STATE", "CZ_NAME"), sep = " ") %>%
    dplyr::mutate_(state_county_name = ~ tolower(state_county_name)) %>%
    dplyr::mutate_(CZ_FIPS = ~ sprintf("%03d", CZ_FIPS)) %>%
    dplyr::mutate_(STATE_FIPS = ~ sprintf("%02d", STATE_FIPS)) %>%
    tidyr::unite_("fips", c("STATE_FIPS","CZ_FIPS"), sep = "") %>%
    dplyr::full_join(storm_data_Z) %>%
    dplyr::mutate_(BEGIN_DAY =  ~ sprintf("%02d", BEGIN_DAY),
                   END_DAY =  ~ sprintf("%02d", END_DAY)) %>%
    tidyr::unite_("begin_date", c("BEGIN_YEARMONTH", "BEGIN_DAY"), sep = "") %>%
    tidyr::unite_("end_date", c("END_YEARMONTH", "END_DAY"), sep = "") %>%
    dplyr::mutate_(begin_date = ~ lubridate::ymd(begin_date),
                   end_date = ~ lubridate::ymd(end_date))%>%
    dplyr::select_(~ -STATE_FIPS, ~ -CZ_FIPS, ~ -state.abb) %>%
    dplyr::filter_(~ !is.na(fips))

  # If a date range in include, filter only on that for date
  if(!is.null(date_range)){
    storm_data <- storm_data %>%
      dplyr::filter_(~ !is.na(begin_date) &
                      begin_date %within% lubridate::interval(date_range[1],
                                                              date_range[2]))
  } else { ## Otherwise, use the storm dates from "closest_dates" to pick dates
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm) %>%
      dplyr::mutate_(closest_date = ~ lubridate::ymd(closest_date))
   storm_closest_interval <- lubridate::interval(min(distance_df$closest_date) -
                                                   lubridate::ddays(2),
                                                 max(distance_df$closest_date) +
                                                   lubridate::ddays(2))
   storm_data <- storm_data %>%
     dplyr::filter_(~ !is.na(begin_date) &
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

  # If the event_type is specified, filter by that
  if(!is.null(event_type)) {
    storm_data <- suppressWarnings(dplyr::filter_(storm_data, ~ tolower(type) == tolower(event_type)))
  }


  return(storm_data)
}
