#' Find all event listings for date range
#'
#' This function will find all of the events in the US for a specified date
#' range.
#'
#' @param include_narratives A logical value for whether the final data
#'    data frame should include columns for episode and event narratives
#'    (TRUE) or not (FALSE, the default)
#' @param include_ids A logical value for whether the final data frame
#'    should include columns for event and episode IDs (TRUE) or not
#'    (FALSE, the default). If included, these IDs could be used in some
#'    cases to link events to data in the "fatalities" or "locations"
#'    files available through the NOAA Storm Events database.
#' @param clean_damage TRUE / FALSE of whether additional cleaning should be
#'    done to try to exclude incorrect damage listings. If TRUE, any property
#'    or crop damages for which the listing for that single event exceeds all
#'    other damages in the state combined for the event dataset, the damages for
#'    that event listing will be set to missing. Default is FALSE (i.e., this
#'    additional check is not performed). In some cases, it seems that a single
#'    listing by forecast zone gives the state total for damages, and this option
#'    may help in identifying and excluding such listings (for example, one listing in
#'    North Carolina for Hurricane Floyd seems to be the state total for damages, rather
#'    than a county-specific damage estimate).
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples \dontrun{
#' # Events by date range
#' find_events(date_range = c("1999-09-10", "1999-09-30"))
#'
#' # Events within a certain distance and time range of a tropical storm
#' find_events(storm = "Floyd-1999", dist_limit = 200)
#'
#' # Limit output to events that are floods or flash floods
#' find_events(storm = "Floyd-1999", dist_limit = 200, event_types = c("Flood", "Flash Flood"))
#'}
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#' @importFrom rlang .data
#'
#' @export
find_events <- function(date_range = NULL, event_types = NULL,
                        dist_limit = NULL, storm = NULL,
                        include_narratives = FALSE,
                        include_ids = FALSE, clean_damage = FALSE){

  if(!is.null(storm)) hasData()

  processed_inputs <- process_input_args(date_range = date_range,
                                         storm = storm)

  storm_data <- create_storm_data(date_range = processed_inputs$date_range,
                                  storm = processed_inputs$storm) %>%
    clean_storm_data(include_narratives = include_narratives) %>%
    adjust_storm_data(date_range = processed_inputs$date_range,
                      event_types = event_types,
                      dist_limit = dist_limit,
                      storm = processed_inputs$storm)

  # If the users chooses, identify any cases where the crop or property damage for a
  # single event is larger than for all others in the state in that year combined and reset
  # those damages to missing
  if(clean_damage){
    storm_data <- storm_data %>%
      dplyr::group_by(.data$state) %>%
      dplyr::mutate(state_crop_damage = sum(.data$damage_crops),
                     damage_crops = ifelse((.data$state_crop_damage -
                                              .data$damage_crops) < .data$damage_crops,
                                             NA, .data$damage_crops),
                     state_property_damage = sum(.data$damage_property),
                     damage_property = ifelse((.data$state_property_damage -
                                                 .data$damage_property) < .data$damage_property,
                                                NA, .data$damage_crops)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-.data$state_crop_damage, -.data$state_property_damage)
  }

  if(!include_ids){
    storm_data <- storm_data %>%
      dplyr::select(-.data$event_id, -.data$episode_id)
  }

  return(storm_data)
}
