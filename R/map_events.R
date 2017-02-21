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
#'
#' @export
find_events <- function(date_range = NULL, event_types = NULL,
                        dist_limit = NULL, storm = NULL,
                        include_narratives = FALSE,
                        include_ids = FALSE, clean_damage = FALSE){

  processed_inputs <- process_input_args(date_range = date_range,
                                         storm = storm)

  storm_data <- create_storm_data(date_range = processed_inputs$date_range,
                                  storm = processed_inputs$storm) %>%
    clean_storm_data(include_narratives = include_narratives) %>%
    adjust_storm_data(date_range = processed_inputs$date_range,
                      event_type = event_type,
                      dist_limit = dist_limit,
                      storm = processed_inputs$storm)

  # If the users chooses, identify any cases where the crop or property damage for a
  # single event is larger than for all others in the state in that year combined and reset
  # those damages to missing
  if(clean_damage){
    storm_data_NONA <- storm_data %>%
      dplyr::group_by_(~ state) %>%
      dplyr::mutate_(state_crop_damage = sum(damage_crops),
                     damage_crops = ~ ifelse((state_crop_damage - damage_crops) < damage_crops,
                                             NA, damage_crops),
                     state_property_damage = sum(damage_property),
                     damage_property = ~ ifelse((state_property_damage - damage_property) < damage_property,
                                                NA, damage_crops)) %>%
      dplyr::ungroup() %>%
      dplyr::select_(~ -state_crop_damage, - state_property_damage)
  }

  if(!include_ids){
    storm_data <- storm_data %>%
      dplyr::select_(~ -event_id, ~ -episode_id)
  }

  return(storm_data)
}

#' Map storm events for a date range
#'
#' This function maps all storm events listed with a starting date within a
#' specified date range.
#'
#' @param east_only A logical value specifying whether to restrict the map to
#'    the eastern half of the United States (default is TRUE).
#' @param add_tracks A logical value specifying whether to add the tracks of
#'    a hurricane to the map (default = FALSE).
#' @param plot_type Specifies the type of plot wanted. It can be either "any
#'    events" or "number of events".
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples \dontrun{
#' map_events(date_range = c("1999-09-10", "1999-09-30"))
#' map_events(date_range = c("1999-09-01", "1999-09-30"),
#'    east_only = FALSE, event_type = c("Flood","Flash Flood"))
#' map_events(date_range = c("1999-09-10", "1999-09-30"),
#'    plot_type = "number of events")
#' map_events(date_range = c("1999-09-10", "1999-09-30"),
#'    dist_limit = 100, storm = "Floyd-1999",
#'    add_tracks = TRUE, plot_type = "number of events")
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
map_events <- function(date_range = NULL, event_type = NULL,
                       east_only = TRUE,
                       plot_type = "any events", dist_limit = NULL,
                       storm = NULL, add_tracks = FALSE){

  utils::data(county.regions, package = "choroplethrMaps")
  eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                      "district of columbia", "florida", "georgia", "illinois",
                      "indiana", "iowa", "kansas", "kentucky", "louisiana",
                      "maine", "maryland", "massachusetts", "michigan",
                      "mississippi", "missouri", "new hampshire", "new jersey",
                      "new york", "north carolina", "ohio", "oklahoma",
                      "pennsylvania", "rhode island", "south carolina",
                      "tennessee", "texas", "vermont", "virginia",
                      "west virginia", "wisconsin")

  map_data <- find_events(date_range = date_range, storm = storm,
                          dist_limit = dist_limit, event_type = event_type) %>%
    dplyr::mutate_(fips = ~ as.numeric(fips)) %>%
    dplyr::rename_(region = ~ fips, value = ~ type) %>%
    dplyr::full_join(county.regions, by = "region") %>%
    dplyr::filter_(~ !is.na(county.name))

  if(east_only){
    map_data <- dplyr::filter_(map_data, ~ state.name %in% eastern_states)
  }

  map_data <- map_data %>% dplyr::select_(~ region, ~ value)

  if(plot_type == "any events"){
    map_data <- map_data %>%
      dplyr::group_by_(~region) %>%
      dplyr::summarize_(value = ~ sum(!is.na(value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_(value = ~ value > 0,
                     value = ~ factor(value, levels = c(TRUE, FALSE),
                     labels = ~ c("Event(s)", "No Event")))
  } else if (plot_type == "number of events"){
    map_data <- map_data %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, 1)) %>%
      dplyr::group_by_(~ region) %>%
      dplyr::summarise_(value = ~ sum(as.numeric(value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_(value = ~ factor(value, levels = 0:max(value)))
  }


  if(plot_type == "any events"){
    out <- choroplethr::CountyChoropleth$new(map_data)
    out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                   values = c("#e6550d", "white"))
  } else if (plot_type == "number of events"){
    if(length(unique(map_data$value)) > 9) {
    map_palette <- RColorBrewer::brewer.pal(9 , name = "Reds")
    map_palette[1] <- "#ffffff"
    boundary <- as.numeric(as.character(sort(unique(map_data$value))[8]))
    map_data$value <- as.numeric(as.character(map_data$value))
    map_data <- map_data %>%
      dplyr::mutate_(value = ~ as.numeric(as.character(value))) %>%
      dplyr::mutate_(value = ~ ifelse(value > boundary, 9999, value)) %>%
      dplyr::mutate_(value = ~ as.factor(value))
      level_names <- levels(map_data$value)
      level_names[9:length(level_names)] <- paste0(">",boundary)
    } else {
      map_palette <- RColorBrewer::brewer.pal(length(unique(map_data$value)),
                                              name = "Reds")
      map_palette[1] <- "#ffffff"
      level_names <- levels(map_data$value)
    }
    map_data$value <- factor(map_data$value,
                             levels = levels(map_data$value),
                             labels = level_names)
    out <- choroplethr::CountyChoropleth$new(map_data)
    out$ggplot_scale <- ggplot2::scale_fill_manual(name = "# of events",
                                                   values = map_palette)
  }

  if(east_only){
    out$set_zoom(eastern_states)
  } else {
    all_states <- out$get_zoom()
    continental_states <- all_states[!(all_states %in% c("alaska", "hawaii"))]
    out$set_zoom(continental_states)
  }

  if(add_tracks){
    tracks_map <- hurricaneexposure::map_tracks(storms = storm,
                                               plot_object = out$render(),
                                               plot_points = FALSE,
                                               color = "black")
    return(tracks_map)
  } else {
    return(out$render())
  }
}


