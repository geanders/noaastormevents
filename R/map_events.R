#' Map storm events for a date range
#'
#' This function maps all storm events listed with a starting date within a
#' specified date range.
#'
#' @param event_data A dataframe of event data, as returned by the \code{find_events}
#'    function.
#' @param states A character string specifying either a state name or names or one of
#'    "all" (map all states in the continental US) or "east" (plot states in the
#'    Eastern half of the US. The default is "east".
#' @param add_tracks A logical value specifying whether to add the tracks of
#'    a hurricane to the map (default = FALSE).
#' @param plot_type Specifies the type of plot wanted. It can be "any
#'    events", "number of events", "direct deaths", "indirect deaths",
#'    "direct injuries", "indirect injuries", "property damage", or "crop damage".
#' @inheritParams create_storm_data
#'
#' @note Indirect deaths and injuries seem to be reported very rarely, so it is
#'    likely that trying to map either of these outcomes will result in a note that
#'    no indirect deaths / injuries were reported for the selected events.
#'
#' @examples \dontrun{
#' # Map for events pulled by a date range
#' event_data <- find_events(date_range = c("1999-09-10", "1999-09-30"))
#' map_events(event_data)
#' map_events(event_data, plot_type = "number of events")
#'
#' # Map for a specific type of event
#' event_data <- find_events(date_range = c("1999-09-10", "1999-09-30"),
#'                           event_types = c("Flood","Flash Flood"))
#' map_events(event_data, states = "north carolina", plot_type = "number of events")
#' map_events(event_data, states = "all")
#'
#' # Map for events identified based on a hurricane storm track
#' event_data <- find_events(storm = "Floyd-1999", dist_limit = 300)
#' map_events(event_data, plot_type = "number of events",
#'            storm = "Floyd-1999", add_tracks = TRUE)
#' map_events(event_data, plot_type = "crop damage",
#'            storm = "Floyd-1999", add_tracks = TRUE,
#'            states = c("north carolina", "virginia", "maryland"))
#' map_events(event_data, plot_type = "property damage",
#'            storm = "Floyd-1999", add_tracks = TRUE)
#' map_events(event_data, plot_type = "direct deaths")
#'
#' event_data <- find_events(date_range = c("1999-01-01", "1999-12-31"))
#' map_events(event_data, plot_type = "direct deaths")
#' map_events(event_data, plot_type = "indirect deaths")
#' map_events(event_data, plot_type = "direct injuries")
#' map_events(event_data, plot_type = "indirect injuries")
#' map_events(event_data, plot_type = "crop damage")
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
#' @export
map_events <- function(event_data, states = "east", plot_type = "any events",
                       storm = NULL, add_tracks = FALSE){

  if(!is.null(storm)) hasData()

  county_map_data <- get_county_map(states = states)
  county_states <- unique(stringr::str_replace(county_map_data$polyname, "[,].+", ""))

  map_data <- event_data %>%
    dplyr::filter(stringr::str_to_lower(.data$state) %in% county_states)

  if(plot_type == "any events"){
    map_data <- map_data %>%
      dplyr::count(.data$fips) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate(value = factor(.data$n > 0 & !is.na(.data$n), levels = c(TRUE, FALSE),
                                      labels = c("Event(s)", "No Event")))
  } else if (plot_type == "number of events"){
    scale_name <- "Number of events"
    map_data <- map_data %>%
      dplyr::count(.data$fips) %>%
      dplyr::rename(value = .data$n) %>%
      dplyr::right_join(county_map_data, by = "fips")
  } else if (plot_type %in% c("crop damage", "property damage")){
    if(plot_type == "crop damage"){
      map_data <- dplyr::rename(map_data, value = .data$damage_crops)
      scale_name <- "Crop damage"
    } else if(plot_type == "property damage"){
      map_data <- dplyr::rename(map_data, value = .data$damage_property)
      scale_name <- "Property damage"
    }
    if(max(map_data$value) == 0){
      return(paste("No", plot_type,"reported for selected events."))
    }
    map_data <- map_data %>%
      dplyr::group_by(.data$fips) %>%
      dplyr::summarize(value = sum(.data$value)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate(value = ifelse(.data$value == 0, NA, .data$value))
  } else if (plot_type %in% c("direct deaths", "indirect deaths", "direct injuries", "indirect injuries")){
    if(plot_type == "direct deaths"){
      map_data <- dplyr::rename(map_data, value = .data$deaths_direct)
      scale_name <- "Direct deaths"
    } else if(plot_type == "indirect deaths"){
      map_data <- dplyr::rename(map_data, value = .data$deaths_indirect)
      scale_name <- "Indirect deaths"
    } else if(plot_type == "direct injuries"){
      map_data <- dplyr::rename(map_data, value = .data$injuries_direct)
      scale_name <- "Direct injuries"
    } else if(plot_type == "indirect injuries"){
      map_data <- dplyr::rename(map_data, value = .data$injuries_indirect)
      scale_name <- "Indirect injuries"
    }
    if(max(map_data$value) == 0){
      return(paste("No", plot_type,"reported for selected events."))
    }
    map_data <- map_data %>%
      dplyr::group_by(.data$fips) %>%
      dplyr::summarize(value = sum(.data$value, na.rm = TRUE)) %>%
      dplyr::right_join(county_map_data, by = "fips")
  }

  out <- map_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes(x = .data$long, y = .data$lat,
                                       group = .data$group, fill = .data$value),
                          color = "darkgray", size = 0.1) +
    ggplot2::geom_polygon(data = ggplot2::map_data("state", region = county_states),
                          ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
                          fill = NA, color = "black", size = 0.2) +
    ggplot2::theme_void() +
    ggplot2::coord_map()

  if (plot_type == "any events"){
    out <- out +
      ggplot2::scale_fill_manual(name = "", values = c("#e6550d", "white"), drop = FALSE)
  } else if (plot_type %in% c("crop damage", "property damage")){
    out <- out + viridis::scale_fill_viridis(name = scale_name, option = "B",
                                             begin = 0.2, end = 0.9, trans = "log10",
                                             breaks = 10**(1:10),
                                             labels = paste0("$",
                                                             formatC(10**(1:10), format = "f",
                                                              big.mark = ",", digits = 0)),
                                             na.value = "white")
  }  else if (plot_type %in% c("number of events", "direct deaths", "indirect deaths",
                               "direct injuries", "indirect injuries")){
    out <- out + viridis::scale_fill_viridis(name = scale_name,
                                             na.value = "white")
  }

  if(add_tracks){
    hurr_track <- hurricaneexposuredata::hurr_tracks %>%
      dplyr::filter(.data$storm_id == storm)
    if(!(states[1] %in% c("east", "all"))){
      hurr_track <- hurr_track %>%
        dplyr::filter(min(out$data$long) - 2 <= .data$longitude &
                         .data$longitude <= max(out$data$long) + 2) %>%
        dplyr::filter(min(out$data$lat) - 2 <= .data$latitude &
                         .data$latitude <= max(out$data$lat) + 2)
    } else {
      hurr_track <- hurr_track %>%
        dplyr::filter(-106.65037 <= .data$longitude & .data$longitude <= -67.00742) %>%
        dplyr::filter(25.12993 <= .data$latitude & .data$latitude <= 47.48101)
    }
    out <- out +
      ggplot2::geom_path(data = hurr_track,
                         ggplot2::aes(x = .data$longitude, y = .data$latitude),
                         color = "red", alpha = 0.9)
  }

  return(out)
}

#' Get map data for counties
#'
#' @inheritParams map_events
#'
#' @return A dataframe with map data pulled using the \code{map_data} function in
#'    \code{ggplot2}, filtered to states in the eastern half of the United States
#'    if the user specifies \code{east_only}.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#'
get_county_map <- function(states = "east"){

  states <- stringr::str_to_lower(states)

  if(states[1] == "east"){
    states <- c("alabama", "arkansas", "connecticut", "delaware",
                        "district of columbia", "florida", "georgia", "illinois",
                        "indiana", "iowa", "kansas", "kentucky", "louisiana",
                        "maine", "maryland", "massachusetts", "michigan",
                        "mississippi", "missouri", "new hampshire", "new jersey",
                        "new york", "north carolina", "ohio", "oklahoma",
                        "pennsylvania", "rhode island", "south carolina",
                        "tennessee", "texas", "vermont", "virginia",
                        "west virginia", "wisconsin")
  } else if (states[1] == "all"){
    states <- stringr::str_to_lower(datasets::state.name)
    states <- states[!(states %in% c("alaska", "hawaii"))]
  }

  map_data <- ggplot2::map_data(map = "county") %>%
    dplyr::filter(.data$region %in% states)

  county.fips <- maps::county.fips %>%
    dplyr::mutate(polyname = as.character(.data$polyname)) %>%
    dplyr::mutate(polyname = stringr::str_replace(.data$polyname,
                                                       ":.+", ""))
  map_data <- map_data %>%
    tidyr::unite(col = "polyname", from = c("region", "subregion"),
                  sep = ",") %>%
    dplyr::left_join(county.fips, by = "polyname")

  return(map_data)
}

