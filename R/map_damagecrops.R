#' Map crops damaged for a date range
#'
#' This function maps all crops damaged listed with a starting date within a
#' specified date range.
#'
#' @inheritParams map_events
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples \dontrun{
#' map_damage_crops(date_range = c("1999-09-10", "1999-09-30"))
#' map_damage_crops(date_range = c("1999-09-01", "1999-09-30"),
#'    east_only = FALSE, event_type = c("Flood","Flash Flood"))
#' map_damage_crops(date_range = c("1999-09-10", "1999-09-30"))
#' map_damage_crops(date_range = c("1999-09-10", "1999-09-30"),
#'    dist_limit = 100, storm = "Floyd-1999", add_tracks = TRUE)
#' map_damage_crops(storm = "Floyd-1999", add_tracks = TRUE)
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
map_damage_crops <- function(date_range = NULL, event_type = NULL,
                             east_only = TRUE, dist_limit = NULL, storm = NULL,
                             add_tracks = FALSE){

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

  map_data <- find_events(date_range = date_range, storm = storm, dist_limit = dist_limit,
                          event_type = event_type) %>%
    dplyr::mutate_(fips = ~ as.numeric(fips)) %>%
    dplyr::rename_(region = ~ fips, value = ~ damage_crops) %>%
    dplyr::full_join(county.regions, by = "region") %>%
    dplyr::filter_(~ !is.na(county.name))

  if(east_only){
    map_data <- dplyr::filter_(map_data, ~ state.name %in% eastern_states)
  }

  map_data <- map_data %>% dplyr::select_(~ region, ~ value)
  map_data$value <- as.numeric(as.character(map_data$value))

  map_data <- map_data %>% dplyr::group_by_(~ region)
  map_data <- dplyr::summarise_(map_data, value = ~ sum(value, na.rm = TRUE))
  map_data <-  dplyr::ungroup(map_data)

  map_data$value <- ifelse(is.na(map_data$value), 0, map_data$value)

  breaks <- c(0, 1, 1000, 10000, 100000, 1000000, 10000000, 999999999,
              1000000000)
  palette_name <- "Reds"
  map_palette <- RColorBrewer::brewer.pal(length(breaks),
                                          name = palette_name)

  if(max(map_data$value) > max(breaks)){
    breaks <- c(breaks, max(map_data$value))
  }

  map_palette[1] <- "#ffffff"
  map_data <- map_data %>%
    dplyr::mutate_(value = ~ cut(value, breaks = breaks,
                                 include.lowest = TRUE, right = F))
  level_names <- levels(map_data$value)
  level_names[length(level_names)] <- paste0(">=", 1000000000)
  map_data$value <- factor(map_data$value, levels = levels(map_data$value),
                           labels = level_names)
  out <- choroplethr::CountyChoropleth$new(map_data)
  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "# of Crops damaged",
                                                 values = map_palette)

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
