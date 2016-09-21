#' Find all damaged crop listings for date range
#'
#' This function will find all of the crops damaged in the US for a specified date
#' range.
#'
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples
#' find_damage_crops(date_range = c("1999-10-15", "1999-10-20"))
#'
#' find_damage_crops(date_range = c("1999-10-16", "1999-10-18"),
#'    storm = "Floyd-1999", dist_limit = 200)
#'
#' @importFrom dplyr %>%
#'
#' @export
find_damage_crops <- function(date_range = NULL, ts_only = FALSE,
                        dist_limit = NULL, storm = NULL){

  processed_inputs <- process_input_args(date_range = date_range, storm = storm)
  date_range <- processed_inputs$date_range
  storm <- processed_inputs$storm

  storm_data <- create_storm_data(date_range = date_range,  storm = storm) %>%
    dplyr::select_(~ BEGIN_YEARMONTH, ~ BEGIN_DAY, ~ END_YEARMONTH, ~ END_DAY, ~ STATE,
                  ~ CZ_NAME, ~ EVENT_TYPE, ~ DAMAGE_CROPS) %>%
    dplyr::rename_(type = ~ EVENT_TYPE,
                  damage_crops = ~ DAMAGE_CROPS) %>%
    adjust_storm_data(date_range = date_range, ts_only = ts_only,
                      dist_limit = dist_limit, storm = storm)

  # Convert crop values (e.g., from "5K" to 5000)
  value_table <- data.frame(letter_crops = c(NA, "K", "M", "B"),
                            value_crops = 10 ^ c(0, 2, 6, 9),
                            stringsAsFactors = FALSE)

  storm_data <- storm_data %>%
    dplyr::mutate_(num_crops = ~ stringr::str_extract(damage_crops, "[0-9]+"),
                  num_crops = ~ as.numeric(num_crops),
                  letter_crops = ~ stringr::str_extract(damage_crops, "[A-Z]+")) %>%
    dplyr::left_join(value_table, by = "letter_crops") %>%
    dplyr::mutate_(damage_crops = ~ num_crops * value_crops) %>%
    dplyr::select_(~ -num_crops, ~ -letter_crops, ~ -value_crops)

  return(storm_data)
}

#' Map crops damaged for a date range
#'
#' This function maps all crops damaged listed with a starting date within a
#' specified date range.
#'
#' @inheritParams map_events
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples
#' map_damage_crops(date_range = c("1999-10-15", "1999-10-20"))
#' map_damage_crops(date_range = c("1999-10-15", "1999-10-20"),
#'    east_only = FALSE, ts_only = TRUE)
#' map_damage_crops(date_range = c("1999-10-15", "1999-10-20"))
#' map_damage_crops(date_range = c("1999-10-15", "1999-10-20"),
#'    dist_limit = 100, storm = "Floyd-1999", add_tracks = TRUE)
#' map_damage_crops(storm = "Floyd-1999", add_tracks = TRUE)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_damage_crops <- function(date_range = NULL, ts_only = FALSE, east_only = TRUE,
                                dist_limit = NULL, storm = NULL, add_tracks = FALSE){

  data(county.regions, package = "choroplethrMaps")
  eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                      "district of columbia", "florida", "georgia", "illinois",
                      "indiana", "iowa", "kansas", "kentucky", "louisiana",
                      "maine", "maryland", "massachusetts", "michigan",
                      "mississippi", "missouri", "new hampshire", "new jersey",
                      "new york", "north carolina", "ohio", "oklahoma",
                      "pennsylvania", "rhode island", "south carolina",
                      "tennessee", "texas", "vermont", "virginia",
                      "west virginia", "wisconsin")

  map_data <- find_damage_crops(date_range = date_range,
                                   storm = storm, dist_limit = dist_limit,
                                   ts_only = ts_only) %>%
    dplyr::mutate_(fips = ~ as.numeric(fips)) %>%
    dplyr::rename_(region = ~ fips, value = damage_crops) %>%
    dplyr::full_join(county.regions, by = "region") %>%
    dplyr::filter_(~ !is.na(county.name))

  if(east_only){
    map_data <- dplyr::filter_(map_data, ~ state.name %in% eastern_states)
  }

  map_data <- map_data %>% dplyr::select_(~ region, ~ value)
  map_data$value <- as.numeric(as.character(map_data$value))

  map_data <- map_data %>% dplyr::group_by_(~ region)
  map_data <- dplyr::summarise_(map_data, ~ value = sum(value, na.rm = TRUE))
  map_data <-  dplyr::ungroup(map_data)

  map_data$value <- ifelse(is.na(map_data$value), 0, map_data$value)

  breaks <- c(0, 1, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
  palette_name <- "Reds"
  map_palette <- RColorBrewer::brewer.pal(length(breaks)  , name = palette_name)

  if(max(map_data$value) > max(breaks)){
    breaks <- c(breaks, max(map_data$value))
  }

  map_palette[1] <- "#ffffff"
  map_data <- map_data %>%
    dplyr::mutate_(value = ~ cut(value, breaks = breaks,
                                 include.lowest = TRUE, right = F))
  level_names <- levels(map_data$value)
  level_names[length(level_names)] <- paste0(">=", 1000000000)
  map_data$value <- factor(map_data$value, levels = levels(map_data$value), labels = level_names)
  out <- choroplethr::CountyChoropleth$new(map_data)
  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "# of Crops damaged", values = map_palette)

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
