#' Find all indirect death listings for date range
#'
#' This function will find all of the indirect deaths in the US for a specified date
#' range.
#'
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples
#' find_indirect_deaths(date_range = c("1999-10-15", "1999-10-20"))
#'
#' find_indirect_deaths(date_range = c("1999-10-16", "1999-10-18"),
#'    storm = "Floyd-1999", dist_limit = 200)
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#'
#' @export
find_indirect_deaths <- function(date_range = NULL, ts_only = FALSE,
                               dist_limit = NULL, storm = NULL){

  processed_inputs <- process_input_args(date_range = date_range, storm = storm)
  date_range <- processed_inputs$date_range
  storm <- processed_inputs$storm

  storm_data <- create_storm_data(date_range = date_range,  storm = storm) %>%
    dplyr::select(BEGIN_YEARMONTH, BEGIN_DAY, END_YEARMONTH, END_DAY,
                  STATE_FIPS, CZ_FIPS, CZ_TYPE, EVENT_TYPE, DEATHS_INDIRECT) %>%
    dplyr::rename(type = EVENT_TYPE,
                  indirect_deaths = DEATHS_INDIRECT) %>%
    adjust_storm_data(date_range = date_range, ts_only = ts_only,
                      dist_limit = dist_limit, storm = storm)

  return(storm_data)
}

#' Map indirect deaths for a date range
#'
#' This function maps all indirect deaths listed with a starting date within a
#' specified date range.
#'
#' @inheritParams map_events
#' @inheritParams create_storm_data
#' @inheritParams adjust_storm_data
#'
#' @examples
#' map_indirect_deaths(date_range = c("1999-10-15", "1999-10-20"))
#' map_indirect_deaths(date_range = c("1999-10-16", "1999-10-18"),
#'    east_only = FALSE, ts_only = TRUE)
#' map_indirect_deaths(date_range = c("1999-10-16", "1999-10-18"))
#' map_indirect_deaths(storm = "Floyd-1999", add_tracks = TRUE)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_indirect_deaths <- function(date_range = NULL, ts_only = FALSE, east_only = TRUE,
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

  map_data <- find_indirect_deaths(date_range = date_range,
                                 storm = storm, dist_limit = dist_limit,
                                 ts_only = ts_only) %>%
    dplyr::mutate(fips = as.numeric(fips)) %>%
    dplyr::rename(region = fips, value = indirect_deaths) %>%
    dplyr::full_join(county.regions, by = "region") %>%
    dplyr::filter(!is.na(county.name))

  if(east_only){
    map_data <- dplyr::filter(map_data, state.name %in% eastern_states)
  }

  map_data <- map_data %>% dplyr::select(region, value)
  map_data$value <- as.numeric(as.character(map_data$value))

  map_data <- map_data %>% dplyr::group_by(region)
  map_data <- dplyr::summarise(map_data, value = sum(value, na.rm = TRUE))
  map_data <-  dplyr::ungroup(map_data)

  max_value <- max(map_data$value)
  ceil <- floor(max_value/10^(floor(log10(max_value))))*10^(floor(log10(max_value)))
  decimal <- floor(max_value/10^(floor(log10(max_value))))

  if(decimal == 1) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/5))
  } else if(decimal == 2) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/5))
  } else if(decimal == 3) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/6))
  } else if(decimal == 4) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/5))
  } else if(decimal == 5) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/5))
  } else if(decimal == 6) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/6))
  } else if(decimal == 7) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/7))
  } else if(decimal == 8) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/5))
  } else if(decimal == 9) {
    breaks <- c(0,seq(1, ceil+1, by = ceil/5))
  }

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
  level_names[length(level_names)] <- paste0(">=", ceil)
  map_data$value <- factor(map_data$value,
                           levels = levels(map_data$value),
                           labels = level_names)
  out <- choroplethr::CountyChoropleth$new(map_data)

  if(east_only){
    out$set_zoom(eastern_states)
  } else {
    all_states <- out$get_zoom()
    continental_states <- all_states[!(all_states %in% c("alaska", "hawaii"))]
    out$set_zoom(continental_states)
  }


  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "# of indirect deaths",
                                                 values = map_palette)


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

