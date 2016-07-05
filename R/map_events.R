#' Find all event listings for date range
#'
#' This function will find all of the events in the US for a specified date
#' range.
#'
#' @param begin_date A character string giving the date, in the format
#'    "%Y-%m-%d".
#' @param end_date A character string giving the date, in the format
#'    "%Y-%m-%d". The end date must be in the same year as \code{begin_date}.
#' @param ts_only A logical value indicating whether to filter events to only
#'    those in tropical storm-related categories.
#'
#' @examples
#' find_events(first_date = "1999-10-15", last_date = "1999-10-20")
#'
#' find_events(first_date = "1999-10-16", last_date = "1999-10-18",
#'    storm = "Floyd-1999", dist_limit = 200)
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#'
#' @export
find_events <- function(date_range = c(NULL, NULL), ts_only = FALSE,
                        dist_limit = NULL, storm = NULL){

  storm_data <- get_file(date_range = date_range,  storm = storm)

  storm_data <- storm_data %>%
    dplyr::select(BEGIN_YEARMONTH, BEGIN_DAY, END_YEARMONTH, END_DAY, STATE_FIPS, CZ_FIPS, EVENT_TYPE)%>%
    plyr::rename(c(BEGIN_YEARMONTH="begin_ym",
                   BEGIN_DAY="begin_d",
                   END_YEARMONTH="end_ym",
                   END_DAY="end_d",
                   STATE_FIPS="st_fips",
                   EVENT_TYPE="type",
                   CZ_FIPS="ct_fips")) %>%
    dplyr::mutate(begin_d = sprintf("%02s", begin_d),
                  end_d = sprintf("%02s", end_d),
                  ct_fips = sprintf("%03s", ct_fips),
                  st_fips = sprintf("%02s", st_fips)) %>%
    tidyr::unite(begin_date, begin_ym, begin_d, sep = "") %>%
    tidyr::unite(end_date, end_ym, end_d, sep = "") %>%
    tidyr::unite(fips, st_fips, ct_fips, sep = "") %>%
    dplyr::tbl_df()

  storm_data <-  adjust_file(date_range = date_range, ts_only = ts_only,
                             dist_limit = dist_limit, storm = storm,
                             data = storm_data)

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
#' @inheritParams find_events
#'
#' @examples
#' map_events(first_date = "1999-10-15", last_date = "1999-10-20")
#' map_events(first_date = "1999-10-16", last_date = "1999-10-18",
#'    east_only = FALSE, ts_only = TRUE)
#' map_events(first_date = "1999-10-16", last_date = "1999-10-18",
#'    plot_type = "number of events")
#' map_events(first_date = "1999-10-16", last_date = "1999-10-18",
#'    dist_limit = 100, storm = "Floyd-1999",
#'     add_tracks = TRUE, plot_type = "number of events")
#'
#' @importFrom dplyr %>%
#'
#' @export
map_events <- function(date_range = c(NULL, NULL), ts_only = FALSE,
                       east_only = TRUE,
                       plot_type = "any events", dist_limit = NULL,
                       storm = NULL, add_tracks = FALSE){

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

  map_data <- find_events(date_range = date_range, storm = storm, dist_limit = dist_limit, ts_only = ts_only) %>%
    dplyr::mutate(fips = as.numeric(fips)) %>%
    dplyr::rename(region = fips, value = type) %>%
    dplyr::full_join(county.regions, by = "region") %>%
    dplyr::filter(!is.na(county.name))

  if(east_only){
    map_data <- dplyr::filter(map_data, state.name %in% eastern_states)
  }

  map_data <- map_data %>% dplyr::select(region, value)

  if(plot_type == "any events"){
    map_data <- map_data %>%
      dplyr::group_by(region) %>%
      dplyr::summarize(value = sum(!is.na(value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = value > 0,
                    value = factor(value, levels = c(TRUE, FALSE),
                                   labels = c("Event(s)", "No Event")))
  } else if (plot_type == "number of events"){
    map_data <- map_data %>%
      dplyr::mutate(value = ifelse(is.na(value), 0, 1)) %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(value = sum(as.numeric(value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = factor(value, levels = 0:max(value)))
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
      dplyr::mutate(value = as.numeric(as.character(value))) %>%
      dplyr::mutate(value = ifelse(value > boundary, 9999, value)) %>%
      dplyr::mutate(value = as.factor(value))
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
    tracks_map <- hurricaneexposuredata::map_tracks(storms = storm,
                                                plot_object = out$render(),
                                                plot_points = FALSE,
                                                color = "black")
    return(tracks_map)
  } else {
    return(out$render())
  }
}


