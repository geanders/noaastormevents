#' Map storm events for a date range
#'
#' This function maps all storm events listed with a starting date within a
#' specified date range.
#'
#' @param event_data A dataframe of event data, as returned by the \code{find_events}
#'    function.
#' @param east_only A logical value specifying whether to restrict the map to
#'    the eastern half of the United States (default is TRUE).
#' @param add_tracks A logical value specifying whether to add the tracks of
#'    a hurricane to the map (default = FALSE).
#' @param plot_type Specifies the type of plot wanted. It can be either "any
#'    events" or "number of events".
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
#' map_events(event_data, east_only = FALSE)
#'
#' # Map for events identified based on a hurricane storm track
#' event_data <- find_events(storm = "Floyd-1999", dist_limit = 300)
#' map_events(event_data, plot_type = "number of events",
#'            storm = "Floyd-1999", add_tracks = TRUE)
#' map_events(event_data, plot_type = "crop damage",
#'            storm = "Floyd-1999", add_tracks = TRUE)
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
#'
#' @export
map_events <- function(event_data, east_only = TRUE, plot_type = "any events",
                       storm = NULL, add_tracks = FALSE){

  county_map_data <- get_county_map(east_only = east_only)
  county_states <- unique(stringr::str_replace(county_map_data$polyname, "[,].+", ""))

  map_data <- event_data %>%
    dplyr::filter_(~ stringr::str_to_lower(state) %in% county_states)

  if(plot_type == "any events"){
    map_data <- map_data %>%
      dplyr::count_(~ fips) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ factor(n > 0 & !is.na(n), levels = c(TRUE, FALSE),
                                      labels = c("Event(s)", "No Event")))
  } else if (plot_type == "number of events"){
    map_data <- map_data %>%
      dplyr::count_(~ fips) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(n), 0, n))
    if(max(map_data$value) <= 9){
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ factor(value, levels = 0:max(value)))
    } else {
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ cut(value,
                                     breaks = round(seq(from = 0, to = max(value), length.out = 9)),
                                     include.lowest = TRUE))
    }
  } else if (plot_type == "crop damage"){
    if(max(map_data$damage_crops) == 0) return("No crop damage reported for selected events.")
    map_data <- map_data %>%
      dplyr::group_by_(~ fips) %>%
      dplyr::summarize_(value = ~ sum(damage_crops)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, value),
                     value = ~ cut(value,
                                   breaks = c(signif(10 ** seq(0, ceiling(log10(max(value))),
                                                                  length.out = 9),
                                                  digits = 1)[1:8], max(value)),
                                   include.lowest = TRUE),
                     value = ~ forcats::fct_relabel(value, convert_damage_costs))
  } else if (plot_type == "property damage"){
    if(max(map_data$damage_property) == 0) return("No property damage reported for selected events.")
    map_data <- map_data %>%
      dplyr::group_by_(~ fips) %>%
      dplyr::summarize_(value = ~ sum(damage_property)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, value),
                     value = ~ cut(value,
                                   breaks = c(signif(10 ** seq(0, ceiling(log10(max(value))),
                                                             length.out = 9),
                                                   digits = 1)[1:8], max(value)),
                                   include.lowest = TRUE),
                     value = ~ forcats::fct_relabel(value, convert_damage_costs))
  } else if (plot_type == "direct deaths"){
    if(max(map_data$deaths_direct) == 0) return("No direct deaths reported for selected events.")
    map_data <- map_data %>%
      dplyr::group_by_(~ fips) %>%
      dplyr::summarize_(value = ~ sum(deaths_direct, na.rm = TRUE)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, value))
    if(max(map_data$value) <= 9){
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ factor(value, levels = 0:max(value)))
    } else {
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ cut(value,
                                     breaks = round(seq(from = 0, to = max(value), length.out = 9)),
                                     include.lowest = TRUE))
    }
  } else if (plot_type == "indirect deaths"){
      if(max(map_data$deaths_indirect) == 0) return("No indirect deaths reported for selected events.")
      map_data <- map_data %>%
      dplyr::group_by_(~ fips) %>%
      dplyr::summarize_(value = ~ sum(deaths_indirect, na.rm = TRUE)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, value))
      if(max(map_data$value) <= 9){
        map_data <- map_data %>%
          dplyr::mutate_(value = ~ factor(value, levels = 0:max(value)))
      } else {
        map_data <- map_data %>%
          dplyr::mutate_(value = ~ cut(value,
                                       breaks = round(seq(from = 0, to = max(value), length.out = 9)),
                                       include.lowest = TRUE))
    }
  } else if (plot_type == "direct injuries"){
    if(max(map_data$injuries_direct) == 0) return("No direct injuries reported for selected events.")
    map_data <- map_data %>%
      dplyr::group_by_(~ fips) %>%
      dplyr::summarize_(value = ~ sum(injuries_direct, na.rm = TRUE)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, value))
    if(max(map_data$value) <= 9){
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ factor(value, levels = 0:max(value)))
    } else {
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ cut(value,
                                     breaks = round(seq(from = 0, to = max(value), length.out = 9)),
                                     include.lowest = TRUE))
    }
  } else if (plot_type == "indirect injuries"){
    if(max(map_data$injuries_indirect) == 0) return("No indirect injuries reported for selected events.")
    map_data <- map_data %>%
      dplyr::group_by_(~ fips) %>%
      dplyr::summarize_(value = ~ sum(injuries_indirect, na.rm = TRUE)) %>%
      dplyr::right_join(county_map_data, by = "fips") %>%
      dplyr::mutate_(value = ~ ifelse(is.na(value), 0, value))
    if(max(map_data$value) <= 9){
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ factor(value, levels = 0:max(value)))
    } else {
      map_data <- map_data %>%
        dplyr::mutate_(value = ~ cut(value,
                                     breaks = round(seq(from = 0, to = max(value), length.out = 9)),
                                     include.lowest = TRUE))
    }
  }

  out <- map_data %>%
    ggplot2::ggplot() +
    ggplot2::geom_polygon(ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group, fill = ~ value),
                          color = "darkgray", size = 0.1) +
    ggplot2::geom_polygon(data = ggplot2::map_data("state", region = county_states),
                          ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group),
                          fill = NA, color = "black", size = 0.2) +
    ggplot2::theme_void() +
    ggplot2::coord_map()

  if (plot_type == "any events"){
    out <- out + ggplot2::scale_fill_manual(name = "", values = c("#e6550d", "white"), drop = FALSE)
  } else if (plot_type == "number of events"){
    map_palette <- RColorBrewer::brewer.pal(9, name = "Reds")
    map_palette[1] <- "#ffffff"
    if (length(levels(map_data$value)) <= 9){
      map_palette <- map_palette[1:length(levels(map_data$value))]
    }
    out <- out + ggplot2::scale_fill_manual(name = "# Events", values = map_palette, drop = FALSE)
  } else if (plot_type == "crop damage"){
    out <- out + viridis::scale_fill_viridis(name = "Crop damage ($)",
                                             option = "B", direction = -1,
                                             begin = 0.2, end = 0.9, discrete = TRUE,
                                             na.translate = FALSE, drop = FALSE)
  } else if (plot_type == "property damage"){
    out <- out + viridis::scale_fill_viridis(name = "Property damage ($)",
                                             option = "B", direction = -1,
                                             begin = 0.2, end = 0.9, discrete = TRUE,
                                             na.translate = FALSE, drop = FALSE)
  } else if (plot_type == "direct deaths"){
    map_palette <- RColorBrewer::brewer.pal(9, name = "Reds")
    map_palette[1] <- "#ffffff"
    if (length(levels(map_data$value)) <= 9){
      map_palette <- map_palette[1:length(levels(map_data$value))]
    }
    out <- out +
      ggplot2::scale_fill_manual(name = "# Direct Deaths", values = map_palette, drop = FALSE)
  } else if (plot_type == "indirect deaths"){
    map_palette <- RColorBrewer::brewer.pal(9, name = "Reds")
    map_palette[1] <- "#ffffff"
    if (length(levels(map_data$value)) <= 9){
      map_palette <- map_palette[1:length(levels(map_data$value))]
    }
    out <- out +
      ggplot2::scale_fill_manual(name = "# Indirect Deaths", values = map_palette, drop = FALSE)
  } else if (plot_type == "direct injuries"){
    map_palette <- RColorBrewer::brewer.pal(9, name = "Reds")
    map_palette[1] <- "#ffffff"
    if (length(levels(map_data$value)) <= 9){
      map_palette <- map_palette[1:length(levels(map_data$value))]
    }
    out <- out +
      ggplot2::scale_fill_manual(name = "# Direct Injuries", values = map_palette, drop = FALSE)
  } else if (plot_type == "indirect injuries"){
    map_palette <- RColorBrewer::brewer.pal(9, name = "Reds")
    map_palette[1] <- "#ffffff"
    if (length(levels(map_data$value)) <= 9){
      map_palette <- map_palette[1:length(levels(map_data$value))]
    }
    out <- out +
      ggplot2::scale_fill_manual(name = "# Indirect Injuries", values = map_palette, drop = FALSE)
  }

  if(add_tracks){
    out <- hurricaneexposure::map_tracks(storms = storm, plot_object = out,
                                         color = "black", alpha = 0.75)
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
get_county_map <- function(east_only = TRUE){

  map_data <- ggplot2::map_data(map = "county") %>%
    dplyr::filter_(~ !(region %in% c("alaska", "hawaii")))

  if(east_only){
    eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                        "district of columbia", "florida", "georgia", "illinois",
                        "indiana", "iowa", "kansas", "kentucky", "louisiana",
                        "maine", "maryland", "massachusetts", "michigan",
                        "mississippi", "missouri", "new hampshire", "new jersey",
                        "new york", "north carolina", "ohio", "oklahoma",
                        "pennsylvania", "rhode island", "south carolina",
                        "tennessee", "texas", "vermont", "virginia",
                        "west virginia", "wisconsin")

    map_data <- map_data %>%
      dplyr::filter_(~ region %in% eastern_states)
  }

  county.fips <- maps::county.fips %>%
    dplyr::mutate_(polyname = ~ as.character(polyname)) %>%
    dplyr::mutate_(polyname = ~ stringr::str_replace(polyname,
                                                       ":.+", ""))
  map_data <- map_data %>%
    tidyr::unite_(col = "polyname", from = c("region", "subregion"),
                  sep = ",") %>%
    dplyr::left_join(county.fips, by = "polyname")

  return(map_data)
}

#' Convert labels for damage costs to a prettier format
#'
#' @param x The levels of a factor, as formatted by \code{cut} for crop or
#'    property damage values.
#'
#' @examples
#' crop_dmg_levels <- c("[0,20]", "(20,600]", "(600,1e+04]", "(1e+04,3e+05]")
#' convert_damage_costs(crop_dmg_levels)
#'
#' @importFrom dplyr %>%
#'
#' @export
convert_damage_costs <- function(x){
  x_nums <- sapply(x, stringr::str_extract, pattern = "([0-9e+,].+)") %>%
    sapply(stringr::str_replace, pattern = "]", replacement = "") %>%
    stringr::str_split(",", simplify = TRUE) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate_each("as.numeric") %>%
    dplyr::mutate_each(dplyr::funs(formatC(., format = "f", digits = 0, big.mark = ","))) %>%
    tidyr::unite_(col = "value", from = c("V1", "V2"), sep = "--")
  return(paste0("$", as.character(x_nums$value)))
}
