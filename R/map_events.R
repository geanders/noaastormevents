#' Find all event listings for date range
#'
#' This function will find all of the events in the US for a specified date
#' range.
#'
#' @param begin_date A character string giving the date, in the format
#'    "%Y-%m-%d".
#' @param end_date A character string giving the date, in the format
#'    "%Y-%m-%d". The end date must be in the same year as \code{begin_date}.
#' @param storm A logical value indicating whether to filter events to only
#'    those in tropical storm-related categories.
#'
#' @examples
#' find_events(first_date = "1999-10-15", last_date = "1999-10-20")
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
find_events <- function(first_date, last_date, storm = FALSE){

  first_date <- lubridate::ymd(begin_date)
  last_date <- lubridate::ymd(end_date)
  if(last_date < first_date | year(first_date) != year(last_date)){
    stop("The `last_date` must be in the same year as and after the `first_date`.")
  }

  file_name <- paste0("data-raw/StormEvents_details-ftp_v1.0_d",
                      year(first_date),
                      "_c20160223.csv")
  storm_data <- suppressWarnings(data.table::fread(file_name,
                            select = c("BEGIN_YEARMONTH", "BEGIN_DAY",
                                       "END_YEARMONTH", "END_DAY",
                                       "STATE_FIPS", "CZ_FIPS", "EVENT_TYPE"),
                           col.names = c("begin_ym", "begin_d", "end_ym", "end_d",
                                         "st_fips", "type", "ct_fips"))) %>%
    dplyr::mutate(begin_d = sprintf("%02s", begin_d),
                  end_d = sprintf("%02s", end_d),
                  ct_fips = sprintf("%03s", ct_fips)) %>%
    tidyr::unite(begin_date, begin_ym, begin_d, sep = "") %>%
    tidyr::unite(end_date, end_ym, end_d, sep = "") %>%
    tidyr::unite(fips, st_fips, ct_fips, sep = "") %>%
    dplyr::mutate(begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                  end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
    dplyr::filter(!is.na(begin_date) &
                    begin_date %within% lubridate::interval(first_date, last_date))

  if (storm == T) {
    ts_types <- c("Coastal Flood","Flash Flood","Flood","Heavy Rain",
                  "High Surf","High Wind Hurricane (Typhoon)",
                  "Storm Surge/Tide","Strong Wind","Thunderstorm Wind",
                  "Tornado","Tropical Storm","Waterspout")
    storm_data <- dplyr::filter(storm_data, type %in% ts_types)
  }

  return(storm_data)
}

#' Map storm events for a date range
#'
#' This function maps all storm events listed with a starting date within a
#' specified date range.
#'
#' @param east_only A logical value specifying whether to restrict the map to
#'    the eastern half of the United States.
#' @inheritParams find_events
#'
#' @examples
#' map_events(first_date = "1999-10-15", last_date = "1999-10-20")
#' map_events(first_date = "1999-10-16", last_date = "1999-10-18",
#'    east_only = FALSE, storm = TRUE)
#' map_events(first_date = "1999-10-16", last_date = "1999-10-18",
#'    plot_type = "number of events")
map_events <- function(first_date, last_date, storm = FALSE, east_only = TRUE,
                       plot_type = "any events"){

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

  map_data <- find_events(first_date = first_date, last_date = last_date,
                          storm = storm) %>%
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
      dplyr::group_by(region) %>%
      dplyr::summarize(value = sum(!is.na(value))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = factor(value, levels = 0:max(value)))
  }

  out <- choroplethr::CountyChoropleth$new(map_data)

  if(east_only){
    out$set_zoom(eastern_states)
  } else {
    all_states <- out$get_zoom()
    continental_states <- all_states[!(all_states %in% c("alaska", "hawaii"))]
    out$set_zoom(continental_states)
  }

  if(plot_type == "any events"){
    out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                   values = c("#e6550d", "white"))
  } else if (plot_type == "number of events"){
    map_palette <- RColorBrewer::brewer.pal(length(levels(map_data$value)),
                                            "Oranges")
    map_palette[1] <- "#f7f7f7"
    out$ggplot_scale <- ggplot2::scale_fill_manual(name = "# of events",
                                                   values = map_palette)
  }

  return(out$render())
}

