#' Find all damaged property listings for date range
#'
#' This function will find all of the property damaged in the US for a specified date
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
#' find_damage_property(first_date = "1999-10-15", last_date = "1999-10-20")
#'
#' find_damage_property(first_date = "1999-10-16", last_date = "1999-10-18",
#'    storm = "Floyd-1999", dist_limit = 200)
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#'
#' @export
find_damage_property <- function(first_date = NULL, last_date = NULL, ts_only = FALSE,
                              dist_limit = NULL, storm = NULL){

  storm_data <- get_file(first_date = first_date, last_date = last_date,
                         storm = storm)

  storm_data <-  storm_data %>%
    dplyr::select(BEGIN_YEARMONTH, BEGIN_DAY,
                  END_YEARMONTH, END_DAY,
                  STATE_FIPS, CZ_FIPS, DAMAGE_PROPERTY)%>%
    plyr::rename(c(BEGIN_YEARMONTH="begin_ym",
                   BEGIN_DAY="begin_d",
                   END_YEARMONTH="end_ym",
                   END_DAY="end_d",
                   STATE_FIPS="st_fips",
                   DAMAGE_PROPERTY="damage_property",
                   CZ_FIPS="ct_fips")) %>%
    dplyr::mutate(begin_d = sprintf("%02s", begin_d),
                  end_d = sprintf("%02s", end_d),
                  ct_fips = sprintf("%03s", ct_fips),
                  st_fips = sprintf("%02s", st_fips)) %>%
    tidyr::unite(begin_date, begin_ym, begin_d, sep = "") %>%
    tidyr::unite(end_date, end_ym, end_d, sep = "") %>%
    tidyr::unite(fips, st_fips, ct_fips, sep = "") %>%
    dplyr::tbl_df()

  storm_data <-  adjust_file(first_date = first_date, last_date = last_date, ts_only = ts_only,
                             dist_limit = dist_limit, storm = storm, data = storm_data)


  num.property = as.numeric(gsub("[^0-9]", "", storm_data$damage_property))
  letter.property = as.numeric(ifelse(grepl("K+", storm_data$damage_property, perl=TRUE), 1000,
                                   ifelse(grepl("M+", storm_data$damage_property, perl=TRUE), 1000000,
                                          ifelse(grepl("B+", storm_data$damage_property, perl=TRUE), 1000000000,
                                                 ifelse(grepl("0+", storm_data$damage_property, perl=TRUE), 0, " ")))))
  storm_data$damage_property <- as.matrix(num.property * letter.property)


  return(storm_data)
}

#' Map property damaged for a date range
#'
#' This function maps all property damaged listed with a starting date within a
#' specified date range.
#'
#' @param east_only A logical value specifying whether to restrict the map to
#'    the eastern half of the United States (default is TRUE).
#' @param add_tracks A logical value specifying whether to add the tracks of
#'    a hurricane to the map (default = FALSE).
#' @inheritParams find_damage_property
#'
#' @examples
#' map_damage_property(first_date = "1999-10-15", last_date = "1999-10-20")
#' map_damage_property(first_date = "1999-10-16", last_date = "1999-10-18",
#'    east_only = FALSE, ts_only = TRUE)
#' map_damage_property(first_date = "1999-10-16", last_date = "1999-10-18")
#' map_damage_property(first_date = "1999-10-16", last_date = "1999-10-18",
#'    dist_limit = 100, storm = "Floyd-1999",
#'     add_tracks = TRUE)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_damage_property <- function(first_date = NULL,
                                last_date = NULL,
                                ts_only = FALSE,
                                east_only = TRUE,
                                dist_limit = NULL,
                                storm = NULL,
                                add_tracks = FALSE)

{

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

  map_data <- find_damage_property(first_date = first_date, last_date = last_date,
                                storm = storm, dist_limit = dist_limit,
                                ts_only = ts_only) %>%
    dplyr::mutate(fips = as.numeric(fips)) %>%
    dplyr::rename(region = fips, value = damage_property) %>%
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

  map_data$value <- ifelse(map_data$value == 0, NA, map_data$value)


  exposure_palette <- RColorBrewer::brewer.pal((9) -2 , name = "Reds")

  out <- choroplethr::CountyChoropleth$new(map_data)

  if(east_only){
    out$set_zoom(eastern_states)
  } else {
    all_states <- out$get_zoom()
    continental_states <- all_states[!(all_states %in% c("alaska", "hawaii"))]
    out$set_zoom(continental_states)
  }

  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "Property Damaged",
                                                 values = exposure_palette)


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






































