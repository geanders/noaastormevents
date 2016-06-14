#' Find all damaged crop listings for date range
#'
#' This function will find all of the crops damaged in the US for a specified date
#' range.
#'
#' @param begin_date A character string giving the date, in the format
#'    "\%Y-\%m-\%d".
#' @param end_date A character string giving the date, in the format
#'    "\%Y-\%m-\%d". The end date must be in the same year as \code{begin_date}.
#' @param ts_only A logical value indicating whether to filter events to only
#'    those in tropical storm-related categories.
#'
#' @examples
#' find_damage_crops(first_date = "1999-10-15", last_date = "1999-10-20")
#'
#' find_damage_crops(first_date = "1999-10-16", last_date = "1999-10-18",
#'    storm = "Floyd-1999", dist_limit = 200)
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#'
#' @export
find_damage_crops <- function(first_date = NULL, last_date = NULL, ts_only = FALSE,
                        dist_limit = NULL, storm = NULL){

  if(!is.null(first_date) & !is.null(last_date)){
    first_date <- lubridate::ymd(first_date)
    last_date <- lubridate::ymd(last_date)
    if(last_date < first_date | lubridate::year(first_date) !=
       lubridate::year(last_date)){
      stop(paste0("The `last_date` must be in the same year as and ",
                  "after the `first_date`."))
    }
  }


  Year <- hurricaneexposure::closest_dist %>%
    dplyr::filter_(~ storm_id == storm)
  Year <-lubridate::year(lubridate::ymd(Year$closest_date[1]))

  file_name <- find_file_name(Year)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/",
                      "swdi/stormevents/csvfiles/",file_name)


  if(!exists("lst")) {
    temp <- tempfile()
    utils::download.file(path_name, temp)
    storm_data_full <<- suppressWarnings(utils::read.csv(gzfile(temp),
                                                         as.is = TRUE))
    unlink(temp)
  } else if(!file_name %in% lst) {
    temp <- tempfile()
    download.file(path_name, temp)
    storm_data_full <<- suppressWarnings(utils::read.csv(gzfile(temp),
                                                         as.is = TRUE))
    unlink(temp)
  }

  if(exists("lst")) {
    lst <<- c(lst, file_name)
  } else {
    lst <<- c(file_name)
  }

    storm_data <- storm_data_full %>%
      dplyr::select(BEGIN_YEARMONTH, BEGIN_DAY,
                    END_YEARMONTH, END_DAY,
                    STATE_FIPS, CZ_FIPS, DAMAGE_CROPS)%>%
      dplyr::rename(begin_ym = BEGIN_YEARMONTH,
                     begin_d = BEGIN_DAY,
                     end_ym = END_YEARMONTH,
                     end_d = END_DAY,
                     st_fips = STATE_FIPS,
                     damage_crops = DAMAGE_CROPS,
                     ct_fips = CZ_FIPS) %>%
      dplyr::mutate(begin_d = sprintf("%02s", begin_d),
                    end_d = sprintf("%02s", end_d),
                    ct_fips = sprintf("%03s", ct_fips)) %>%
      tidyr::unite(begin_date, begin_ym, begin_d, sep = "") %>%
      tidyr::unite(end_date, end_ym, end_d, sep = "") %>%
      tidyr::unite(fips, st_fips, ct_fips, sep = "") %>%
      dplyr::tbl_df()


  if(!is.null(dist_limit)) {
    distance_df <- hurricaneexposure::closest_dist %>%
      dplyr::filter_(~ storm_id == storm & storm_dist <= dist_limit)
  } else {
    distance_df <- hurricaneexposure::closest_dist %>%
      dplyr::filter_(~ storm_id == storm)
  }

  if(!is.null(first_date) & !is.null(last_date)){
    storm_data <- storm_data %>%
      dplyr::mutate(begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                    end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
      dplyr::filter(!is.na(begin_date) &
                      begin_date %within% lubridate::interval(ymd(first_date), ymd(last_date))) %>%
      dplyr::left_join(distance_df,  by = "fips") %>%
      dplyr::filter_(~ !is.na(storm_dist))
  } else {
    first_date <- substr(min(as.numeric(distance_df$closest_date)), 1, 8)
    last_date <-  substr(max(as.numeric(distance_df$closest_date)), 1, 8)
    storm_data <- dplyr::mutate(storm_data,
                                begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                                end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
      dplyr::filter(!is.na(begin_date) &
                      begin_date %within% lubridate::interval(lubridate::ymd(first_date),
                                                              lubridate::ymd(last_date))) %>%
      dplyr::left_join(distance_df, by = "fips") %>%
      dplyr::filter_(~ !is.na(storm_dist))
  }

  if (ts_only) {
    ts_types <- c("Coastal Flood","Flash Flood" ,"Flood","Heavy Rain",
                  "High Surf","High Wind Hurricane (Typhoon)",
                  "Storm Surge/Tide","Strong Wind","Thunderstorm Wind",
                  "Tornado","Tropical Storm","Waterspout")
    storm_data <- dplyr::filter(storm_data, type %in% ts_types)
  }


  num.crops <- as.numeric(gsub("[^0-9]", "", storm_data$damage_crops))
  letter.crops <- as.numeric(ifelse(grepl("K+", storm_data$damage_crops, perl=TRUE), 1000,
                                      ifelse(grepl("M+", storm_data$damage_crops, perl=TRUE), 1000000,
                                             ifelse(grepl("B+", storm_data$damage_crops, perl=TRUE), 1000000000,
                                                    ifelse(grepl("0+", storm_data$damage_crops, perl=TRUE), 0, " ")))))
  storm_data$damage_crops <- as.matrix(num.crops * letter.crops)


  return(storm_data)
}

#' Map crops damaged for a date range
#'
#' This function maps all crops damaged listed with a starting date within a
#' specified date range.
#'
#' @param east_only A logical value specifying whether to restrict the map to
#'    the eastern half of the United States (default is TRUE).
#' @param add_tracks A logical value specifying whether to add the tracks of
#'    a hurricane to the map (default = FALSE).
#' @inheritParams find_damage_crops
#'
#' @examples
#' map_damage_crops(first_date = "1999-10-15", last_date = "1999-10-20")
#' map_damage_crops(first_date = "1999-10-16", last_date = "1999-10-18",
#'    east_only = FALSE, ts_only = TRUE)
#' map_damage_crops(first_date = "1999-10-16", last_date = "1999-10-18")
#' map_damage_crops(first_date = "1999-10-16", last_date = "1999-10-18",
#'    dist_limit = 100, storm = "Floyd-1999",
#'     add_tracks = TRUE)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_damage_crops <- function(first_date = NULL, last_date = NULL, ts_only = FALSE, east_only = TRUE,
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

  map_data <- find_damage_crops(first_date = first_date, last_date = last_date,
                                   storm = storm, dist_limit = dist_limit,
                                   ts_only = ts_only) %>%
    dplyr::mutate(fips = as.numeric(fips)) %>%
    dplyr::rename(region = fips, value = damage_crops) %>%
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
  out$set_zoom(eastern_states)
  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "Crops Damaged",
                                                 values = exposure_palette)


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






































