#' Find all indirect death listings for date range
#'
#' This function will find all of the indirect deaths in the US for a specified date
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
#' find_indirect_deaths(first_date = "1999-10-15", last_date = "1999-10-20")
#'
#' find_indirect_deaths(first_date = "1999-10-16", last_date = "1999-10-18",
#'    storm = "Floyd-1999", dist_limit = 200)
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate %within%
#'
#' @export
find_indirect_deaths <- function(first_date = NULL, last_date = NULL, ts_only = FALSE,
                               dist_limit = NULL, storm = NULL){

  if(!is.null(first_date) & !is.null(last_date)){
    first_date <- lubridate::ymd(first_date)
    last_date <- lubridate::ymd(last_date)
    if(last_date < first_date | year(first_date) != year(last_date)){
      stop("The `last_date` must be in the same year as and after the `first_date`.")
    }
  }


  if(!is.null(storm)){
    Year <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm)
  } else {
    Year <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ lubridate::year(closest_date) == lubridate::year(first_date))
  }
  Year <-lubridate::year(lubridate::ymd(Year$closest_date[1]))

  file_name <- find_file_name(Year)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/",file_name)


  Year <- hurricaneexposuredata::closest_dist %>%
    dplyr::filter_(~ storm_id == storm)
  Year <-lubridate::year(lubridate::ymd(Year$closest_date[1]))

  file_name <- find_file_name(Year)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/",file_name)

  if(!exists("lst")) {
    temp <- tempfile()
    download.file(path_name, temp)
    lst <<- list()
    lst[[as.character(Year)]] <<-  suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
    unlink(temp)
  } else if(is.null(lst[[as.character(Year)]])) {
    temp <- tempfile()
    download.file(path_name, temp)
    lst[[as.character(Year)]] <<-  suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
    unlink(temp)
  }


  storm_data <- lst[[as.character(Year)]] %>%
    dplyr::select(BEGIN_YEARMONTH, BEGIN_DAY,
                  END_YEARMONTH, END_DAY,
                  STATE_FIPS, CZ_FIPS, DEATHS_INDIRECT)%>%
    plyr::rename(c(BEGIN_YEARMONTH="begin_ym",
                   BEGIN_DAY="begin_d",
                   END_YEARMONTH="end_ym",
                   END_DAY="end_d",
                   STATE_FIPS="st_fips",
                   DEATHS_INDIRECT="indirect_deaths",
                   CZ_FIPS="ct_fips")) %>%
    dplyr::mutate(begin_d = sprintf("%02s", begin_d),
                  end_d = sprintf("%02s", end_d),
                  ct_fips = sprintf("%03s", ct_fips),
                  st_fips = sprintf("%02s", st_fips)) %>%
    tidyr::unite(begin_date, begin_ym, begin_d, sep = "") %>%
    tidyr::unite(end_date, end_ym, end_d, sep = "") %>%
    tidyr::unite(fips, st_fips, ct_fips, sep = "") %>%
    dplyr::tbl_df()

  if(!is.null(dist_limit) & !is.null(storm)) {
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm & storm_dist <= dist_limit)
  } else if(is.null(dist_limit) & !is.null(storm)){
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_id == storm)
  } else if(!is.null(dist_limit) & is.null(storm)){
    distance_df <- hurricaneexposuredata::closest_dist %>%
      dplyr::filter_(~ storm_dist <= dist_limit)
  } else {
    distance_df <- hurricaneexposuredata::closest_dist
  }


  if(!is.null(first_date) & !is.null(last_date)){
    if(!is.null(storm)){
      storm_first_date <- lubridate::ymd(min(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
      storm_last_date <-  lubridate::ymd(max(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
      storm_interval <- interval(storm_first_date, storm_last_date)
      if(!(first_date %within% storm_interval) & last_date %within% (storm_interval)){
        first_date <- storm_first_date
      } else if((first_date %within% storm_interval) & !(last_date %within% storm_interval)) {
        last_date <- storm_last_date
      } else if(!(first_date %within% storm_interval) & !(last_date %within% storm_interval)) {
        first_date <- storm_first_date
        last_date <- storm_last_date
      }
      storm_data <- storm_data %>%
        dplyr::mutate(begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                      end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
        dplyr::filter(!is.na(begin_date) &
                        lubridate::ymd(begin_date) %within% interval(first_date,last_date)) %>%
        dplyr::left_join(distance_df, by = "fips") %>%
        dplyr::filter_(~ !is.na(storm_dist))
    } else {
      storm_data <- storm_data %>%
        dplyr::mutate(begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                      end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
        dplyr::filter(!is.na(begin_date) &
                        lubridate::ymd(begin_date) %within% interval(first_date,last_date))
    }
  } else {
    first_date <- lubridate::ymd(min(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
    last_date <-  lubridate::ymd(max(as.numeric(gsub("[^0-9]","",as.character(distance_df$closest_date)))))
    storm_data <- dplyr::mutate(storm_data, begin_date = suppressWarnings(lubridate::ymd(begin_date)),
                                end_date = suppressWarnings(lubridate::ymd(end_date))) %>%
      dplyr::filter(!is.na(begin_date) &
                      lubridate::ymd(begin_date) %within% lubridate::interval(first_date,last_date)) %>%
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

  return(storm_data)
}


#' Map indirect deaths for a date range
#'
#' This function maps all indirect deaths listed with a starting date within a
#' specified date range.
#'
#' @param east_only A logical value specifying whether to restrict the map to
#'    the eastern half of the United States (default is TRUE).
#' @param add_tracks A logical value specifying whether to add the tracks of
#'    a hurricane to the map (default = FALSE).
#' @inheritParams find_indirect_deaths
#'
#' @examples
#' map_indirect_deaths(first_date = "1999-10-15", last_date = "1999-10-20")
#' map_indirect_deaths(first_date = "1999-10-16", last_date = "1999-10-18",
#'    east_only = FALSE, ts_only = TRUE)
#' map_indirect_deaths(first_date = "1999-10-16", last_date = "1999-10-18")
#' map_indirect_deaths(first_date = "1999-10-16", last_date = "1999-10-18",
#'    dist_limit = 100, storm = "Floyd-1999",
#'     add_tracks = TRUE)
#'
#' @importFrom dplyr %>%
#'
#' @export
map_indirect_deaths <- function(first_date = NULL, last_date = NULL, ts_only = FALSE, east_only = TRUE,
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

  map_data <- find_indirect_deaths(first_date = first_date, last_date = last_date,
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



  breaks <- c(0,seq(1, 201, by = 25))
  palette_name <- "Reds"
  map_palette <- RColorBrewer::brewer.pal(length(breaks)- 3 , name = palette_name)

  if(max(map_data$value) > max(breaks)){
    breaks <- c(breaks, max(map_data$value))
  }

  map_palette <- c("#ffffff", map_palette ,"#1a1a1a")
  map_data <- map_data %>%
    dplyr::mutate_(value = ~ cut(value, breaks = breaks,
                                 include.lowest = TRUE, right = FALSE))
  level_names <- levels(map_data$value)
  level_names[length(level_names)] <- ">201"
  map_data$value <- factor(map_data$value,
                           levels = levels(map_data$value),
                           labels = level_names)
  exposure_palette <- utils::tail(map_palette,
                                  length(unique(map_data$value)))
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
    tracks_map <- hurricaneexposuredata::map_tracks(storms = storm,
                                                plot_object = out$render(),
                                                plot_points = FALSE,
                                                color = "black")
    return(tracks_map)
  } else {
    return(out$render())
  }
}


























