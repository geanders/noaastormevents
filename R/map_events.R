data = read.csv("data-raw/StormEvents_details-ftp_v1.0_d1999_c20160223.csv", header = TRUE)

# Try dplyr functions: mutate, rename, filter

library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(hurricaneexposure)

storm = NULL
storm = as.logical(storm)

track = NULL
track = as.logical(track)

dist_limit = NULL
dist_limit = as.numeric(dist_limit)

map_events <- function(data, begin_date, end_date, storm = FALSE, track = FALSE, dist_limit = NA){
  begin.date = ymd(100*as.numeric(as.character(data$BEGIN_YEARMONTH)) + as.numeric(as.character(data$BEGIN_DAY)))
  end.date = ymd(100*as.numeric(as.character(data$END_YEARMONTH)) + as.numeric(as.character(data$END_DAY)))

  data0 = data.frame(begin.date, end.date, data)
  int = interval(ymd(begin_date), ymd(end_date))
  rows = which(data0$begin.date %within% int)
  county = as.matrix(as.numeric(as.character(data$STATE_FIPS))*1000 + as.numeric(as.character(data$CZ_FIPS)))
  data1 = data.frame(county[rows, ], data[rows, ])
  names(data1)[1] = c("FIPS")

  if (storm == T) {
    data1 = filter(data1, EVENT_TYPE %in% c("Coastal Flood","Flash Flood","Flood","Heavy Rain",
                                            "High Surf","High Wind Hurricane (Typhoon)",
                                            "Storm Surge/Tide","Strong Wind","Thunderstorm Wind",
                                           "Tornado","Tropical Storm","Waterspout"))
  }

  Event = data1[,c("FIPS","EVENT_TYPE")]
  colnames(Event) = c("region", "value")
  Event[, 2] = 1

  ###
  if (is.na(dist_limit) == F) {
    distance_df <- hurricaneexposure::closest_dist %>%
      dplyr::filter_(~ storm_id == "Floyd-1999") %>%
      dplyr::mutate_(exposed = ~ storm_dist <= dist_limit)

    metric_df <- distance_df %>%
      dplyr::mutate_(value = ~ factor(exposed,
                                      levels = c("FALSE", "TRUE")))

    map_data <- metric_df %>%
      dplyr::filter_(~ storm_id == "Floyd-1999") %>%
      dplyr::mutate_(region = ~ as.numeric(as.character(fips))) %>%
      dplyr::select_(~ region, ~ value)

    selected <- map_data %>%
      dplyr::filter(value == TRUE)

    Event = Event %>%
      dplyr::filter(region %in% selected$region)
  }
  ###

  data(county.regions)
  region = data.frame(county.regions$region, rep(0,nrow(county.regions)))
  colnames(region) = c("region", "value")

  Event0 = rbind(region, Event)

  aggEvent = aggregate(value ~ region, data = Event0, sum)
  aggEvent[, 2] = ifelse(aggEvent[, 2] == "0", 0, 1)

  eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                      "district of columbia", "florida", "georgia", "illinois",
                      "indiana", "iowa", "kansas", "kentucky", "louisiana",
                      "maine", "maryland", "massachusetts", "michigan",
                      "mississippi", "missouri", "new hampshire", "new jersey",
                      "new york", "north carolina", "ohio", "oklahoma",
                      "pennsylvania", "rhode island", "south carolina",
                      "tennessee", "texas", "vermont", "virginia",
                      "west virginia", "wisconsin")

  out <- choroplethr::CountyChoropleth$new(aggEvent)
  out$set_zoom(eastern_states)
  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                 values = c("white",
                                                            "navy"),
                                                 labels = c("Unexposed",
                                                            "Exposed"))

  if (track == F) {
    return(out$render())
  }
  else {
    out = map_tracks(storms = "Floyd-1999", plot_object = out$render())
    return(out)
  }
}

