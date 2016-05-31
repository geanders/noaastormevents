data = read.csv("data-raw/StormEvents_details-ftp_v1.0_d1999_c20160223.csv", header = TRUE)

# Try dplyr functions: mutate, rename, filter

library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)

map_indirectinjuries <- function(data, begin_date, end_date, storm = FALSE, track = FALSE, dist_limit = NA){
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
                                            "High Surf","High Wind Hurricane (Typhoon)","Storm Surge/Tide",
                                            "Strong Wind","Thunderstorm Wind","Tornado","Tropical Storm","Waterspout"))
  }

  IndirectInj = data1[,c("FIPS","INJURIES_INDIRECT")]
  colnames(IndirectInj) = c("region", "value")
  IndirectInj[,2] = as.numeric(as.character(IndirectInj[,2]))

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

    IndirectInj = IndirectInj %>%
      dplyr::filter(region %in% selected$region)
  }
  ###

  data(county.regions)
  region = data.frame(county.regions$region, rep(0,nrow(county.regions)))
  colnames(region) = c("region", "value")

  IndirectInj = rbind(region, IndirectInj)

  aggIndirectInj = aggregate(value ~ region, data = IndirectInj, sum)
  aggIndirectInj[, 2] <- ifelse(aggIndirectInj[ ,2] == 0, NA, aggIndirectInj[ ,2])


  eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                      "district of columbia", "florida", "georgia", "illinois",
                      "indiana", "iowa", "kansas", "kentucky", "louisiana",
                      "maine", "maryland", "massachusetts", "michigan",
                      "mississippi", "missouri", "new hampshire", "new jersey",
                      "new york", "north carolina", "ohio", "oklahoma",
                      "pennsylvania", "rhode island", "south carolina",
                      "tennessee", "texas", "vermont", "virginia",
                      "west virginia", "wisconsin")

  breaks <- seq(0, 9, by = 1)
  exposure_palette <- RColorBrewer::brewer.pal(length(breaks) - 2,name = "Blues")

  out <- choroplethr::CountyChoropleth$new(aggIndirectInj)
  out$set_zoom(eastern_states)
  out$ggplot_scale <- ggplot2::scale_fill_manual(name = "",
                                                 values = exposure_palette)

  if (track == F) {
    return(out$render())
  }
  else {
    out = map_tracks(storms = "Floyd-1999", plot_object = out$render())
    return(out)
  }
}


