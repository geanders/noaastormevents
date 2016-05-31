data = read.csv("data-raw/StormEvents_details-ftp_v1.0_d1999_c20160223.csv", header = TRUE)

# Try dplyr functions: mutate, rename, filter

library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)

map_damagecrops <- function(data, begin_date, end_date, storm = F, track = F, dist_limit = NA){
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

  num.crops = as.numeric(gsub("[^0-9]", "", data1$DAMAGE_CROPS))
  letter.crops = as.numeric(ifelse(grepl("K+", data1$DAMAGE_CROPS, perl=TRUE), 1000,
                                 ifelse(grepl("M+", data1$DAMAGE_CROPS, perl=TRUE), 1000000,
                                        ifelse(grepl("B+", data1$DAMAGE_CROPS, perl=TRUE), 1000000000,
                                               ifelse(grepl("0+", data1$DAMAGE_CROPS, perl=TRUE), 0, " ")))))

  DAMAGE_CROPS = as.matrix(num.crops * letter.crops)
  data1$DAMAGE_CROPS = DAMAGE_CROPS


  DamageCrops = data1[,c("FIPS","DAMAGE_CROPS")]
  colnames(DamageCrops) = c("region", "value")
  DamageCrops[,2] = as.numeric(as.character(DamageCrops[,2]))

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

    DamageCrops = DamageCrops %>%
      dplyr::filter(region %in% selected$region)
  }
  ###

  data(county.regions)
  region = data.frame(county.regions$region, rep(0,nrow(county.regions)))
  colnames(region) = c("region", "value")

  DamageCrops = rbind(region, DamageCrops)

  aggDamageCrops = aggregate(value ~ region, data = DamageCrops, sum)
  aggDamageCrops[, 2] <- ifelse(aggDamageCrops[ ,2] == 0, NA, aggDamageCrops[ ,2])

  eastern_states <- c("alabama", "arkansas", "connecticut", "delaware",
                      "district of columbia", "florida", "georgia", "illinois",
                      "indiana", "iowa", "kansas", "kentucky", "louisiana",
                      "maine", "maryland", "massachusetts", "michigan",
                      "mississippi", "missouri", "new hampshire", "new jersey",
                      "new york", "north carolina", "ohio", "oklahoma",
                      "pennsylvania", "rhode island", "south carolina",
                      "tennessee", "texas", "vermont", "virginia",
                      "west virginia", "wisconsin")

  breaks <- seq(0, 10, by = 1)
  exposure_palette <- RColorBrewer::brewer.pal(length(breaks) - 2, name = "Greens")

  out <- choroplethr::CountyChoropleth$new(aggDamageCrops)
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

