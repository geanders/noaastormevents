data = read.csv("data-raw/StormEvents_details-ftp_v1.0_d1999_c20160223.csv", header = TRUE)

# Try dplyr functions: mutate, rename, filter

library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)

storm = NULL
storm = as.logical(storm)

map_events <- function(data, begin_date, end_date, storm = FALSE){
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

  data(county.regions)
  region = data.frame(county.regions$region, rep(0,nrow(county.regions)))
  colnames(region) = c("region", "value")
  Event0 = rbind(Event, region)
  aggEvent = aggregate(value ~ region, data = Event0, sum)
  aggEvent[, 2] = ifelse(aggEvent[, 2] == "0", 0, 1)

  county_choropleth(aggEvent, num_colors = 9, state_zoom = c("alabama", "arkansas",
                                                             "connecticut", "delaware",
                                                             "district of columbia", "florida",
                                                             "georgia", "illinois", "indiana",
                                                             "iowa", "kansas", "kentucky", "louisiana",
                                                             "maine", "maryland", "massachusetts",
                                                             "michigan", "mississippi",
                                                             "missouri", "new hampshire", "new jersey",
                                                             "new york", "north carolina", "ohio",
                                                             "oklahoma", "pennsylvania", "rhode island",
                                                             "south carolina", "tennessee", "texas",
                                                             "vermont", "virginia", "west virginia",
                                                             "wisconsin"), title = "Events")
}

