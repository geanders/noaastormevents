data = read.csv("data-raw/StormEvents_details-ftp_v1.0_d1999_c20160223.csv", header = TRUE)

# Try dplyr functions: mutate, rename, filter

library(lubridate)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)

map_damagecrops <- function(data, begin_date, end_date, storm = F){
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

  #data(county.regions)
  #region = data.frame(county.regions$region, rep(0,nrow(county.regions)))
  #colnames(region) = c("region", "value")
  #DamageCrops0 = rbind(DamageCrops, region)
  aggDamageCrops = aggregate(value ~ region, data = DamageCrops, sum)

  county_choropleth(aggDamageCrops, num_colors = 9, state_zoom = c("alabama", "arkansas",
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
                                                                 "wisconsin"), title = "Damaged Crops")
}

