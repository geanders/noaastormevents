library(dplyr)

#forecast zones cited from https://www.weather.gov/source/gis/Shapefiles/County/bp08mr23.dbx
forecast_zone_data <- read.csv(file = "~//noaastormevents//data-raw//forecast_zones.csv", sep = "|",
                               col.names = c("STATE","ZONE","CWA","NAME","STATE_ZONE","COUNTY","FIPS",
                                             "TIME_ZONE","FE_AREA","LAT","LON"))
forecast_zone_data <- forecast_zone_data %>%
  select(STATE, NAME, COUNTY, FIPS)

usethis::use_data(forecast_zone_data)


