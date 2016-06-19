# Script for hurricane exposure

########################################################################
#find_combined_events
find_combined_events <- function(first_date = NULL, last_date = NULL, ts_only = FALSE,
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
                  END_YEARMONTH, END_DAY, STATE_FIPS, CZ_FIPS,
                  EVENT_TYPE, INJURIES_DIRECT, INJURIES_INDIRECT,
                  DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS)%>%
    plyr::rename(c(BEGIN_YEARMONTH="begin_ym",
                   BEGIN_DAY="begin_d",
                   END_YEARMONTH="end_ym",
                   END_DAY="end_d",
                   STATE_FIPS="st_fips",
                   EVENT_TYPE="type",
                   CZ_FIPS="ct_fips",
                   INJURIES_DIRECT = "direct_injuries",
                   INJURIES_INDIRECT = "indirect_injuries",
                   DEATHS_DIRECT = "direct_deaths",
                   DEATHS_INDIRECT = "indirect_deaths",
                   DAMAGE_PROPERTY = "damage_property",
                   DAMAGE_CROPS = "damage_crops")) %>%
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
                      lubridate::ymd(begin_date) %within% interval(first_date,last_date)) %>%
      dplyr::left_join(distance_df,  by = "fips") %>%
      dplyr::filter_(~ !is.na(storm_dist))
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

  num.crops <- as.numeric(gsub("[^0-9]", "", storm_data$damage_crops))
  letter.crops <- as.numeric(ifelse(grepl("K+", storm_data$damage_crops, perl=TRUE), 1000,
                                    ifelse(grepl("M+", storm_data$damage_crops, perl=TRUE), 1000000,
                                           ifelse(grepl("B+", storm_data$damage_crops, perl=TRUE), 1000000000,
                                                  ifelse(grepl("0+", storm_data$damage_crops, perl=TRUE), 0, " ")))))
  storm_data$damage_crops <- as.matrix(num.crops * letter.crops)


  num.property = as.numeric(gsub("[^0-9]", "", storm_data$damage_property))
  letter.property = as.numeric(ifelse(grepl("K+", storm_data$damage_property, perl=TRUE), 1000,
                                      ifelse(grepl("M+", storm_data$damage_property, perl=TRUE), 1000000,
                                             ifelse(grepl("B+", storm_data$damage_property, perl=TRUE), 1000000000,
                                                    ifelse(grepl("0+", storm_data$damage_property, perl=TRUE), 0, " ")))))
  storm_data$damage_property <- as.matrix(num.property * letter.property)



  return(storm_data)
}



library(lubridate)
library(dplyr)
########################################################################
#Get 1996-2014 data
storm_id <- unique(hurricaneexposure::closest_dist$storm_id)
storm_id <- data.frame(storm_id)

combined_data <- NULL
for (i in 1996:2014) {
  storm_name <- filter(storm_id,gsub("[^0-9]", "", storm_id) == i)
  file_name <- paste0("combined_data",i)
  file_name <- NULL
  for (j in 1:nrow(storm_name)) {
    file_name <- rbind(file_name, find_combined_events(storm = storm_name[j,]))
  }
  combined_data <- rbind(combined_data, file_name)
}
combined_data <-dplyr::select(combined_data, fips, storm_id, type, direct_injuries, indirect_injuries,
                              direct_deaths, indirect_deaths, damage_property, damage_crops)



########################################################################
#Get 2015 data
name = NULL
storm_id = NULL
storm_name <- c("Alex", "Bonnie", "Colin", "Danielle", "Earl", "Fiona", "Gaston", "Hermine", "Ian",
                "Julia", "Karl", "Lisa", "Matthew", "Nicole", "Otto", "Paula", "Richard", "Shary",
                "Tobias", "Virginie", "Walter")
path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/",find_file_name(2015))
temp <- tempfile()
download.file(path_name, temp)
data2015 <-   suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
unlink(temp)
for (i in 1:nrow(data2015)) {
  name = NULL
  for (j in 1:length(storm_name)) {
    name = c(name, ifelse(grepl(storm_name[j],data2015$EPISODE_NARRATIVE[i],fixed = T),
                          storm_name[j], NA))
    name = (name[!is.na(name)])[1]
  }
  storm_id = c(storm_id, name)
}
data2015 <-  data.frame(storm_id, data2015)
data2015 <- data2015 %>%
  dplyr::filter(!is.na(storm_id)) %>%
  dplyr::mutate(ct_fips = sprintf("%03s", CZ_FIPS)) %>%
  tidyr::unite(fips, STATE_FIPS, ct_fips, sep = "") %>%
  plyr::rename(c(EVENT_TYPE = "type",
                 INJURIES_DIRECT = "direct_injuries",
                 INJURIES_INDIRECT = "indirect_injuries",
                 DEATHS_DIRECT = "direct_deaths",
                 DEATHS_INDIRECT = "indirect_deaths",
                 DAMAGE_PROPERTY = "damage_property",
                 DAMAGE_CROPS = "damage_crops")) %>%
  dplyr::select(fips, storm_id, type, direct_injuries, indirect_injuries,
                direct_deaths, indirect_deaths, damage_property, damage_crops)%>%
  dplyr::tbl_df()



########################################################################
#Get 2016 data
name = NULL
storm_id = NULL
storm_name <- c("Alex", "Bonnie", "Colin", "Danielle", "Earl", "Fiona", "Gaston", "Hermine", "Ian",
                "Julia", "Karl", "Lisa", "Matthew", "Nicole", "Otto", "Paula", "Richard", "Shary",
                "Tobias", "Virginie", "Walter")
path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/",find_file_name(2016))
temp <- tempfile()
download.file(path_name, temp)
data2016 <-   suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
unlink(temp)
for (i in 1:nrow(data2016)) {
  name = NULL
  for (j in 1:length(storm_name)) {
    name = c(name, ifelse(grepl(storm_name[j],data2016$EPISODE_NARRATIVE[i],fixed = T),
                          storm_name[j], NA))
    name = (name[!is.na(name)])[1]
  }
  storm_id = c(storm_id, name)
}
data2016 <-  data.frame(storm_id, data2016)
data2016 <- data2016 %>%
  dplyr::filter(!is.na(storm_id)) %>%
  dplyr::mutate(ct_fips = sprintf("%03s", CZ_FIPS)) %>%
  tidyr::unite(fips, STATE_FIPS, ct_fips, sep = "") %>%
  plyr::rename(c(EVENT_TYPE = "type",
                 INJURIES_DIRECT = "direct_injuries",
                 INJURIES_INDIRECT = "indirect_injuries",
                 DEATHS_DIRECT = "direct_deaths",
                 DEATHS_INDIRECT = "indirect_deaths",
                 DAMAGE_PROPERTY = "damage_property",
                 DAMAGE_CROPS = "damage_crops")) %>%
  dplyr::select(fips, storm_id, type, direct_injuries, indirect_injuries,
                direct_deaths, indirect_deaths, damage_property, damage_crops)%>%
  dplyr::tbl_df()


########################################################################
#Combine data
data2015$storm_id <- paste0(data2015$storm_id,"-2015")
data2016$storm_id <- paste0(data2016$storm_id,"-2016")
data15_16 <- rbind(data2015, data2016)

num.crops <- as.numeric(gsub("[^0-9]", "", data15_16$damage_crops))
letter.crops <- as.numeric(ifelse(grepl("K+", data15_16$damage_crops, perl=TRUE), 1000,
                                  ifelse(grepl("M+", data15_16$damage_crops, perl=TRUE), 1000000,
                                         ifelse(grepl("B+", data15_16$damage_crops, perl=TRUE), 1000000000,
                                                ifelse(grepl("0+", data15_16$damage_crops, perl=TRUE), 0, " ")))))
data15_16$damage_crops <- as.matrix(num.crops * letter.crops)


num.property = as.numeric(gsub("[^0-9]", "", data15_16$damage_property))
letter.property = as.numeric(ifelse(grepl("K+", data15_16$damage_property, perl=TRUE), 1000,
                                    ifelse(grepl("M+", data15_16$damage_property, perl=TRUE), 1000000,
                                           ifelse(grepl("B+", data15_16$damage_property, perl=TRUE), 1000000000,
                                                  ifelse(grepl("0+", data15_16$damage_property, perl=TRUE), 0, " ")))))
data15_16$damage_property <- as.matrix(num.property * letter.property)



combined.data <- rbind(combined_data,data15_16)
write.csv(combined.data, file = "Combined Data 1996-2016.csv")



