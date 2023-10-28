library(dplyr)
library(tidycensus)
library(stringr)


county.fips <- tidycensus::fips_codes %>%
  mutate(fips = as.numeric(paste0(state_code,county_code, sep = "")),
         state = stringr::str_to_lower(state_name))

special_counties <- county.fips %>%
  filter(str_detect(county,"Borough")|str_detect(state_name,"Connecticut")&!str_detect(county, "County")|str_detect(state_name, "District of Columbia")|str_detect(county,"Baltimore city")|str_detect(county,"St. Louis city"))

county.fips <- county.fips %>%
      filter(!str_detect(county, "City")) %>%
      mutate(county = stringr::str_extract(county,
             pattern = ".{1,100}(?= County)|.{1,100}(?= Parish)|.{1,50}(?= Census Area)")) %>%
      bind_rows(special_counties) %>%
      mutate(county = stringr::str_to_lower(county)) %>%
      filter(!is.na(county)) %>%
  select(fips, state, county)

# usethis::use_data(county.fips, overwrite = TRUE)
