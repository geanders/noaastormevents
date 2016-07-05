context("database format")

test_that("The format of the database remains unchanged",{
  file_name <- find_file_name(1996)
  path_name <- paste0("http://www1.ncdc.noaa.gov/pub/data/swdi/",
                      "stormevents/csvfiles/",file_name)
  temp <- tempfile()
  download.file(path_name, temp)
  data <-  suppressWarnings(read.csv(gzfile(temp), as.is = TRUE))
  unlink(temp)

  expect_is(data, "data.frame")
  expect_is(data$BEGIN_YEAR, "integer")
  expect_is(data$BEGIN_DAY, "integer")
  expect_is(data$BEGIN_TIME, "integer")
  expect_is(data$END_YEARMONTH, "integer")
  expect_is(data$END_DAY, "integer")
  expect_is(data$END_TIME, "integer")
  expect_is(data$EPISODE_ID, "integer")
  expect_is(data$EVENT_ID, "integer")
  expect_is(data$STATE, "character")
  expect_is(data$STATE_FIPS, "integer")
  expect_is(data$YEAR, "integer")
  expect_is(data$EVENT_TYPE, "character")
  expect_is(data$CZ_FIPS, "integer")
  expect_is(data$INJURIES_DIRECT, "integer")
  expect_is(data$INJURIES_INDIRECT, "integer")
  expect_is(data$DEATHS_DIRECT, "integer")
  expect_is(data$DEATHS_INDIRECT, "integer")
  expect_is(data$DAMAGE_PROPERTY, "character")
  expect_is(data$DAMAGE_CROPS, "character")

}
          )
