context("finding filename from NOAA webpage")


test_that("Can find filename for 1999", {
  skip_on_cran()
  expect_silent(find_file_name(year = 1999, file_type = "fatalities"))
  })

test_that("Only one file name returned for 1999", {
  skip_on_cran()
  expect_equal(length(find_file_name(year = 1999)), 1)
})
