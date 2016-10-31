skip_on_cran()

context("finding filename from NOAA webpage")

test_that("Can find filename for 1999", {
  expect_equal(length(find_file_name(year = 1999)), 1)
})

