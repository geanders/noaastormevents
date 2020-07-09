context("mapping events")

test_that("Plot returns ggplot object",{
  skip_on_cran()
  event_data <- find_events(date_range = c("1999-01-01", "1999-12-31"))
  p <- map_events(event_data, plot_type = "direct deaths")
  expect_type(p,"list")
})
