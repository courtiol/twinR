test_that("full data are fine", {
  job <- digest::digest(data_births_all)
  ref <- "d15966ed5666ba27a1351506ae9309f6"
  expect_equal(job, ref)
})

test_that("clean data are fine", {
  job <- digest::digest(filter_data(data_births_all))
  ref <- "a2b59696cf81876935b1919856c3bb33"
  expect_equal(job, ref)
})
