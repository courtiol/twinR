test_that("full data are fine", {
  job <- digest::digest(data_births_all)
  ref <- "dbae070d28185106e600a52c1d106c73"
  expect_equal(job, ref)
})

test_that("clean data are fine", {
  job <- digest::digest(filter_data(data_births_all))
  ref <- "b99ad0fa1210ea20e36c8eec032014d0"
  expect_equal(job, ref)
})
