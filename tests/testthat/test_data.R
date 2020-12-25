test_that("full data are fine", {
  job <- digest::digest(data_births_all)
  ref <- "a8e32c014441c826829a7949a6d6a3dc"
  expect_equal(job, ref)
})

test_that("clean data are fine", {
  job <- digest::digest(filter_data(data_births_all))
  ref <- "eb526e54d01022a71024444f722a90dd"
  expect_equal(job, ref)
})
