test_that("full data are fine", {
  job <- digest::digest(data_births_all)
  ref <- "ccf09ce5e99e0c5d505c5cdb880d0839"
  expect_equal(job, ref)
})

test_that("clean data are fine", {
  job <- digest::digest(filter_data(data_births_all))
  ref <- "6a2070372ee4aba35dcc048d1b4d93d7"
  expect_equal(job, ref)
})

test_that("expanded data are fine", {
  job <- digest::digest(expand_data(data_births_all))
  ref <- "77ad584cbf3ae846d860f614d50bb03b"
  expect_equal(job, ref)
})
