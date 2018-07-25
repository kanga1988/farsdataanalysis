library(testthat)
library(maps)
library(farsdataanalysis)
test_that('R Mapping is Perfect', {
  map <- fars_map_state(12, 2014)
  expect_that(map, is_null())
})
