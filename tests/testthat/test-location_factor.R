library(testthat)
library(CNAIM)

context("location factor")

test_that("location_factor", {
  # TODO: verify correctness
  expect_equal(location_factor(placement = "Default",
                               altitude_m = "Default",
                               distance_from_coast_km = "Default",
                               corrosion_category_index = "Default",
                               asset_type = "6.6/11kV Transformer (GM)"), 0.925)
})
