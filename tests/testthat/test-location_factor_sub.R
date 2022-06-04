library(testthat)
library(CNAIM)

context("Location factor for a non-landlocked submarine cable")

test_that("cof", {

  res <- location_factor_sub(topography = "Default",
                    situation = "Default",
                    wind_wave = "Default",
                    intensity = "Default",
                    landlocked = "no")

  expected_val <- 1.3

  expect_equal(res, expected_val)
})

