library(testthat)
library(CNAIM)

context("Oil test modifier")

test_that("", {
  res <- oil_test_modifier(moisture = 15,
                           acidity = 0.15,
                           bd_strength = 30)

  expected_df <- data.frame(oil_condition_factor = 1.2,
                            oil_condition_cap = 10,
                            oil_condition_collar = 3)

  expect_equal(res, expected_df)
})
