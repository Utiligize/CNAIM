library(testthat)
library(CNAIM)

context("Oil test modifier")

test_that("", {
  res <- oil_test_modifier(moisture = 15,
                           acidity = 0.15,
                           bd_strength = 30,
                           transformer_type_all = "20kV Transformer (GM)")

  expected_df <- data.frame(oil_condition_factor = 1.4,
                            oil_condition_cap = 10,
                            oil_condition_collar = 5.5)

  expect_equal(res, expected_df)
})
