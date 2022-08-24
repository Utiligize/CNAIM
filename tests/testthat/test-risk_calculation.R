library(testthat)
library(CNAIM)

context("Risk calculation")

test_that("Default case", {
  matrix_structure <- risk_matrix_structure(5,4,NA)

  risk_coordinates <- risk_calculation(matrix_dimensions = matrix_structure,
                                       id = 1,
                                       chs = 5.5,
                                       cof = 44542,
                                       ci_bands = NULL,
                                       hi_bands = NULL,
                                       asset_type = "6.6/11kV Transformer (GM)")

  # doing to avoid floating point errors during comparison
  risk_coordinates$point_y <- risk_coordinates$point_y %>% round(5)
  expected_df <- data.frame(id = 1, point_x = 50, point_y = 75)

  expect_equal(risk_coordinates, expected_df)
})




