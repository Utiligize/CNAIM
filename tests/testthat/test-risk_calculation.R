library(testthat)
library(CNAIM)

context("Risk calculation")

test_that("Default case", {
  matrix_structure <- risk_matrix_structure(5,4,NA)

  risk_coordinates <- risk_calculation(matrix_dimensions = matrix_structure,
                                       id = "Transformer1",
                                       pof = 0.08,
                                       cof = 18232,
                                       pof_limits = c(0,1),
                                       cof_limits = c(0,1),
                                       asset_type = "6.6/11kV Transformer (GM)")

  # doing to avoid floating point errors during comparison
  risk_coordinates$point_y <- risk_coordinates$point_y %>% round(5)
  expected_df <- data.frame(id = "Transformer1", point_x = 54, point_y = 100)

  expect_equal(risk_coordinates, expected_df)
})




