library(testthat)
library(CNAIM)

context("matrix_adjusted_intervals")

test_that("Default case", {
  risk_data_matrix <- risk_matrix_structure(2,3,NA)
  set.seed(2)
  risk_data_matrix$value <- sample(1:30,size=nrow(risk_data_matrix),replace = T)

  y_intervals = c(0.25, 0.25, 0.25, 0.25)
  x_intervals = c(0.2, 0.2, 0.2, 0.2, 0.2)

  res <- matrix_adjusted_intervals(risk_data_matrix,x_intervals,y_intervals)

  expected_df <- data.frame(x = c(1,2,1,2,1,2),
                            y = c(1,1,2,2,3,3),
                            value = c(21, 15, 6, 6, 8, 17),
                            id = c(1,2,3,4,5,6),
                            x_intervals = c("0.2","0.2","0.2","0.2","0.2", "na"),
                            y_intervals = c("0.25","0.25","0.25","0.25","na", "na"))

  # Removing attributes for comparison
  attr(res, "out.attrs") <- NULL

  expect_equal(res, expected_df)
})


context("matrix_adjusted_circles")

test_that("Default case", {

  matrix_structure <- risk_matrix_structure(2,1,NA)

  risk_coordinates <- risk_calculation(matrix_dimensions = matrix_structure,
                                       id = 1,
                                       pof = 0.08,
                                       cof = 18232,
                                       pof_limits = c(0,1),
                                       cof_limits = c(0,1),
                                       asset_type = "6.6/11kV Transformer (GM)")
  res <- matrix_adjusted_circles(matrix_structure,
                          dots_vector = risk_coordinates,
                          dot_radius = 4)

  # Removing attributes for comparison
  attr(res, "out.attrs") <- NULL

  expected_df <- data.frame(x = c(1,2),
                            y = c(1,1),
                            value = c("na", "na"),
                            id = c(1,2),
                            point_x = c("54", "na"),
                            point_y = c("100", "na"),
                            dot_radius = c("4", "na"))

  expect_equal(res, expected_df)
})
