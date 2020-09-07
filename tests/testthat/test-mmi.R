library(testthat)
library(CNAIM)

context("mmi")

# TODO: investigate NA handling
test_that("mmi", {
  # doc example
  expect_equal(mmi(c(1, 1.5),
                   factor_divider_1 = 1.5,
                   factor_divider_2 = 1.5,
                   max_no_combined_factors = 1), 1.5)

  # factors less than 1
  expect_equal(mmi(c(1, 1),
                   factor_divider_1 = 0,
                   factor_divider_2 = 1,
                   max_no_combined_factors = 0), 1)

  # factors less than 1
  expect_equal(mmi(c(0.75, 0.5),
                   factor_divider_1 = 0,
                   factor_divider_2 = 2,
                   max_no_combined_factors = 0), 0.375)

  # factors greater than 1
  expect_equal(mmi(c(1, 2, 3, 4, 5, 6),
                   factor_divider_1 = 2,
                   factor_divider_2 = 0,
                   max_no_combined_factors = 4), 10.5)

  expect_equal(mmi(c(1, 1, 1, 1, 5, 6),
                   factor_divider_1 = 2,
                   factor_divider_2 = 0,
                   max_no_combined_factors = 4), 8)

  expect_equal(mmi(c(9, 8, 7, 6, 5, 4),
                   factor_divider_1 = 5,
                   factor_divider_2 = 0,
                   max_no_combined_factors = 20), 14)

  expect_equal(mmi(c(9, 8, 7, 6, 5, 4),
                   factor_divider_1 = 5,
                   factor_divider_2 = 0,
                   max_no_combined_factors = 1), 9)

  expect_equal(mmi(c(9, NA, 3, 3, 3, 1),
                   factor_divider_1 = 6,
                   factor_divider_2 = 0,
                   max_no_combined_factors = 20), 10)

  expect_equal(mmi(c(9, NA, 1, 1, 1, 1),
                   factor_divider_1 = 6,
                   factor_divider_2 = 0,
                   max_no_combined_factors = 20), 9)
})
