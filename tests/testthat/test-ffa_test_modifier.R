library(testthat)
library(CNAIM)

context("FFA test modifier")

test_that("ffa_test_modifier", {
  res <- ffa_test_modifier(furfuraldehyde = 50)

  expected_df <- data.frame(ffa_test_factor = 1.6,
                            ffa_test_cap = 10,
                            ffa_test_collar = 7)

  expect_equal(res, expected_df)
})
