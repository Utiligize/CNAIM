library(testthat)
library(CNAIM)

context("Beta 2")

test_that("basic case", {
  res <- beta_2(current_health_score = 1, age = 25)
  expected_val <- 0.02772589

  expect_equal(res, expected_val)
})
