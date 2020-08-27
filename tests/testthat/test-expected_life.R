library(testthat)
library(CNAIM)

context("expected life")

test_that("expected_life", {
  expect_equal(expected_life(1, 2, 5), 0.1)
})
