library(testthat)
library(CNAIM)

context("current health")

# TODO: test more parameter combinations
test_that("current_health", {
  expect_equal(current_health(2, 3), 6)
  expect_equal(current_health(2, 3, reliability_factor = 20), 20)
  expect_equal(current_health(200, 300), 10)
})
