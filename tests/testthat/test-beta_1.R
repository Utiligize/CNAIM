library(testthat)
library(CNAIM)

context("Beta 1")

test_that("beta_1", {
  expect_equal(beta_1(10), 0.239789527)
  expect_equal(beta_1(5), 0.479579055)
})
