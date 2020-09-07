library(testthat)
library(CNAIM)

context("consequences of failure")

test_that("cof", {
  expect_equal(cof(1, 2, 3, 4), 10)
})
