library(testthat)
library(CNAIM)


test_that("initial_health uncapped", {
  expect_equal(initial_health(1, 2), 3.694528)
})

test_that("initial_health capped", {
  expect_equal(initial_health(100, 200), 5.5)
})
