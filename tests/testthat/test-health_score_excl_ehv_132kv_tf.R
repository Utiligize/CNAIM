library(testthat)
library(CNAIM)

context("Health Score Factor for all Assets")

test_that("health_score_excl_ehv_132kv_tf", {
  # a > 1, b > 1
  expect_equal(health_score_excl_ehv_132kv_tf(5, 4), 7)
  expect_equal(health_score_excl_ehv_132kv_tf(4, 5), 7)

  # a > 1, b <= 1
  expect_equal(health_score_excl_ehv_132kv_tf(5, 1), 5)
  expect_equal(health_score_excl_ehv_132kv_tf(1, 5), 5)
  expect_equal(health_score_excl_ehv_132kv_tf(5, 0.1), 5)
  expect_equal(health_score_excl_ehv_132kv_tf(0.1, 5), 5)

  # a <= 1, b <= 1
  expect_equal(health_score_excl_ehv_132kv_tf(1, 1), 1)
  expect_equal(health_score_excl_ehv_132kv_tf(1, 0.5), 0.5)
  expect_equal(health_score_excl_ehv_132kv_tf(0.5, 1), 0.5)
})
