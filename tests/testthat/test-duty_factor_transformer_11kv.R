library(testthat)
library(CNAIM)

context("Duty Factor for a 6.6/11 kV Transformer")

test_that("duty_factor_transformer_11kv", {
  expect_equal(duty_factor_transformer_11kv(), 1)
  expect_equal(duty_factor_transformer_11kv(45), 0.9)
  expect_equal(duty_factor_transformer_11kv(50), 0.9)
  expect_equal(duty_factor_transformer_11kv(65), 0.95)
  expect_equal(duty_factor_transformer_11kv(70), 0.95)
  expect_equal(duty_factor_transformer_11kv(75), 1)
  expect_equal(duty_factor_transformer_11kv(100), 1)
  expect_equal(duty_factor_transformer_11kv(1000), 1.4)
})
