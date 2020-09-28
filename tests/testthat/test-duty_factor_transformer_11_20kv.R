library(testthat)
library(CNAIM)

context("Duty Factor for 6.6/11kV and 20kV Transformer")

test_that("duty_factor_transformer_11_20kv", {
  expect_equal(duty_factor_transformer_11_20kv(), 1)
  expect_equal(duty_factor_transformer_11_20kv(45), 0.9)
  expect_equal(duty_factor_transformer_11_20kv(50), 0.9)
  expect_equal(duty_factor_transformer_11_20kv(65), 0.95)
  expect_equal(duty_factor_transformer_11_20kv(70), 0.95)
  expect_equal(duty_factor_transformer_11_20kv(75), 1)
  expect_equal(duty_factor_transformer_11_20kv(100), 1)
  expect_equal(duty_factor_transformer_11_20kv(1000), 1.4)
})
