library(testthat)
library(CNAIM)

context("COF:Financial Relay")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- financial_cof_relay(access_factor_criteria = "Type A")

  expected_value <- 30306.45

  expect_equal(res, expected_value)
})


# ----------------------------------------

context("COF:Safety Relay")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- safety_cof_relay(location_risk = "Default",
                          type_risk = "Default")

  expected_value <- 21004.17

  expect_equal(res, expected_value)
})


# ----------------------------------------

context("COF:Environmental Relay")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- environmental_cof_relay(type_env_factor = "Oil",
                                 prox_water = 95,
                                 bunded = "Yes")
  expected_value <- 3171.05

  expect_equal(res, expected_value)
})

# ----------------------------------------

context("COF:Network Relay")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- network_cof_relay(no_customers = 100, kva_per_customer = 40)

  expected_value <- 6303.863

  expect_equal(res, expected_value)
})
