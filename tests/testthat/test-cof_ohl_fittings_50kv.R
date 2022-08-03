library(testthat)
library(CNAIM)

context("COF:Financial 50 kV OHL Fittings")

test_that("50kV Fittings", {
  res <- financial_cof_ohl_fittings_50kv(type_financial_factor_criteria = "Tension",
                                         access_factor_criteria = "Type A")

  expected_value <- 5086.64

  expect_equal(res, expected_value)
})


context("COF:Safety 50 kV OHL Fittings")

test_that("50kV Fittings", {
  res <- safety_cof_ohl_fittings_50kv(location_risk = "Default",
                                      type_risk = "Default")
  expected_value <- 13134.68

  expect_equal(res, expected_value)
})


context("COF:Environmental 50 kV OHL Fittings")

test_that("50kV Fittings", {
  res <- environmental_cof_ohl_fittings_50kv()
  expected_value <- 836.16

  expect_equal(res, expected_value)
})


context("COF:Network 50 kV OHL Fittings")

test_that("50kV Fittings", {
  res <- network_cof_ohl_fittings_50kv(actual_load_mva = 15) %>% round(2)
  expected_value <- 2901.52

  expect_equal(res, expected_value)
})

