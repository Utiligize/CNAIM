library(testthat)
library(CNAIM)

context("COF:Financial switchgear secondary 10 kV")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- financial_cof_switchgear_secondary_10kv(access_factor_criteria = "Type A") %>% round(2)

  expected_value <- 60612.89

  expect_equal(res, expected_value)
})


context("COF:Safety switchgear secondary 10 kV")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- safety_cof_switchgear_secondary_10kv(location_risk = "Default",
                                              type_risk = "Default") %>% round(2)
  expected_value <- 42008.33

  expect_equal(res, expected_value)
})


context("COF:Environmental switchgear secondary 10 kV")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- environmental_cof_switchgear_secondary_10kv(type_env_factor = "Oil",
                                                     prox_water = 95,
                                                     bunded = "Yes") %>% round(2)
  expected_value <- 6342.10

  expect_equal(res, expected_value)
})


context("COF:Network switchgear secondary 10 kV")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- network_cof_switchgear_secondary_10kv(no_customers = 250,
                                               kva_per_customer = 51) %>% round(1)
  expected_value <- 787982.8

  expect_equal(res, expected_value)
})
