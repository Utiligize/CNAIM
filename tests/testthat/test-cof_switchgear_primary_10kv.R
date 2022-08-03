library(testthat)
library(CNAIM)

context("COF:Financial switchgear primary 10 kV")

test_that("6.6/11kV CB (GM) Primary", {
  res <- financial_cof_switchgear_primary_10kv(access_factor_criteria = "Type A") %>% round(2)

  expected_value <- 66074.06

  expect_equal(res, expected_value)
})


context("COF:Safety switchgear primary 10 kV")

test_that("6.6/11kV CB (GM) Primary", {
  res <- safety_cof_switchgear_primary_10kv(location_risk = "Default",
                                            type_risk = "Default") %>% round(1)
  expected_value <- 204702.4

  expect_equal(res, expected_value)
})


context("COF:Environmental switchgear primary 10 kV")

test_that("6.6/11kV CB (GM) Primary", {
  res <- environmental_cof_switchgear_primary_10kv(type_env_factor = "Oil",
                                                   prox_water = 95,
                                                   bunded = "Yes") %>% round(2)
  expected_value <- 6535.07

  expect_equal(res, expected_value)
})


context("COF:Network switchgear primary 10 kV")

test_that("6.6/11kV CB (GM) Primary", {
  res <- network_cof_switchgear_primary_10kv(no_customers = 750,
                                             kva_per_customer = 51) %>% round(0)
  expected_value <- 6619056

  expect_equal(res, expected_value)
})
