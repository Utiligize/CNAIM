library(testthat)
library(CNAIM)

context("COF:Financial HV switch gear distribution")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- financial_cof_hv_switchgear_distribution(hv_asset_category = "6.6/11kV CB (GM) Secondary",
                                               access_factor_criteria = "Type A")

  expected_value <- 6959

  expect_equal(res, expected_value)
})


context("COF:Safety HV switch gear distribution")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- safety_cof_hv_switchgear_distribution(hv_asset_category = "6.6/11kV CB (GM) Secondary",
                                            location_risk = "Default",
                                            type_risk = "Default")
  expected_value <- 4823

  expect_equal(res, expected_value)
})


context("COF:Environmental HV switch gear distribution")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- environmental_cof_hv_switchgear_distribution(hv_asset_category = "6.6/11kV CB (GM) Secondary",
                                                      type_env_factor = "Oil",
                                                      prox_water = 95,
                                                      bunded = "Yes")
  expected_value <- 728.14

  expect_equal(res, expected_value)
})


context("COF:Network HV switch gear distribution")

test_that("6.6/11kV CB (GM) Secondary", {
  res <- network_cof_hv_switchgear_distribution(hv_asset_category = "6.6/11kV CB (GM) Secondary",
                                             no_customers = 750, kva_per_customer = 51) %>% round(1)
  expected_value <- 271406.2

  expect_equal(res, expected_value)
})
