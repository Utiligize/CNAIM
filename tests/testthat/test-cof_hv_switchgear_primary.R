library(testthat)
library(CNAIM)

context("COF:Financial HV switch gear primary")

test_that("6.6/11kV CB (GM) Primary", {
  res <- financial_cof_hv_switchgear_primary(hv_asset_category = "6.6/11kV CB (GM) Primary",
                                             access_factor_criteria = "Type A")

  expected_value <- 6315

  expect_equal(res, expected_value)
})


context("COF:Safety HV switch gear primary")

test_that("6.6/11kV CB (GM) Primary", {
  res <- safety_cof_hv_switchgear_primary(hv_asset_category = "6.6/11kV CB (GM) Primary",
                                          location_risk = "Default",
                                          type_risk = "Default")
  expected_value <- 20771

  expect_equal(res, expected_value)
})


context("COF:Environmental HV switch gear primary")

test_that("6.6/11kV CB (GM) Primary", {
  res <- environmental_cof_hv_switchgear_primary(hv_asset_category = "6.6/11kV CB (GM) Primary",
                                                 type_env_factor = "Oil",
                                                 prox_water = 95,
                                                 bunded = "Yes")
  expected_value <- 536.27

  expect_equal(res, expected_value)
})


context("COF:Network HV switch gear primary")

test_that("6.6/11kV CB (GM) Primary", {
  res <- network_cof_hv_switchgear_primary(hv_asset_category = "6.6/11kV CB (GM) Primary",
                                           no_customers = 750, kva_per_customer = 51) %>% round(1)
  expected_value <- 182343.8

  expect_equal(res, expected_value)
})
