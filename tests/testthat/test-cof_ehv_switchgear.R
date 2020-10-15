library(testthat)
library(CNAIM)

context("COF:Financial EHV Switchgear")

test_that("33kV RMU", {
  res <- financial_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                      access_factor_criteria = "Type A")

  expected_value <- 21099

  expect_equal(res, expected_value)
})


context("COF:Safety EHV Switchgear")

test_that("33kV RMU", {
  res <- safety_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                   location_risk = "Default",
                                   type_risk = "Default")
  expected_value <- 20771

  expect_equal(res, expected_value)
})


context("COF:Environmental EHV Switchgear")

test_that("33kV RMU", {
  res <- environmental_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                          type_env_factor = "Oil",
                                          prox_water = 95,
                                          bunded = "Yes")
  expected_value <- 1255.665

  expect_equal(res, expected_value)
})


context("COF:Network EHV switch gear")
