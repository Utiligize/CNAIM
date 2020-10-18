library(testthat)
library(CNAIM)

context("COF:Financial EHV Fittings")

test_that("33kV Fittings", {
  res <- financial_cof_ehv_fittings(ehv_asset_category = "33kV Fittings",
                                    type_financial_factor_criteria = "Tension",
                                    access_factor_criteria = "Type A")

  expected_value <- 378

  expect_equal(res, expected_value)
})


context("COF:Safety EHV Fittings")

test_that("33kV Fittings", {
  res <- safety_cof_ehv_fittings(ehv_asset_category = "33kV Fittings",
                                   location_risk = "Default",
                                   type_risk = "Default")
  expected_value <- 1336

  expect_equal(res, expected_value)
})


context("COF:Environmental EHV Fittings")

test_that("33kV Fittings", {
  res <- environmental_cof_ehv_fittings(ehv_asset_category = "33kV Fittings")
  expected_value <- 80

  expect_equal(res, expected_value)
})


context("COF:Network EHV Fittings")

test_that("33kV Fittings", {
  res <- network_cof_ehv_fittings(ehv_asset_category = "33kV Fittings",
                                  actual_load_mva = 15) %>% round(2)
  expected_value <- 278.33

  expect_equal(res, expected_value)
})
