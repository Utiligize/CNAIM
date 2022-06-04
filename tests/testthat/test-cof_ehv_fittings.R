library(testthat)
library(CNAIM)

context("COF:Financial EHV Fittings")

test_that("33kV Fittings", {
  res <- financial_cof_ehv_fittings(ehv_asset_category = "33kV Fittings",
                                    type_financial_factor_criteria = "Tension",
                                    access_factor_criteria = "Type A")

  expected_value <- 454

  expect_equal(res, expected_value)
})


context("COF:Safety EHV Fittings")

test_that("33kV Fittings", {
  res <- safety_cof_ehv_fittings(ehv_asset_category = "33kV Fittings",
                                   location_risk = "Default",
                                   type_risk = "Default")
  expected_value <- 1508

  expect_equal(res, expected_value)
})


context("COF:Environmental EHV Fittings")

test_that("33kV Fittings", {
  res <- environmental_cof_ehv_fittings(ehv_asset_category = "33kV Fittings")
  expected_value <- 96

  expect_equal(res, expected_value)
})


context("COF:Network EHV Fittings")

test_that("33kV Fittings", {
  res <- network_cof_ehv_fittings(ehv_asset_category = "33kV Fittings",
                                  actual_load_mva = 15) %>% round(2)
  expected_value <- 333.75

  expect_equal(res, expected_value)
})

#-------------------------------132 KV section

context("COF:Financial 132kV Fittings")

test_that("132kV Fittings", {
  res <- financial_cof_ehv_fittings(ehv_asset_category = "132kV Fittings",
                                    type_financial_factor_criteria = "Tension",
                                    access_factor_criteria = "Type A")

  expected_value <- 970

  expect_equal(res, expected_value)
})


context("COF:Safety 132kV Fittings")

test_that("132kV Fittings", {
  res <- safety_cof_ehv_fittings(ehv_asset_category = "132kV Fittings",
                                 location_risk = "Default",
                                 type_risk = "Default")
  expected_value <- 1508

  expect_equal(res, expected_value)
})


context("COF:Environmental 132kV Fittings")

test_that("132kV Fittings", {
  res <- environmental_cof_ehv_fittings(ehv_asset_category = "132kV Fittings")
  expected_value <- 96

  expect_equal(res, expected_value)
})


context("COF:Network 132kV Fittings")

test_that("132kV Fittings", {
  res <- network_cof_ehv_fittings(ehv_asset_category = "132kV Fittings",
                                  actual_load_mva = 15) %>% round(2)
  expected_value <- 333.25

  expect_equal(res, expected_value)
})
