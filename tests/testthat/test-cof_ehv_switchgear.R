library(testthat)
library(CNAIM)

context("COF:Financial EHV Switchgear")

test_that("33kV RMU", {
  res <- financial_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                      access_factor_criteria = "Type A")

  expected_value <- 21099

  expect_equal(res, expected_value)
})

context("COF:Financial 132kV CB")

test_that("132kV CB (Air Insulated Busbars)(ID) (GM)", {
  res <- financial_cof_ehv_switchgear(ehv_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
                                      access_factor_criteria = "Type A")

  expected_value <- 67501

  expect_equal(res, expected_value)
})

# ----------------------------------------

context("COF:Safety EHV Switchgear")

test_that("33kV RMU", {
  res <- safety_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                   location_risk = "Default",
                                   type_risk = "Default")
  expected_value <- 20771

  expect_equal(res, expected_value)
})

context("COF:Safety 132kV CB")

test_that("132kV CB (Air Insulated Busbars)(ID) (GM)", {
  res <- safety_cof_ehv_switchgear(ehv_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
                                   location_risk = "Default",
                                   type_risk = "Default")
  expected_value <- 31968

  expect_equal(res, expected_value)
})


# ----------------------------------------

context("COF:Environmental EHV Switchgear")

test_that("33kV RMU", {
  res <- environmental_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                          type_env_factor = "Oil",
                                          prox_water = 95,
                                          bunded = "Yes")
  expected_value <- 1255.665

  expect_equal(res, expected_value)
})

context("COF:Environmental 132kV CB")

test_that("132kV CB (Air Insulated Busbars)(ID) (GM)", {
  res <- environmental_cof_ehv_switchgear(ehv_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
                                          type_env_factor = "Oil",
                                          prox_water = 95,
                                          bunded = "Yes")
  expected_value <- 3444.47

  expect_equal(res, expected_value)
})

# ----------------------------------------

context("COF:Network EHV switch gear")

test_that("33kV RMU", {
  res <- network_cof_ehv_switchgear(ehv_asset_category = "33kV RMU",
                                          actual_load_mva = 15)
  expected_value <- 6137

  expect_equal(res, expected_value)
})


context("COF:Network 132kV CB")

test_that("132kV CB (Air Insulated Busbars)(ID) (GM)", {
  res <- network_cof_ehv_switchgear(ehv_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
                                    actual_load_mva = 15) %>% round(2)
  expected_value <- 24023.62

  expect_equal(res, expected_value)
})
