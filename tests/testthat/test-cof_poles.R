library(testthat)
library(CNAIM)

context("COF:Financial Poles: EHV")

test_that("33kV Pole", {
  res <- financial_cof_poles(pole_asset_category = "33kV Pole",
                             type_financial_factor_criteria = "Small footprint steel masts",
                             access_factor_criteria = "Type A")

  expected_value <- 4106

  expect_equal(res, expected_value)
})

context("COF:Financial Poles: HV")

test_that("20kV Poles", {
  res <- financial_cof_poles(pole_asset_category = "20kV Poles",
                             type_financial_factor_criteria = "Small footprint steel masts",
                             access_factor_criteria = "Type A")

  expected_value <- 3820

  expect_equal(res, expected_value)
})


context("COF:Financial Poles: LV")

test_that("LV Poles", {
  res <- financial_cof_poles(pole_asset_category = "LV Poles",
                             type_financial_factor_criteria = "Steel Poles",
                             access_factor_criteria = "Type A")

  expected_value <- 2226

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Safety Poles: EHV")

test_that("33kV Pole", {
  res <- safety_cof_poles(pole_asset_category = "33kV Pole",
                          location_risk = "Default",
                          type_risk = "Default")
  expected_value <- 179

  expect_equal(res, expected_value)
})


context("COF:Safety Poles: HV")

test_that("20kV Poles", {
  res <- safety_cof_poles(pole_asset_category = "20kV Poles",
                          location_risk = "Default",
                          type_risk = "Default")
  expected_value <- 179

  expect_equal(res, expected_value)
})


context("COF:Safety Poles: LV")

test_that("LV Poles", {
  res <- safety_cof_poles(pole_asset_category = "LV Poles",
                          location_risk = "Default",
                          type_risk = "Default")
  expected_value <- 536

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Env Poles: EHV")

test_that("33kV pole", {
  res <- environmental_cof_poles(pole_asset_category = "33kV Pole")
  expected_value <- 75

  expect_equal(res, expected_value)
})


context("COF:Env Poles: HV")

test_that("20kV pole", {
  res <- environmental_cof_poles(pole_asset_category = "20kV Poles")
  expected_value <- 75

  expect_equal(res, expected_value)
})


context("COF:Env Poles: LV")

test_that("LV Poles", {
  res <- environmental_cof_poles(pole_asset_category = "LV Poles")
  expected_value <- 75

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Network Poles: LV")

test_that("LV Poles", {
  res <- network_cof_hv_lv_poles(pole_asset_category = "LV Poles",
                                 no_customers = 750,
                                 kva_per_customer = 51) %>% round(1)
  expected_value <- 285468.8

  expect_equal(res, expected_value)
})


context("COF:Network Poles: HV")

test_that("20kV Poles", {
  res <- network_cof_hv_lv_poles(pole_asset_category = "20kV Poles",
                                 no_customers = 750,
                                 kva_per_customer = 51) %>% round(2)
  expected_value <- 30398.44

  expect_equal(res, expected_value)
})


context("COF:Network Poles: EHV")

test_that("33kV Pole", {
  res <- network_cof_ehv_pole(pole_asset_category = "33kV Pole",
                              actual_load_mva = 15) %>% round(2)
  expected_value <- 95

  expect_equal(res, expected_value)
})
