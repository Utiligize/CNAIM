library(testthat)
library(CNAIM)

context("COF:Financial Tower: EHV")

test_that("33kV Tower", {
  res <- financial_cof_towers(tower_asset_category = "33kV Tower",
                             type_financial_factor_criteria = "Suspension",
                             access_factor_criteria = "Type A")

  expected_value <- 6749

  expect_equal(res, expected_value)
})


context("COF:Financial Tower: 132kV")

test_that("132kV Tower", {
  res <- financial_cof_towers(tower_asset_category = "132kV Tower",
                              type_financial_factor_criteria = "Suspension",
                              access_factor_criteria = "Type A")

  expected_value <- 14623

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Tower: EHV")

test_that("33kV Tower", {
  res <- safety_cof_towers(tower_asset_category = "33kV Tower",
                          location_risk = "Default",
                          type_risk = "Default")
  expected_value <- 377

  expect_equal(res, expected_value)
})


context("COF:Safety Tower: 132kV")

test_that("132kV Tower", {
  res <- safety_cof_towers(tower_asset_category = "132kV Tower",
                           location_risk = "Default",
                           type_risk = "Default")
  expected_value <- 377

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Tower: EHV")

test_that("33kV Tower", {
  res <- environmental_cof_towers(tower_asset_category = "33kV Tower")
  expected_value <- 186

  expect_equal(res, expected_value)
})


context("COF:Env Tower: 132kV")

test_that("132kV Tower", {
  res <- environmental_cof_towers(tower_asset_category = "132kV Tower")
  expected_value <- 186

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Tower: EHV")

test_that("33kV Tower", {
  res <- network_cof_tower(tower_asset_category = "33kV Tower",
                              actual_load_mva = 15) %>% round(2)
  expected_value <- 725

  expect_equal(res, expected_value)
})


test_that("132kV Tower", {
  res <- network_cof_tower(tower_asset_category = "132kV Tower",
                           actual_load_mva = 15) %>% round(2)
  expected_value <- 1039.25

  expect_equal(res, expected_value)
})
