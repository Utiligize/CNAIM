library(testthat)
library(CNAIM)

context("COF:Financial Overhead Line conductor: EHV")

test_that("33kV OHL (Tower Line) Conductor", {
  res <- financial_cof_ohl_cond(ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor",
                              access_factor_criteria = "Type A")

  expected_value <- 14811

  expect_equal(res, expected_value)
})


context("COF:Financial Overhead Line conductor: 132kV")

test_that("132kV OHL (Tower Line) Conductor", {
  res <- financial_cof_ohl_cond(ohl_cond_asset_category = "132kV OHL (Tower Line) Conductor",
                              access_factor_criteria = "Type A")

  expected_value <- 16988

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Overhead Line conductor: EHV")

test_that("33kV OHL (Tower Line) Conductor", {
  res <- safety_cof_ohl_cond(ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor",
                           location_risk = "Default",
                           type_risk = "Default")
  expected_value <- 1336

  expect_equal(res, expected_value)
})


context("COF:Safety Overhead Line conductor: 132kV")

test_that("132kV OHL (Tower Line) Conductor", {
  res <- safety_cof_ohl_cond(ohl_cond_asset_category = "132kV OHL (Tower Line) Conductor",
                           location_risk = "Default",
                           type_risk = "Default")
  expected_value <- 1336

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Overhead Line conductor: EHV")

test_that("33kV OHL (Tower Line) Conductor", {
  res <- environmental_cof_ohl_cond(ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor")
  expected_value <- 80

  expect_equal(res, expected_value)
})


context("COF:Env Overhead Line conductor: 132kV")

test_that("132kV OHL (Tower Line) Conductor", {
  res <- environmental_cof_ohl_cond(ohl_cond_asset_category = "132kV OHL (Tower Line) Conductor")
  expected_value <- 80

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Overhead Line conductor: EHV")

test_that("33kV OHL (Tower Line) Conductor", {
  res <- network_cof_ohl_cond(ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor",
                           actual_load_mva = 15) %>% round(2)
  expected_value <- 1388.33

  expect_equal(res, expected_value)
})


test_that("132kV OHL (Tower Line) Conductor", {
  res <- network_cof_ohl_cond(ohl_cond_asset_category = "132kV OHL (Tower Line) Conductor",
                           actual_load_mva = 15) %>% round(2)
  expected_value <- 1387.92

  expect_equal(res, expected_value)
})
