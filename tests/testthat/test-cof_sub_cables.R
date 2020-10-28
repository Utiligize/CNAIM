library(testthat)
library(CNAIM)

context("COF:Financial Sub Cables: HV")

test_that("HV Sub Cable", {
  res <- financial_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
  expected_value <- 151492

  expect_equal(res, expected_value)
})

context("COF:Financial Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- financial_cof_sub_cables(sub_cable_asset_category = "EHV Sub Cable")
  expected_value <- 237500

  expect_equal(res, expected_value)
})


context("COF:Financial Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <- financial_cof_sub_cables(sub_cable_asset_category = "132kV Sub Cable")
  expected_value <- 400000

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Safety Sub Cables: HV")

test_that("HV Sub Cable", {
  res <- safety_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
  expected_value <- 2

  expect_equal(res, expected_value)
})


context("COF:Safety Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- safety_cof_sub_cables(sub_cable_asset_category = "EHV Sub Cable")
  expected_value <- 2

  expect_equal(res, expected_value)
})


context("COF:Safety Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <- safety_cof_sub_cables(sub_cable_asset_category = "132kV Sub Cable")
  expected_value <- 2

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Env Sub Cables: HV")

test_that("HV Sub Cable", {
  res <- environmental_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
  expected_value <- 3000

  expect_equal(res, expected_value)
})


context("COF:Env Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- environmental_cof_sub_cables(sub_cable_asset_category = "EHV Sub Cable")
  expected_value <- 3000

  expect_equal(res, expected_value)
})

context("COF:Env Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <- environmental_cof_sub_cables(sub_cable_asset_category = "132kV Sub Cable")
  expected_value <- 3000

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Network Sub Cables: HV")

test_that("HV Sub Cable", {
  res <- network_cof_hv_sub_cables(sub_cable_asset_category = "HV Sub Cable",
                                 no_customers = 750,
                                 kva_per_customer = 51) %>% round(1)
  expected_value <- 3764695.3

  expect_equal(res, expected_value)
})


context("COF:Network Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <-  network_cof_ehv_sub_cable(sub_cable_asset_category = "EHV Sub Cable",
                               actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 3674.29

  expect_equal(res, expected_value)
})

context("COF:Network Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <-  network_cof_ehv_sub_cable(sub_cable_asset_category = "132kV Sub Cable",
                                    actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 3673.93

  expect_equal(res, expected_value)
})

