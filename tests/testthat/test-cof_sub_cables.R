library(testthat)
library(CNAIM)

context("COF:Financial Sub Cables: HV")

test_that("HV Sub Cable", {
  res <- financial_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
  expected_value <- 181996

  expect_equal(res, expected_value)
})

context("COF:Financial Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- financial_cof_sub_cables(sub_cable_asset_category = "EHV Sub Cable")
  expected_value <- 285322

  expect_equal(res, expected_value)
})


context("COF:Financial Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <- financial_cof_sub_cables(sub_cable_asset_category = "132kV Sub Cable")
  expected_value <- 480542

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
  expected_value <- 3600

  expect_equal(res, expected_value)
})


context("COF:Env Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- environmental_cof_sub_cables(sub_cable_asset_category = "EHV Sub Cable")
  expected_value <- 3600

  expect_equal(res, expected_value)
})

context("COF:Env Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <- environmental_cof_sub_cables(sub_cable_asset_category = "132kV Sub Cable")
  expected_value <- 3600

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Network Sub Cables: HV")

test_that("HV Sub Cable", {
  res <- network_cof_hv_sub_cables(sub_cable_asset_category = "HV Sub Cable",
                                 no_customers = 750,
                                 kva_per_customer = 51) %>% round(0)
  expected_value <- 4461188

  expect_equal(res, expected_value)
})


context("COF:Network Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <-  network_cof_ehv_sub_cable(sub_cable_asset_category = "EHV Sub Cable",
                               actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 4412.5

  expect_equal(res, expected_value)
})

context("COF:Network Sub Cables: 132kV")

test_that("132kV Sub Cable", {
  res <-  network_cof_ehv_sub_cable(sub_cable_asset_category = "132kV Sub Cable",
                                    actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 4412

  expect_equal(res, expected_value)
})

