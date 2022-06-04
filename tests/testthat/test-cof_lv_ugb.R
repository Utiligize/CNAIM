library(testthat)
library(CNAIM)

context("COF:Financial LV UGB")

test_that("LV UGB", {
  res <- financial_cof_lv_ugb(lv_asset_category = "LV UGB")

  expected_value <- 3429

  expect_equal(res, expected_value)
})


context("COF:Safety LV UGB")

test_that("LV UGB", {
  res <- safety_cof_lv_ugb(lv_asset_category = "LV UGB",
                                            location_risk = "Default",
                                            type_risk = "Default")
  expected_value <- 9622

  expect_equal(res, expected_value)
})


context("COF:Environmental LV UGB")

test_that("LV UGB", {
  res <- environmental_cof_lv_ugb(lv_asset_category = "LV UGB")
  expected_value <- 85

  expect_equal(res, expected_value)
})


context("COF:Network LV UGB")

test_that("LV UGB", {
  res <- network_cof_lv_ugb(lv_asset_category = "LV UGB",
                                             no_customers = 750, kva_per_customer = 51) %>% round(1)
  expected_value <- 644062.5

  expect_equal(res, expected_value)
})
