library(testthat)
library(CNAIM)

context("COF:Financial LV switch gear and others")

test_that("LV Board (WM)", {
  res <- financial_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
                                               type_financial_factor_criteria = "Asbestos clad",
                                               access_factor_criteria = "Type A")

  expected_value <- 6520

  expect_equal(res, expected_value)
})


context("COF:Safety LV switch gear and others")

test_that("LV Board (WM)", {
  res <- safety_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
                                            location_risk = "Default",
                                            type_risk = "Default")
  expected_value <- 8050

  expect_equal(res, expected_value)
})


context("COF:Environmental LV switch gear and others")

test_that("LV Board(WM)", {
  res <- environmental_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)")
  expected_value <- 18

  expect_equal(res, expected_value)
})


context("COF:Network LV switch gear and others")

test_that("LV Board(WM)", {
  res <- network_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
                                             no_customers = 750, kva_per_customer = 51) %>% round(1)
  expected_value <- 866906.2

  expect_equal(res, expected_value)
})
