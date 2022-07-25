library(testthat)
library(CNAIM)

context("COF:Financial Switchgear 30/60 kV")

test_that("30kV", {
  res <- financial_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
                                          access_factor_criteria = "Type A")

  expected_value <- 126408.2

  expect_equal(res, expected_value)
})


# ----------------------------------------

context("COF:Safety Switchgear 30/60 kV")

test_that("30kV", {
  res <- safety_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
                                       location_risk = "Default",
                                       type_risk = "Default")
  expected_value <- 204702.4

  expect_equal(res, expected_value)
})


# ----------------------------------------

context("COF:Environmental Switchgear 30/60 kV")

test_that("30kV", {
  res <- environmental_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
                                              type_env_factor = "Oil",
                                              prox_water = 95,
                                              bunded = "Yes")
  expected_value <- 17642.45

  expect_equal(res, expected_value)
})

# ----------------------------------------

context("COF:Network Switchgear 30/60 kV")

test_that("30kV", {
  res <- network_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
                                        actual_load_mva = 15)
  expected_value <- 126817.6

  expect_equal(res, expected_value)
})


