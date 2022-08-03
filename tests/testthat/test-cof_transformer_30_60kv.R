library(testthat)
library(CNAIM)

context("COF:Financial Transformers: 30/10kv and 60/10kv")

test_that("30kV Transformer (GM)", {
  res <- financial_cof_transformer_30_60kv(tf_asset_category = "30kV Transformer (GM)",
                                           type_financial_factor_kva_mva = 20,
                                           access_factor_criteria = "Type A") %>% round(0)

  expected_value <- 1291480

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Transformers: 30/10kv and 60/10kv")

test_that("30kV Transformer (GM)", {
  res <- safety_cof_transformer_30_60kv(tf_asset_category = "30kV Transformer (GM)",
                                        location_risk = "Default",
                                        type_risk = "Default") %>% round(1)
  expected_value <- 204702.4

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Transformers: 30/10kv and 60/10kv")

test_that("30kV Transformer (GM)", {
  res <- environmental_cof_transformer_30_60kv(tf_asset_category = "30kV Transformer (GM)",
                                               prox_water = 95,
                                               bunded = "Yes",
                                               size_kva_mva = 20) %>% round(1)

  expected_value <- 118790.5

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Transformers: 30/10kv and 60/10kv")

test_that("30kV Transformer", {
  res <- network_cof_transformer_30_60kv(tf_asset_category = "30kV Transformer (GM)",
                                         actual_load_mva = 15) %>% round(1)
  expected_value <- 252067.4

  expect_equal(res, expected_value)
})


