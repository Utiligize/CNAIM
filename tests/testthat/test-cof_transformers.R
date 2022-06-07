library(testthat)
library(CNAIM)

context("COF:Financial Transformers: EHV")

test_that("33kV Transformer (GM)", {
  res <- financial_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
   type_financial_factor_size = "33/20kV, CMR equivalent",
   type_financial_factor_kva_mva = 20,
   access_factor_criteria = "Type A")

  expected_value <- 168495

  expect_equal(res, expected_value)
})

context("COF:Financial Transformers: 132kV")

test_that("132kV Transformer (GM)", {
  res <- financial_cof_transformers(tf_asset_category = "132kV Transformer (GM)",
                                    type_financial_factor_size = "132/33kV",
                                    type_financial_factor_kva_mva = 20,
                                    access_factor_criteria = "Type A")

  expected_value <- 197038.8

  expect_equal(res, expected_value)
})


context("COF:Financial Transformers: 20kV")

test_that("20kV Transformer (GM)", {
  res <- financial_cof_transformers(tf_asset_category = "20kV Transformer (GM)",
                                    type_financial_factor_kva_mva = 20,
                                    access_factor_criteria = "Type A")

  expected_value <- 8997.25

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Safety Transformers: EHV")

test_that("33kV Transformer (GM)", {
  res <- safety_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
                                 location_risk = "Default",
                                 type_risk = "Default")
  expected_value <- 23502

  expect_equal(res, expected_value)
})


context("COF:Safety Transformers: 132kV")

test_that("132kV Transformer (GM)", {
  res <- safety_cof_transformers(tf_asset_category = "132kV Transformer (GM)",
                                 location_risk = "Default",
                                 type_risk = "Default")
  expected_value <- 36171

  expect_equal(res, expected_value)
})


context("COF:Safety Transformers: HV")

test_that("20kV Transformer (GM)", {
  res <- safety_cof_transformers(tf_asset_category = "20kV Transformer (GM)",
                                 location_risk = "Default",
                                 type_risk = "Default")
  expected_value <- 4823

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Env Transformers: EHV")

test_that("33kV Transformer (GM)", {
  res <- environmental_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
                                        prox_water = 95, bunded = "Yes", size_kva_mva = 20, size_conversion = "33/20kV")

  expected_value <- 13638.4

  expect_equal(res, expected_value)
})


context("COF:Env Transformers: 132kV")

test_that("132kV Transformer (GM)", {
  res <- environmental_cof_transformers(tf_asset_category = "132kV Transformer (GM)",
                                        prox_water = 95, bunded = "Yes", size_kva_mva = 20,
                                        size_conversion = "132/33kV")

  expected_value <- 14038

  expect_equal(res, expected_value)
})


context("COF:Env Transformers: HV")

test_that("20kV Transformer (GM)", {
  res <- environmental_cof_transformers(tf_asset_category = "20kV Transformer (GM)",
                                        prox_water = 95, bunded = "Yes", size_kva_mva = 20)

  expected_value <- 1142.7

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Transformers: EHV")

test_that("33kV Transformer", {
  res <- network_cof_transformers(tf_asset_category = "33kV Transformer (GM)",
                                  actual_load_mva = 15) %>% round(1)
  expected_value <- 28940

  expect_equal(res, expected_value)
})


context("COF:Network Transformers: 132kV")

test_that("132kV transformer", {
  res <- network_cof_transformers(tf_asset_category = "132kV Transformer (GM)",
                                  actual_load_mva = 75) %>% round(1)

  expected_value <- 288051.2

  expect_equal(res, expected_value)
})
