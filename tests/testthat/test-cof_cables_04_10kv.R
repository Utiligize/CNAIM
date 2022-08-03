library(testthat)
library(CNAIM)

context("COF:Financial Cables: 10 kV UG Cable Oil")

test_that("10kV UG Cable (Oil)", {
  res <- financial_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Oil)")

  expected_value <- 1123.59

  expect_equal(res, expected_value)
})

context("COF:Financial Cables: 10 kV UG Cable PEX")

test_that("10kV UG Cable (Non Pressurised)", {
  res <- round(financial_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Non Pressurised)"), digits=0)

  expected_value <- 275619

  expect_equal(res, expected_value)
})

context("COF:Financial Cables: 0.4 kV UG Cable PEX")

test_that("0.4kV UG Cable (Non Pressurised)", {
  res <- round(financial_cof_cables_04_10kv(hv_asset_category = "0.4kV UG Cable (Non Pressurised)"), digits=0)

  expected_value <- 275619

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Cables: 10 kV UG Cable Oil")

test_that("10kV UG Cable (Oil)", {
  res <- safety_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Oil)")

  expected_value <- 17.42

  expect_equal(res, expected_value)
})

context("COF:Safety Cables: 10 kV UG Cable PEX")

test_that("10kV UG Cable (Non Pressurised)", {
  res <- safety_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Non Pressurised)")

  expected_value <- 17.42

  expect_equal(res, expected_value)
})

context("COF:Safety Cables: 0.4 kV UG Cable PEX")

test_that("0.4kV UG Cable (Non Pressurised)", {
  res <- safety_cof_cables_04_10kv(hv_asset_category = "0.4kV UG Cable (Non Pressurised)")

  expected_value <- 17.42

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Env Cables: 10 kV UG Cable Oil")

test_that("10kV UG Cable (Oil)", {
  res <- round(environmental_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Oil)",
                                          prox_water = 95,
                                          bunded = "Yes"), digits=1)

  expected_value <- 25629.2

  expect_equal(res, expected_value)
})

context("COF:Env Cables: 10 kV UG Cable PEX")

test_that("10kV UG Cable (Non Pressurised", {
  res <- environmental_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Non Pressurised)",
                                          prox_water = 95,
                                          bunded = "Yes")

  expected_value <- 6323.46

  expect_equal(res, expected_value)
})

context("COF:Env Cables: 0.4 kV UG Cable PEX")

test_that("0.4kV UG Cable (Non Pressurised", {
  res <- environmental_cof_cables_04_10kv(hv_asset_category = "0.4kV UG Cable (Non Pressurised)",
                                          prox_water = 95,
                                          bunded = "Yes")

  expected_value <- 6323.46

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Network Cables: 10 kV UG Cable Oil")

test_that("10kV UG Cable (Oil)", {
  res <- network_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Oil)",
                                    actual_load_mva = 15, secure = T) %>% round(2)
  expected_value <- 43.55

  expect_equal(res, expected_value)
})

context("COF:Network Cables: 10 kV UG Cable PEX")

test_that("10kV UG Cable (Non Pressurised)", {
  res <- network_cof_cables_04_10kv(hv_asset_category = "10kV UG Cable (Non Pressurised)",
                                    actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 38432.88

  expect_equal(res, expected_value)
})


context("COF:Network Cables: 0.4 kV UG Cable PEX")

test_that("0.4kV UG Cable (Non Pressurised)", {
  res <- network_cof_cables_04_10kv(hv_asset_category = "0.4kV UG Cable (Non Pressurised)",
                                    actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 38432.88

  expect_equal(res, expected_value)
})
