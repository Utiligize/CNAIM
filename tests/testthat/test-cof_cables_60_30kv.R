library(testthat)
library(CNAIM)

context("COF:Financial Cables: 30 kV")

test_that("30kV UG Cable (Oil)", {
  res <- financial_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)")

  expected_value <- 1123.59

  expect_equal(res, expected_value)
})

context("COF:Financial Cables: 60 kV")

test_that("60kV UG Cable (Gas)", {
  res <- financial_cof_cables_60_30kv(ehv_asset_category = "60kV UG Cable (Gas)")

  expected_value <- 4520.49

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Cables: 30 kV")

test_that("30kV UG Cable (Oil)", {
  res <- safety_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)")

  expected_value <- 17.42

  expect_equal(res, expected_value)
})

context("COF:Safety Cables: 60 kV")

test_that("60kV UG Cable (Gas)", {
  res <- safety_cof_cables_60_30kv(ehv_asset_category = "60kV UG Cable (Gas)")

  expected_value <- 17.42

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Env Cables: 30 kV")

test_that("30kV UG Cable (Oil)", {
  res <- environmental_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)",
                                          prox_water = 95,
                                          bunded = "Yes") %>% round(0)

  expected_value <- 25629

  expect_equal(res, expected_value)
})

context("COF:Env Cables: 60 kV")

test_that("60kV UG Cable (Gas)", {
  res <- environmental_cof_cables_60_30kv(ehv_asset_category = "60kV UG Cable (Gas)",
                                          prox_water = 95,
                                          bunded = "Yes")

  expected_value <- 470.34

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Network Cables: 30 kV")

test_that("30kV UG Cable (Oil)", {
  res <- network_cof_cables_60_30kv(ehv_asset_category = "30kV UG Cable (Oil)",
                                    actual_load_mva = 15, secure = T) %>% round(2)
  expected_value <- 43.55

  expect_equal(res, expected_value)
})

context("COF:Network Cables: 60 kV")

test_that("60kV UG Cable (Gas)", {
  res <- network_cof_cables_60_30kv(ehv_asset_category = "60kV UG Cable (Gas)",
                                    actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 386.51

  expect_equal(res, expected_value)
})

