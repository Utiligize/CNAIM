library(testthat)
library(CNAIM)

context("COF:Financial Cables: EHV")

test_that("33kV UG Cable (Oil)", {
  res <- financial_cof_ehv_cables(ehv_asset_category = "33kV UG Cable (Oil)")

  expected_value <- 108

  expect_equal(res, expected_value)
})

context("COF:Financial Cables: 132 kV")

test_that("132kV UG Cable (Gas)", {
  res <- financial_cof_ehv_cables(ehv_asset_category = "132kV UG Cable (Gas)")

  expected_value <- 667

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Cables: EHV")

test_that("33kV UG Cable (Oil)", {
  res <- safety_cof_ehv_cables(ehv_asset_category = "33kV UG Cable (Oil)")

  expected_value <- 2

  expect_equal(res, expected_value)
})

context("COF:Safety Cables: 132 kV")

test_that("132kV UG Cable (Gas)", {
  res <- safety_cof_ehv_cables(ehv_asset_category = "132kV UG Cable (Gas)")

  expected_value <- 2

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Env Cables: EHV")

test_that("33kV UG Cable (Oil)", {
  res <- environmental_cof_ehv_cables(ehv_asset_category = "33kV UG Cable (Oil)",
                                      prox_water = 95,
                                      bunded = "Yes")

  expected_value <- 2449

  expect_equal(res, expected_value)
})

context("COF:Env Cables: 132 kV")

test_that("132kV UG Cable (Gas)", {
  res <- environmental_cof_ehv_cables(ehv_asset_category = "132kV UG Cable (Gas)",
                                      prox_water = 95,
                                      bunded = "Yes")

  expected_value <- 67

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Network Cables: EHV")

test_that("33kV UG Cable (Oil)", {
  res <- network_cof_ehv_cables(ehv_asset_category = "33kV UG Cable (Oil)",
                                 actual_load_mva = 15, secure = T) %>% round(2)
  expected_value <- 4.29

  expect_equal(res, expected_value)
})

context("COF:Network Cables: 132 kV")

test_that("132kV UG Cable (Gas)", {
  res <- network_cof_ehv_cables(ehv_asset_category = "132kV UG Cable (Gas)",
                                actual_load_mva = 15, secure = T) %>% round(2)

  expected_value <- 36.79

  expect_equal(res, expected_value)
})

