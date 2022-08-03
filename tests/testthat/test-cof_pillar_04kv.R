library(testthat)
library(CNAIM)

context("COF:Financial Pillar 0.4 kV")

test_that("LV Pillar (ID)", {
  res <- financial_cof_pillar_04kv(type_financial_factor_criteria = "Asbestos clad",
                                   access_factor_criteria = "Type A")

  expected_value <- 49376.99

  expect_equal(res, expected_value)
})


context("COF:Safety Pillar 0.4 kV")

test_that("LV Pillar (ID)", {
  res <- safety_cof_pillar_04kv(location_risk = "Default",
                                type_risk = "Default")
  expected_value <- 79339.39

  expect_equal(res, expected_value)
})


context("COF:Environmental Pillar 0.4 kV")

test_that("LV Pillar (ID)", {
  res <- environmental_cof_pillar_04kv()
  expected_value <- 191.62

  expect_equal(res, expected_value)
})


context("COF:Network Pillar 0.4 kV")

test_that("LV Pillar (ID)", {
  res <- network_cof_pillar_04kv(no_customers = 750,
                                 kva_per_customer = 51) %>% round(2)
  expected_value <- 6730924.69

  expect_equal(res, expected_value)
})
