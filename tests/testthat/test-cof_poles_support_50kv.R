library(testthat)
library(CNAIM)

context("COF:Financial Poles: 50 kV")

test_that("50kV Pole", {
  res <- financial_cof_poles_ohl_support_50kv(
         type_financial_factor_criteria = "Small footprint steel masts",
         access_factor_criteria = "Type A")

  expected_value <- 64767.56

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Poles: 50 kV")

test_that("50kV Pole", {
  res <- safety_cof_poles_ohl_support_50kv(location_risk = "Default",
                                           type_risk = "Default")
  expected_value <- 1742

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Poles: 50 kV")

test_that("50kV pole", {
  res <- environmental_cof_poles_ohl_support_50kv()
  expected_value <- 783.9

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Poles: 50 kV")

test_that("50kV Pole", {
  res <- network_cof_poles_ohl_support_50kv(actual_load_mva = 15) %>% round(2)

  expected_value <- 996.21

  expect_equal(res, expected_value)
})
