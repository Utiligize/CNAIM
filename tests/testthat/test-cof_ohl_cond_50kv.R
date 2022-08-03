library(testthat)
library(CNAIM)

context("COF:Financial Overhead Line conductor: 50 kV")

test_that("50kV OHL (Tower Line) Conductor", {
  res <- financial_cof_ohl_cond_50kv(access_factor_criteria = "Type A")

  expected_value <- 205556

  expect_equal(res, expected_value)
})

#-----------------------------------------

context("COF:Safety Overhead Line conductor: 50 kV")

test_that("50kV OHL (Tower Line) Conductor", {
  res <- safety_cof_ohl_cond_50kv(location_risk = "Default",
                                  type_risk = "Default")
  expected_value <- 13134.68

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Overhead Line conductor: 50 kV")

test_that("50kV OHL (Tower Line) Conductor", {
  res <- environmental_cof_ohl_cond_50kv()
  expected_value <- 836.16

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Overhead Line conductor: 50 kV")

test_that("50kV OHL (Tower Line) Conductor", {
  res <- network_cof_ohl_cond_50kv(actual_load_mva = 15) %>% round(2)
  expected_value <- 14513.04

  expect_equal(res, expected_value)
})

