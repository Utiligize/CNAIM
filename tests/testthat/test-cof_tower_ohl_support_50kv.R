library(testthat)
library(CNAIM)

context("COF:Financial Tower: 50 kV")

test_that("50kV Tower", {
  res <- financial_cof_tower_ohl_support_50kv(type_financial_factor_criteria = "Suspension",
                                              access_factor_criteria = "Type A")

  expected_value <- 110155.4

  expect_equal(res, expected_value)
})



#-----------------------------------------

context("COF:Safety Tower: 50 kV")

test_that("50kV Tower", {
  res <- safety_cof_tower_ohl_support_50kv(location_risk = "Default",
                                           type_risk = "Default")
  expected_value <- 3283.67

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Tower: 50 kV")

test_that("50kV Tower", {
  res <- environmental_cof_tower_ohl_support_50kv()
  expected_value <- 1620.06

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Tower: 50 kV")

test_that("50kV Tower", {
  res <- network_cof_tower_ohl_support_50kv(actual_load_mva = 15)
  expected_value <- 9052.956

  expect_equal(res, expected_value)
})

