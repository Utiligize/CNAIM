library(testthat)
library(CNAIM)

context("COF:Financial Cables: Serviceline")

test_that("serviceline" , {
  res <- financial_cof_serviceline()

  expected_value <- 137809.6

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Cables: Serviceline")

test_that("serviceline", {
  res <- safety_cof_serviceline()

  expected_value <- 8.71

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Cables: Serviceline")

test_that("serviceline", {
  res <- environmental_cof_serviceline(prox_water = 95,
                                       bunded = "Yes")

  expected_value <- 3161.73

  expect_equal(res, expected_value)
})



#-----------------------------------------

context("COF:Network Cables: Service")

test_that("service", {
  res <- network_cof_serviceline(actual_load_mva = 0.5, secure = T) %>% round(2)

  expected_value <- 640.55

  expect_equal(res, expected_value)
})

