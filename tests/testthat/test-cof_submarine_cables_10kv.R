library(testthat)
library(CNAIM)

context("COF:Financial Sub Cables: 10kV")

test_that("HV Sub Cable", {
  res <- financial_cof_submarine_cables_10kv()
  expected_value <- 1585185

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Sub Cables: 10kV")

test_that("HV Sub Cable", {
  res <- safety_cof_submarine_cables_10kv()
  expected_value <- 17.42

  expect_equal(res, expected_value)
})



#-----------------------------------------

context("COF:Env Sub Cables: 10kV")

test_that("HV Sub Cable", {
  res <- environmental_cof_submarine_cables_10kv()
  expected_value <- 31356

  expect_equal(res, expected_value)
})



#-----------------------------------------

context("COF:Network Sub Cables: 10kV")

test_that("HV Sub Cable", {
  res <- network_cof_submarine_cables_10kv(no_customers = 250,
                                           kva_per_customer = 51) %>% round(0)
  expected_value <- 12952314

  expect_equal(res, expected_value)
})





