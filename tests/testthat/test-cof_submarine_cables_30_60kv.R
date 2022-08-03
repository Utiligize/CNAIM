library(testthat)
library(CNAIM)

context("COF:Financial Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- financial_cof_submarine_cables_30_60kv() %>% round(0)
  expected_value <- 2485155

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Safety Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- safety_cof_submarine_cables_30_60kv()
  expected_value <- 17.42

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Env Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <- environmental_cof_submarine_30_60kv()
  expected_value <- 31356

  expect_equal(res, expected_value)
})


#-----------------------------------------

context("COF:Network Sub Cables: EHV")

test_that("EHV Sub Cable", {
  res <-  network_cof_submarine_cables_30_60kv(no_customers = 250,
                                               kva_per_customer = 51) %>% round(0)

  expected_value <- 12952314

  expect_equal(res, expected_value)
})

