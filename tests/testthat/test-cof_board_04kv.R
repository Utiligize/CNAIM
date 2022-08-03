library(testthat)
library(CNAIM)

context("COF:Financial Board 0.4 kV")

test_that("LV Board (WM)", {
  res <- financial_cof_board_04kv(type_financial_factor_criteria = "Asbestos clad",
                                  access_factor_criteria = "Type A")

  expected_value <- 68225.43

  expect_equal(res, expected_value)
})


context("COF:Safety Board 0.4 kV")

test_that("LV Board (WM)", {
  res <- safety_cof_board_04kv(location_risk = "Default",
                               type_risk = "Default")
  expected_value <- 79339.39

  expect_equal(res, expected_value)
})


context("COF:Environmental Board 0.4 kV")

test_that("LV Board(WM)", {
  res <- environmental_cof_board_04kv()
  expected_value <- 191.62

  expect_equal(res, expected_value)
})


#something wrong with the network cof
context("COF:Network Board 0.4 kV")

test_that("LV Board(WM)", {
  res <- network_cof_board_04kv(no_customers = 750,
                                kva_per_customer = 51)
  expected_value <- 6730924.69 %>% round(2)

  expect_equal(res, expected_value)
})

