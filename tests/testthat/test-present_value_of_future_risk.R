library(testthat)
library(CNAIM)

context("Present value of Future Risk")

test_that("Regular Case", {

  res <- present_value_future_risk(c(0.1, 0.2, 0.5), 100) %>% round(2)
  expected_val <- 73.43

  expect_equal(res, expected_val)
})

