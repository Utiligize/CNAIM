library(testthat)
library(CNAIM)

context("Risk matrix structure")

test_that("basic case", {
  res <- risk_matrix_structure(2,2)

  expected_df <- expand.grid(x = 1:2, y = 1:2) %>% mutate(value = NA)

  expect_equal(res, expected_df)
})
