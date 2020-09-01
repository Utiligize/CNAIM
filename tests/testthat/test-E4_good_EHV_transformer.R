library(testthat)
library(CNAIM)

test_that("E4_good_EHV_transformer", {
  # 5.
  lfactor = location_factor(placement = "Default",
                            altitude_m = 100,
                            distance_from_coast_km = 5,
                            corrosion_category_index = 3,
                            asset_type = "33kV Transformer (GM)")
  expect_equal(lfactor, 1.1)

  # 8.
  elife_transformer = expected_life(normal_expected_life = 60,
                                    duty_factor = 1.0,
                                    location_factor = lfactor)
  expect_equal(elife_transformer, 54.54545454545454)

  # 9.
  elife_tapchanger = expected_life(normal_expected_life = 60,
                                   duty_factor = 0.9,
                                   location_factor = lfactor)
  expect_equal(elife_tapchanger, 60.6060606060606)

  # 10.
  b1_transformer = beta_1(elife_transformer)
  expect_equal(b1_transformer, 0.0439614133346368)

  # 11.
  b1_tapchanger = beta_1(elife_tapchanger)
  expect_equal(b1_tapchanger, 0.039565272001173116)

  # 12.
  ih_transformer = initial_health(b1 = b1_transformer, age = 40)
  expect_equal(ih_transformer, 2.90173651)

  # 13.
  ih_tapchanger = initial_health(b1 = b1_tapchanger, age = 40)
  expect_equal(ih_tapchanger, 2.43382404)

  # 15.
  ch_transformer = current_health(ih_transformer,
                                  health_score_factor = 1)
  expect_equal(ch_transformer, 2.90173651)

  # 16.
  ch_tapchanger = current_health(ih_tapchanger,
                                  health_score_factor = 1)
  expect_equal(ch_tapchanger, 2.43382404)
})
