library(testthat)
library(CNAIM)

context("POF Building")

test_that("POF Building", {

  res <- pof_building(substation_type = "Secondary",
                      material_type = "Wood",
                      placement = "Outdoor",
                      altitude_m = "Default",
                      distance_from_coast_km = "Default",
                      corrosion_category_index = "Default",
                      age = 43,
                      temperature_reading = "Default",
                      coolers_radiator = "Default",
                      kiosk = "Default",
                      cable_boxes = "Default",
                      reliability_factor = "Default",
                      k_value = "Default",
                      c_value = 1.087,
                      normal_expected_life_building = "Default")

  expected_val <- data.frame(pof = 0.010769, chs = 0.929352)

  expect_equal(res %>% round(6), expected_val)

})
