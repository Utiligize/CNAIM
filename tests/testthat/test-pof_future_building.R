library(testthat)
library(CNAIM)

context("Future Probability of Failure for Building")

test_that("pof_future_Building", {

  res <-   pof_future_building(substation_type = "Secondary",
     material_type = "Wood",
     placement = "Outdoor",
     altitude_m = "Default",
     distance_from_coast_km = "Default",
     corrosion_category_index = "Default",
     age = 1,
     temperature_reading = "Default",
     coolers_radiator = "Default",
     kiosk = "Default",
     cable_boxes = "Default",
     reliability_factor = "Default",
   k_value = "Default",
   c_value = 1.087,
   normal_expected_life_building = "Default",
   simulation_end_year = 100)



  expected_value <- readRDS(system.file("testdata/pof_future_building.rds", package =
                                          "CNAIM"))

  expect_equal(res, expected_value)

})
