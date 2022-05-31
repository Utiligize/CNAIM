#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 10 kV Switchgear (GM) Primary
#' @description This function calculates the current
#' annual probability of failure per kilometer 10 kV Switchgear (GM) Primary
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param hv_asset_category String The type of HV asset category
#' @param number_of_operations The number of operations for duty factor
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor. A setting of \code{"Default"}
#' will set the altitude factor to 1 independent of \code{asset_type}.
#' @param distance_from_coast_km Numeric. Specify the distance from the
#' coast measured in kilometers. \code{distance_from_coast_km} is used
#' to derive the distance from coast factor.
#' A setting of \code{"Default"} will set the
#' distance from coast factor to 1 independent of \code{asset_type}.
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @param age  Numeric. The current age in years of the conductor.
#' @param measured_condition_inputs Named list observed_conditions_input
#' @param observed_condition_inputs Named list observed_conditions_input
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
#' @inheritParams current_health
#' @param k_value Numeric. \code{k_value = 0.0052} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 55} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @return Numeric. Current probability of failure
#' per annum per kilometer.
#' @export
#' @examples
#' # Current annual probability of failure for 10 kV Switchgear (GM) Primary
# pof_switchgear_primary_10kv(
# number_of_operations = "Default",
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# observed_condition_inputs =
# list("external_condition" =
# list("Condition Criteria: Observed Condition" = "Default"),
# "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
# "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
# "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
# "indoor_env" = list("Condition Criteria: Observed Condition" = "Default")),
# measured_condition_inputs =
# list("partial_discharge" =
# list("Condition Criteria: Partial Discharge Test Results" = "Default"),
# "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
# "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
# "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
# "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default"),
# "ir_test" = list("Condition Criteria: IR Test Results" = "Default" )),
# reliability_factor = "Default",
# k_value = 0.0052,
# c_value = 1.087,
# normal_expected_life = 55)

pof_switchgear_primary_10kv <-
  function(placement = "Default",
           number_of_operations = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           k_value = 0.0052,
           c_value = 1.087,
           normal_expected_life = 55) {

    hv_asset_category <- "6.6/11kV CB (GM) Primary"
    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      hv_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()


    # Constants C and K for PoF function --------------------------------------

    # POF function asset category.

    k <- k_value/100

    c <- c_value
    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- get_duty_factor_hv_switchgear_primary(number_of_operations)

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = hv_asset_category)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Measured conditions
    mci_table_names <- list("partial_discharge" = "mci_hv_swg_pri_prtl_dischrg",
                            "ductor_test" = "mci_hv_swg_pri_ductor_test",
                            "oil_test" = "mci_hv_swg_pri_oil_tests",
                            "temp_reading" = "mci_hv_swg_pri_temp_reading",
                            "trip_test" = "mci_hv_swg_pri_trip_test",
                            "ir_test"= "mci_hv_swg_pri_ir_test")

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear(asset_category,
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list("external_condition" = "oci_hv_swg_pri_swg_ext",
                            "oil_gas" = "oci_hv_swg_pri_oil_leak_gas_pr",
                            "thermo_assment" = "oci_hv_swg_pri_thermo_assment",
                            "internal_condition" = "oci_hv_swg_pri_swg_int_cond_op",
                            "indoor_env" = "oci_hv_swg_pri_indoor_environ")

    observed_condition_modifier <-
      get_observed_conditions_modifier_hv_switchgear(asset_category,
                                                     oci_table_names,
                                                     observed_condition_inputs)

    # Health score factor ---------------------------------------------------
    health_score_factor <-
      health_score_excl_ehv_132kv_tf(observed_condition_modifier$condition_factor,
                                     measured_condition_modifier$condition_factor)

    # Health score cap --------------------------------------------------------
    health_score_cap <- min(observed_condition_modifier$condition_cap,
                            measured_condition_modifier$condition_cap)

    # Health score collar -----------------------------------------------------
    health_score_collar <-  max(observed_condition_modifier$condition_collar,
                                measured_condition_modifier$condition_collar)

    # Health score modifier ---------------------------------------------------
    health_score_modifier <- data.frame(health_score_factor,
                                        health_score_cap,
                                        health_score_collar)

    # Current health score ----------------------------------------------------
    current_health_score <-
      current_health(initial_health_score,
                     health_score_modifier$health_score_factor,
                     health_score_modifier$health_score_cap,
                     health_score_modifier$health_score_collar,
                     reliability_factor = reliability_factor)

    # Probability of failure ---------------------------------------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(probability_of_failure)
  }

