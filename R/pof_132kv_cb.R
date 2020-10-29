#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 132kV Switchgear
#' @description This function calculates the current
#' annual probability of failure per kilometer 132kV Switchgear
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param cb_asset_category String The type of 132kV asset category
#' @param number_of_operations The number of operations for duty factor
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor. See page 107,
#' table 23 in CNAIM (2017). A setting of \code{"Default"}
#' will set the altitude factor to 1 independent of \code{asset_type}.
#' @param distance_from_coast_km Numeric. Specify the distance from the
#' coast measured in kilometers. \code{distance_from_coast_km} is used
#' to derive the distance from coast factor See page 106,
#' table 22 in CNAIM (2017). A setting of \code{"Default"} will set the
#'  distance from coast factor to 1 independent of \code{asset_type}.
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @param age  Numeric. The current age in years of the conductor.
#' @param measured_condition_inputs Named list observed_conditions_input
#' @param observed_condition_inputs Named list observed_conditions_input
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
#' See page 146-147, table 192 and 194 in CNAIM (2017).
#' @inheritParams current_health
#' @return Numeric. Current probability of failure
#' per annum per kilometer.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for EHV Swicthgear
#'pof_132kv_cb(
#'cb_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
#'number_of_operations = "Default",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'observed_condition_inputs =
#'list("external_condition" =
#'list("Condition Criteria: Observed Condition" = "Default"),
#'"oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
#'"thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
#'"internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
#'"indoor_env" = list("Condition Criteria: Observed Condition" = "Default"),
#'"support_structure" = list("Condition Criteria: Observed Condition" = "Default"),
#'"air_systems" = list("Condition Criteria: Observed Condition" = "Default")),
#'measured_condition_inputs =
#'list("partial_discharge" =
#'list("Condition Criteria: Partial Discharge Test Results" = "Default"),
#'"ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
#'"oil_test" = list("Condition Criteria: Oil Test/ Gas Test Results" = "Default"),
#'"temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
#'"trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default"),
#'"ir_test" = list("Condition Criteria: IR Test Results" = "Default" )),
#'reliability_factor = "Default")

pof_132kv_cb <-
  function(cb_asset_category = "132kV CB (Air Insulated Busbars)(ID) (GM)",
           placement = "Default",
           number_of_operations = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default") {

    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      cb_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  -------------------------
    normal_expected_life_cond <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` ==
                      cb_asset_category) %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------

    # POF function asset category.

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% asset_category) %>%
      dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% asset_category) %>%
      dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- get_duty_factor_hv_switchgear_primary(number_of_operations)

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = cb_asset_category)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cond,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Measured conditions
    mci_table_names <- list("partial_discharge" = "mci_132kv_swg_partial_discharg",
                            "ductor_test" = "mci_132kv_swg_ductor_test",
                            "oil_test" = "mci_132kv_swg_oil_gas_test",
                            "temp_reading" = "mci_132kv_swg_temp_reading",
                            "trip_test" = "mci_132kv_swg_trip_test",
                            "ir_test"= "mci_132kv_swg_ir_test")

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear("132kV Switchgear (GM)",
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list("external_condition" = "oci_132kv_swg_swg_ext_cond",
                            "oil_gas" = "oci_132kv_swg_oil_leak_gas_pr",
                            "thermo_assment" = "oci_132kv_swg_thermo_assment",
                            "internal_condition" = "oci_132kv_swg_swg_int_cond_ops",
                            "indoor_env" = "oci_132kv_swg_indoor_environ",
                            "support_structure" = "oci_132kv_swg_support_structur",
                            "air_systems" = "oci_132kv_swg_air_systems")

    observed_condition_modifier <-
      get_observed_conditions_modifier_hv_switchgear("132kV Switchgear (GM)",
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

