#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 50kV Fittings
#' @description This function calculates the current
#' annual probability of failure per kilometer EHV Fittings
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param ehv_asset_category String The type of EHV asset category
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor. A setting of \code{"Default"}
#' will set the altitude factor to 1 independent of \code{asset_type}.
#' @param distance_from_coast_km Numeric. Specify the distance from the
#' coast measured in kilometers. \code{distance_from_coast_km} is used
#' to derive the distance from coast factor.
#'  A setting of \code{"Default"} will set the
#'  distance from coast factor to 1 independent of \code{asset_type}.
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @param age  Numeric. The current age in years of the conductor.
#' @param measured_condition_inputs Named list observed_conditions_input
#' @param observed_condition_inputs Named list observed_conditions_input
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
#' @inheritParams current_health
#' @param k_value Numeric. \code{k_value = 0.0096} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 40} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @return Numeric. Current probability of failure
#' per annum per kilometer.
#' @export
#' @examples
#' # Current annual probability of failure for 50kV fittings
# pof_ohl_fittings_50kV_res <-
# pof_ohl_fittings_50kv(
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# observed_condition_inputs =
# list("insulator_elec_cond" =
# list("Condition Criteria: Observed Condition" = "Default"),
# "insulator_mech_cond" =
# list("Condition Criteria: Observed Condition" = "Default"),
# "conductor_fitting_cond" =
# list("Condition Criteria: Observed Condition" = "Default"),
# "tower_fitting_cond" =
# list("Condition Criteria: Observed Condition" = "Default")),
# measured_condition_inputs =
# list("thermal_imaging" =
# list("Condition Criteria: Thermal Imaging Result" = "Default"),
# "ductor_test" = list("Condition Criteria: Ductor Test Result" = "Default")),
# reliability_factor = "Default",
# k_value = 0.0096,
# c_value = 1.087,
# normal_expected_life = 40) * 100
# paste0(sprintf("Probability of failure %.4f", pof_ohl_fittings_50kV_res),
# " percent per annum")

pof_ohl_fittings_50kv <-
  function(placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           k_value = 0.0096,
           c_value = 1.087,
           normal_expected_life = 40) {


    ehv_asset_category = "66kV Fittings"
    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      ehv_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()



    # Constants C and K for PoF function --------------------------------------

    k <- k_value/100

    c <- c_value

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = ehv_asset_category)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Measured conditions
    mci_table_names <- list("thermal_imaging" = "mci_ehv_fittings_thrml_imaging",
                            "ductor_test" = "mci_ehv_fittings_ductor_test")

    asset_category_mmi <- "EHV Fittings"

    if(ehv_asset_category == "132kV Fittings"){
      asset_category_mmi <- "132kV Fittings"
    }

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear(asset_category_mmi,
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list("insulator_elec_cond" = "oci_ehv_fitg_insltr_elect_cond",
                            "insulator_mech_cond" = "oci_ehv_fitg_insltr_mech_cond",
                            "conductor_fitting_cond" = "oci_ehv_cond_fitting_cond",
                            "tower_fitting_cond" = "oci_ehv_twr_fitting_cond")

    observed_condition_modifier <-
      get_observed_conditions_modifier_hv_switchgear(asset_category_mmi,
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

