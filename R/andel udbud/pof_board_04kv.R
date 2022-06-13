#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 0.4kV Board
#' @description This function calculates the current
#' annual probability of failure for 0.4kV Board
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param lv_asset_category String.
#' A sting that refers to the specific asset category.
#' @param lv_asset_category String The type of LV asset category
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor. A setting of \code{"Default"}
#' will set the altitude factor to 1 independent of \code{asset_type}.
#' @param distance_from_coast_km Numeric. Specify the distance from the
#' coast measured in kilometers. \code{distance_from_coast_km} is used
#' to derive the distance from coast factor.
#' A setting of \code{"Default"} will set the
#'  distance from coast factor to 1 independent of \code{asset_type}.
#' @param corrosion_category_index Integer.
#' Specify the corrosion index category, 1-5.
#' @param age  Numeric. The current age in years of the conductor.
#' @param measured_condition_inputs Named list observed_conditions_input
#' @param observed_condition_inputs Named list observed_conditions_input
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
#' @inheritParams current_health
#' @param k_value Numeric. \code{k_value = 0.0069} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 60} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @return Numeric. Current probability of failure
#' per annum per kilometer.
#' @export
#' @examples
#' # Current annual probability of failure for 0.4kV board
# pof_board_04kV_res <-
# pof_board_04kv(
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# observed_condition_inputs =
# list("external_cond" =
# list("Condition Criteria: Observed Condition" = "Default"),
# "compound_leaks" = list("Condition Criteria: Observed Condition" = "Default"),
# "internal_cond" = list("Condition Criteria: Observed Condition" = "Default"),
# "insulation" = list("Condition Criteria: Observed Condition" = "Default"),
# "signs_heating" = list("Condition Criteria: Observed Condition" = "Default"),
# "phase_barriers" = list("Condition Criteria: Observed Condition" = "Default")),
# measured_condition_inputs =
# list("opsal_adequacy" =
# list("Condition Criteria: Operational Adequacy" = "Default")),
# reliability_factor = "Default",
# k_value = 0.0069,
# c_value = 1.087,
# normal_expected_life = 60)*100
# paste0(sprintf("Probability of failure %.4f", pof_board_04kV_res),
# " percent per annum")

pof_board_04kv <-
  function(placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           k_value = 0.0069,
           c_value = 1.087,
           normal_expected_life = 60) {

    lv_asset_category <- "LV Board (WM)"
    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category`  = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      lv_asset_category) %>%
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
                                            asset_type = lv_asset_category)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    asset_category_mmi <- get_mmi_lv_switchgear_asset_category(lv_asset_category)

    # Measured conditions
    mci_table_names <- list("opsal_adequacy" = "mci_lv_board_wm_opsal_adequacy")

    measured_condition_modifier <-
      get_measured_conditions_modifier_lv_switchgear(asset_category_mmi,
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list(
      "external_cond" = "oci_lv_board_swg_ext_cond",
      "compound_leaks" = "oci_lv_board_wm_compound_leak",
      "internal_cond" = "oci_lv_board_wm_swg_int_cond",
      "insulation" = "oci_lv_board_wm_insulation_cond",
      "signs_heating" = "oci_lv_board_wm_signs_heating",
      "phase_barriers" = "oci_lv_board_wm_phase_barriers"
    )

    observed_condition_modifier <-
      get_observed_conditions_modifier_lv_switchgear(asset_category_mmi,
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


