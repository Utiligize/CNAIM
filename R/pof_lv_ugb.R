#' @title Current Probability of Failure for LV UGB
#' @description This function calculates the current
#' annual probability of failure for LV UGB
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param lv_asset_category String.
#' A sting that refers to the specific asset category.
#' See See page 15, table 1 in CNAIM (2017).
#' @param lv_asset_category String The type of LV asset category
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
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 10kV OHL (Tower Line) Conductor
#'pof_lv_ugb(
#'lv_asset_category = "LV UGB",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'observed_condition_inputs =
#'list("steel_cover_and_pit_condition" =
#'list("Condition Criteria: Observed Condition" = "Default"),
#'"water_moisture" = list("Condition Criteria: Observed Condition" = "Default"),
#'"bell_cond" = list("Condition Criteria: Observed Condition" = "Default"),
#'"insulation_cond" = list("Condition Criteria: Observed Condition" = "Default"),
#'"signs_heating" = list("Condition Criteria: Observed Condition" = "Default"),
#'"phase_barriers" = list("Condition Criteria: Observed Condition" = "Default")),
#'measured_condition_inputs =
#'list("opsal_adequacy" =
#'list("Condition Criteria: Operational Adequacy" = "Default")),
#'reliability_factor = "Default")
pof_lv_ugb <-
  function(lv_asset_category = "LV UGB",
           placement = "Default",
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
                      lv_asset_category) %>%
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
                      lv_asset_category) %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% lv_asset_category) %>%
      dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% lv_asset_category) %>%
      dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = lv_asset_category)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cond,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Measured conditions
    mci_table_names <- list("opsal_adequacy" = "mci_lv_ugb_opsal_adequacy")

    measured_condition_modifier <-
      get_measured_conditions_modifier_lv_switchgear(lv_asset_category,
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list(
      "steel_cover_and_pit_condition" = "oci_lv_ugb_steel_covr_pit_cond",
      "water_moisture" = "oci_lv_ugb_water_moisture",
      "bell_cond" = "oci_lv_ugb_bell_cond",
      "insulation_cond" = "oci_lv_ugb_insulation_cond",
      "signs_heating" = "oci_lv_ugb_signs_heating",
      "phase_barriers" = "oci_lv_ugb_phase_barriers"
    )

    observed_condition_modifier <-
      get_observed_conditions_modifier_lv_switchgear(lv_asset_category,
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
