#' @title Current Probability of Failure for Towers
#' @description This function calculates the current
#' annual probability of failure per kilometer EHV Switchgear
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param tower_asset_category String The type of Tower asset category
#' @param number_of_operations Numeric Number of operations for the tower
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
#' @param foundation_type String Foundation type of the tower
#' @param paint_type String Paint type of the tower
#' @param observed_condition_inputs_steelwork Named list observed_conditions_input
#' @param observed_condition_inputs_paint Named list observed_conditions_input
#' @param observed_condition_inputs_foundation Named list observed_conditions_input
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
#' # Current annual probability of failure for Towers
#'pof_towers(
#'tower_asset_category = "33kV Tower",
#'number_of_operations = "Default",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'paint_type = "Paint System - Galvanising",
#'foundation_type = "Foundation - Earth Grillage",
#'observed_condition_inputs_steelwork =
#'list("tower_legs" = list("Condition Criteria: Observed Condition" = "Default"),
#'"tower_bracings" = list("Condition Criteria: Observed Condition" = "Default"),
#'"tower_crossarms" = list("Condition Criteria: Observed Condition" = "Default"),
#'"tower_peak" = list("Condition Criteria: Observed Condition" = "Default")),
#'observed_condition_inputs_paint =
#'list("paintwork_cond" = list("Condition Criteria: Observed Condition" = "Default")),
#'observed_condition_inputs_foundation =
#'list("foundation_cond" = list("Condition Criteria: Observed Condition" = "Default")),
#'reliability_factor = "Default")
pof_towers <-
  function(tower_asset_category = "33kV Tower",
           foundation_type = "Foundation - Fully Encased Concrete",
           paint_type = "Paint System - Paint",
           placement = "Default",
           number_of_operations = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           observed_condition_inputs_steelwork,
           observed_condition_inputs_paint,
           observed_condition_inputs_foundation,
           reliability_factor = "Default") {

    `Asset Register Category` = `Health Index Asset Category` = `Sub-division` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      tower_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  -------------------------
    normal_expected_life_steelwork <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` ==
                      tower_asset_category, `Sub-division` == "Steelwork") %>%
      dplyr::pull()

    normal_expected_life_foundation <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` ==
                      tower_asset_category, `Sub-division` == foundation_type) %>%
      dplyr::pull()

    normal_expected_life_paint <- gb_ref$normal_expected_life %>%
      dplyr::filter(`Asset Register  Category` ==
                      tower_asset_category, `Sub-division` == paint_type) %>%
      dplyr::pull()

    # Constants C and K for PoF function --------------------------------------

    # POF function asset category.

    pof_asset_category <- "Towers"

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% pof_asset_category) %>%
      dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% pof_asset_category) %>%
      dplyr::select(`C-Value`) %>%
      dplyr::pull()

    # Location factor ----------------------------------------------------
    duty_factor <- 1

    # Location factor ----------------------------------------------------
    location_factor <- location_factor(placement,
                                       altitude_m,
                                       distance_from_coast_km,
                                       corrosion_category_index,
                                       asset_type = tower_asset_category)


    # Expected life for structure ------------------------------
    expected_life_years_steelwork <- expected_life(normal_expected_life =
                                                     normal_expected_life_steelwork,
                                            duty_factor,
                                            location_factor)

    # Expected life for paint ------------------------------
    expected_life_years_foundation <- expected_life(normal_expected_life =
                                                     normal_expected_life_foundation,
                                                   duty_factor,
                                                   location_factor)

    # Expected life for paint ------------------------------
    expected_life_years_paint <- expected_life(normal_expected_life =
                                                      normal_expected_life_paint,
                                                    duty_factor,
                                                    location_factor)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1_steelwork <- beta_1(expected_life_years_steelwork)
    b1_foundation <- beta_1(expected_life_years_foundation)
    b1_paint <- beta_1(expected_life_years_paint)

    # Initial health score ----------------------------------------------------
    initial_health_score_steelwork <- initial_health(b1_steelwork, age)
    initial_health_score_foundation <- initial_health(b1_foundation, age)
    initial_health_score_paint <- initial_health(b1_paint, age)


    # Measured conditions
    measured_condition_modifier <- data.frame(condition_factor = 1,
                                              condition_cap = 10,
                                              condition_collar = 0.5)

    # Observed conditions -----------------------------------------------------

    # The table data is same for all towers category
    oci_table_names_steelwork <- list("tower_legs" = "oci_ehv_tower_tower_legs",
                            "tower_bracings" = "oci_ehv_tower_bracings",
                            "tower_crossarms" = "oci_ehv_tower_crossarms",
                            "tower_peak" = "oci_ehv_tower_peak")

    oci_table_names_paint <- list("paintwork_cond" = "oci_ehv_tower_paintwork_cond")

    oci_table_names_foundation <- list("foundation_cond" = "oci_ehv_tower_foundation_cond")

    observed_condition_modifier_steelwork <-
      get_observed_conditions_modifier_hv_switchgear("EHV Towers",
                                                     oci_table_names_steelwork,
                                                     observed_condition_inputs_steelwork,
                                                     "Tower Steelwork")

    observed_condition_modifier_paint <-
      get_observed_conditions_modifier_hv_switchgear("EHV Towers",
                                                     oci_table_names_paint,
                                                     observed_condition_inputs_paint,
                                                     "Tower Paintwork")

    observed_condition_modifier_foundation <-
      get_observed_conditions_modifier_hv_switchgear("EHV Towers",
                                                     oci_table_names_foundation,
                                                     observed_condition_inputs_foundation,
                                                     "Foundations")

    # Health score factor ---------------------------------------------------
    health_score_factor_steelwork <-
      health_score_excl_ehv_132kv_tf(observed_condition_modifier_steelwork$condition_factor,
                                     measured_condition_modifier$condition_factor)

    health_score_factor_paint <-
      health_score_excl_ehv_132kv_tf(observed_condition_modifier_paint$condition_factor,
                                     measured_condition_modifier$condition_factor)

    health_score_factor_foundation <-
      health_score_excl_ehv_132kv_tf(observed_condition_modifier_foundation$condition_factor,
                                     measured_condition_modifier$condition_factor)

    # Health score cap --------------------------------------------------------
    health_score_cap_steelwork <- min(observed_condition_modifier_steelwork$condition_cap,
                            measured_condition_modifier$condition_cap)

    health_score_cap_paint <- min(observed_condition_modifier_paint$condition_cap,
                            measured_condition_modifier$condition_cap)

    health_score_cap_foundation <- min(observed_condition_modifier_foundation$condition_cap,
                            measured_condition_modifier$condition_cap)

    # Health score collar -----------------------------------------------------
    health_score_collar_steelowrk <-  max(observed_condition_modifier_steelwork$condition_collar,
                                measured_condition_modifier$condition_collar)

    health_score_collar_paint <-  max(observed_condition_modifier_paint$condition_collar,
                                          measured_condition_modifier$condition_collar)

    health_score_collar_foundation <-  max(observed_condition_modifier_foundation$condition_collar,
                                      measured_condition_modifier$condition_collar)

    # Health score modifier ---------------------------------------------------
    health_score_modifier_steelwork <- data.frame(health_score_factor_steelwork,
                                        health_score_cap_steelwork,
                                        health_score_collar_steelowrk)

    health_score_modifier_paint <- data.frame(health_score_factor_paint,
                                                  health_score_cap_paint,
                                                  health_score_collar_paint)

    health_score_modifier_foundation <- data.frame(health_score_factor_foundation,
                                                  health_score_cap_foundation,
                                                  health_score_collar_foundation)

    # Current health score ----------------------------------------------------
    current_health_score_steelwork <-
      current_health(initial_health_score_steelwork,
                     health_score_modifier_steelwork$health_score_factor,
                     health_score_modifier_steelwork$health_score_cap,
                     health_score_modifier_steelwork$health_score_collar,
                     reliability_factor = reliability_factor)

    current_health_score_paint <-
      current_health(initial_health_score_paint,
                     health_score_modifier_paint$health_score_factor,
                     health_score_modifier_paint$health_score_cap,
                     health_score_modifier_paint$health_score_collar,
                     reliability_factor = reliability_factor)

    current_health_score_foundation <-
      current_health(initial_health_score_foundation,
                     health_score_modifier_foundation$health_score_factor,
                     health_score_modifier_foundation$health_score_cap,
                     health_score_modifier_foundation$health_score_collar,
                     reliability_factor = reliability_factor)

    current_health_score_paint <- min(current_health_score_paint, 6.4)

    current_health_score <- max(current_health_score_foundation,
                                current_health_score_steelwork,
                                current_health_score_paint)

    # Probability of failure ---------------------------------------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(probability_of_failure)
  }
