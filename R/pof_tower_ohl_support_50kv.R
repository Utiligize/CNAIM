#' @title Current Probability of Failure for Towers OHL support 50kV
#' @description This function calculates the current
#' annual probability of failure per kilometer EHV for Towers OHL support 50kV
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param number_of_operations Numeric Number of operations for the tower
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
#' @param foundation_type String. Foundation type of the tower
#' \code{foundation_type = c("Foundation - Fully Encased Concrete",
#'  "Foundation - Earth Grillage")}
#' @param paint_type String. Paint type of the tower
#' \code{foundation_type = c(Paint System - Galvanising, Paint System - Paint )}
#' @param observed_condition_inputs_steelwork Named list observed_conditions_input
#' @param observed_condition_inputs_paint Named list observed_conditions_input
#' @param observed_condition_inputs_foundation Named list observed_conditions_input
#' \code{conductor_samp = c("Low","Medium/Normal","High","Default")}.
#' @inheritParams current_health
#' @param k_value Numeric. \code{k_value = 0.0545} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = "Default"} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return DataFrame Current probability of failure
#' per annum per kilometer along with current health score.
#' @export
#' @examples
#' # Current annual probability of failure for Towers
#' pof_tower_ohl_support_50kv(
#' number_of_operations = "Default",
#' placement = "Default",
#' altitude_m = "Default",
#' distance_from_coast_km = "Default",
#' corrosion_category_index = "Default",
#' age = 10,
#' paint_type = "Paint System - Galvanising",
#' foundation_type = "Foundation - Earth Grillage",
#' observed_condition_inputs_steelwork =
#' list("tower_legs" = list("Condition Criteria: Observed Condition" = "Default"),
#' "tower_bracings" = list("Condition Criteria: Observed Condition" = "Default"),
#' "tower_crossarms" = list("Condition Criteria: Observed Condition" = "Default"),
#' "tower_peak" = list("Condition Criteria: Observed Condition" = "Default")),
#' observed_condition_inputs_paint =
#' list("paintwork_cond" = list("Condition Criteria: Observed Condition" = "Default")),
#' observed_condition_inputs_foundation =
#' list("foundation_cond" = list("Condition Criteria: Observed Condition" = "Default")),
#' reliability_factor = "Default",
#' k_value = 0.0545,
#' c_value = 1.087,
#' normal_expected_life = "Default")

pof_tower_ohl_support_50kv <-
  function(foundation_type = "Foundation - Fully Encased Concrete",
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
           reliability_factor = "Default",
           k_value = 0.0545,
           c_value = 1.087,
           normal_expected_life = "Default",
           gb_ref_given = NULL) {

    tower_asset_category <- "66kV Tower"
    `Asset Register Category` = `Health Index Asset Category` = `Sub-division` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check
    if(is.null(gb_ref_given)){
      gb_ref_taken <- gb_ref
    }else{
      check_gb_ref_given(gb_ref_given)
      gb_ref_taken <- gb_ref_given
    }

    asset_category <- gb_ref_taken$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      tower_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()

    # Normal expected life  -------------------------

    if (normal_expected_life == "Default") {
      normal_expected_life_steelwork <- gb_ref_taken$normal_expected_life %>%
        dplyr::filter(`Asset Register  Category` ==
                        tower_asset_category, `Sub-division` == "Steelwork") %>%
        dplyr::pull()

      normal_expected_life_foundation <- gb_ref_taken$normal_expected_life %>%
        dplyr::filter(`Asset Register  Category` ==
                        tower_asset_category, `Sub-division` == foundation_type) %>%
        dplyr::pull()

      normal_expected_life_paint <- gb_ref_taken$normal_expected_life %>%
        dplyr::filter(`Asset Register  Category` ==
                        tower_asset_category, `Sub-division` == paint_type) %>%
        dplyr::pull()
    } else {
      normal_expected_life_steelwork <- normal_expected_life
      normal_expected_life_foundation <- normal_expected_life
      normal_expected_life_paint <- normal_expected_life
    }

    # Constants C and K for PoF function --------------------------------------

    k <- k_value/100
    c <- c_value

    # Duty factor ----------------------------------------------------
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
                                                     "Tower Steelwork",
                                                     gb_ref_taken = gb_ref_taken)

    observed_condition_modifier_paint <-
      get_observed_conditions_modifier_hv_switchgear("EHV Towers",
                                                     oci_table_names_paint,
                                                     observed_condition_inputs_paint,
                                                     "Tower Paintwork",
                                                     gb_ref_taken = gb_ref_taken)

    observed_condition_modifier_foundation <-
      get_observed_conditions_modifier_hv_switchgear("EHV Towers",
                                                     oci_table_names_foundation,
                                                     observed_condition_inputs_foundation,
                                                     "Foundations",
                                                     gb_ref_taken = gb_ref_taken)

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


    return(data.frame(pof = probability_of_failure, chs = current_health_score))
  }
