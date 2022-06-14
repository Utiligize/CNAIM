#' @importFrom magrittr %>%
#' @title Current Probability of Failure for Poles OHL Support 50 kV
#' @description This function calculates the current
#' annual probability of failure per kilometer Poles
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param pole_asset_category String The type of asset category
#' @param sub_division String. Refers to material the pole is
#' made of. Options:
#' \code{sub_division = c("Concrete", "Steel", "Wood")}.
#' @param placement String. Specify if the asset is located outdoor or indoor.
#' @param altitude_m Numeric. Specify the altitude location for
#' the asset measured in meters from sea level.\code{altitude_m}
#' is used to derive the altitude factor.  A setting of \code{"Default"}
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
#' See page 161, table 199 and 201 in CNAIM (2021).
#' @inheritParams current_health
#' @param k_value Numeric. \code{k_value = 0.0285} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = "Default"} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @return Numeric. Current probability of failure
#' per annum per kilometer.
#' @export
#' @examples
#' # Current annual probability of failure for Poles OHL support 50 kV
# pof_poles_ohl_support_50kv_res <-
# pof_poles_ohl_support_50kv(
# sub_division = "Wood",
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# observed_condition_inputs =
# list("visual_pole_cond" =
# list("Condition Criteria: Pole Top Rot Present?" = "Default"),
# "pole_leaning" = list("Condition Criteria: Pole Leaning?" = "Default"),
# "bird_animal_damage" =
# list("Condition Criteria: Bird/Animal Damage?" = "Default"),
# "top_rot"  = list("Condition Criteria: Pole Top Rot Present?" = "Default")),
# measured_condition_inputs =
# list("pole_decay" =
# list("Condition Criteria: Degree of Decay/Deterioration" = "Default")),
# reliability_factor = "Default",
# k_value = 0.0285,
# c_value = 1.087,
# normal_expected_life = "Default") * 100
# paste0(sprintf("Probability of failure %.4f", pof_poles_ohl_support_50kv_res),
# " percent per annum")

pof_poles_ohl_support_50kv <-
  function(sub_division = "Wood",
           placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           k_value = 0.0285,
           c_value = 1.087,
           normal_expected_life = "Default") {

    pole_asset_category <- "66kV Pole"
    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = `Sub-division` = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      pole_asset_category) %>%
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
                      pole_asset_category,
                    `Sub-division` == sub_division) %>%
      dplyr::pull()

    if (normal_expected_life == "Default") {
      normal_expected_life_cond <- gb_ref$normal_expected_life %>%
        dplyr::filter(`Asset Register  Category` ==
                        pole_asset_category,
                      `Sub-division` == sub_division) %>%
        dplyr::pull()
    } else {
      normal_expected_life_cond <- normal_expected_life
    }

    # Constants C and K for PoF function --------------------------------------

    # POF function asset category.

    pof_asset_category <- "Poles"

    k <- k_value/100

    c <- c_value

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

    # Location factor ----------------------------------------------------
    location_factor_cond <- location_factor(placement,
                                            altitude_m,
                                            distance_from_coast_km,
                                            corrosion_category_index,
                                            asset_type = pole_asset_category,
                                            sub_division = sub_division)
    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life_cond,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Measured conditions
    # The table data is same for all poles category
    mci_table_names <- list("pole_decay" = "mci_ehv_pole_pole_decay_deter")

    # The table data is same for all poles category
    asset_category_mmi <- "EHV Poles"

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear(asset_category_mmi,
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    # The table data is same for all poles category
    oci_table_names <- list("visual_pole_cond" = "oci_ehv_pole_visual_pole_cond",
                            "pole_leaning" = "oci_ehv_pole_pole_leaning",
                            "bird_animal_damage" = "oci_ehv_pole_bird_animal_damag",
                            "top_rot" = "oci_ehv_pole_pole_top_rot")

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

