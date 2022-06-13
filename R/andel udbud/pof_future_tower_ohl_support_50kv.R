#' @importFrom magrittr %>%
#' @title Future Probability of Failure for towers OHL support 50 kV
#' @description This function calculates the future
#' annual probability of failure per kilometer for a towers OHL support 50 kV.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams pof_tower_ohl_support_50kv
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric array. Future probability of failure
#' per annum per kilometre for Towers OHL support 50 kV.
#' @export
#' @examples
#' # Future annual probability of failure for Towers
# pof_future_tower_ohl_support_50kv(
# number_of_operations = "Default",
# placement = "Default",
# altitude_m = "Default",
# distance_from_coast_km = "Default",
# corrosion_category_index = "Default",
# age = 10,
# paint_type = "Paint System - Galvanising",
# foundation_type = "Foundation - Earth Grillage",
# observed_condition_inputs_steelwork =
# list("tower_legs" = list("Condition Criteria: Observed Condition" = "Default"),
# "tower_bracings" = list("Condition Criteria: Observed Condition" = "Default"),
# "tower_crossarms" = list("Condition Criteria: Observed Condition" = "Default"),
# "tower_peak" = list("Condition Criteria: Observed Condition" = "Default")),
# observed_condition_inputs_paint =
# list("paintwork_cond" = list("Condition Criteria: Observed Condition" = "Default")),
# observed_condition_inputs_foundation =
# list("foundation_cond" = list("Condition Criteria: Observed Condition" = "Default")),
# reliability_factor = "Default",
# k_value = 0.0545,
# c_value = 1.087,
# normal_expected_life = "Default",
# simulation_end_year = 100)


pof_future_tower_ohl_support_50kv <-
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
           simulation_end_year = 100) {

    tower_asset_category <- "66kV Tower"
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

    if (normal_expected_life == "Default") {
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
    # Future probability of failure -------------------------------------------

    b1 <- 0.1
    # the Health Score of a new asset
    H_new <- 0.5

    # the Health Score of the asset when it reaches its Expected Life
    b2 <- beta_2(current_health_score, age)

    if (b2 > 2*b1){
      b2 <- b1
    } else if (current_health_score == 0.5){
      b2 <- b1
    }

    if (current_health_score < 2) {
      ageing_reduction_factor <- 1
    } else if (current_health_score <= 5.5) {
      ageing_reduction_factor <- ((current_health_score - 2)/7) + 1
    } else {
      ageing_reduction_factor <- 1.5
    }

    # Dynamic part
    pof_year <- list()
    year <- seq(from=0,to=simulation_end_year,by=1)

    for (y in 1:length(year)){
      t <- year[y]

      future_health_Score <- current_health_score*exp((b2/ageing_reduction_factor) * t)

      H <- future_health_Score

      future_health_score_limit <- 15
      if (H > future_health_score_limit){
        H <- future_health_score_limit
      }

      pof_year[[paste(y)]] <- k * (1 + (c * H) +
                                     (((c * H)^2) / factorial(2)) +
                                     (((c * H)^3) / factorial(3)))
    }

    pof_future <- data.frame(year=year, PoF=as.numeric(unlist(pof_year)))
    pof_future$age <- NA
    pof_future$age[1] <- age

    for(i in 2:nrow(pof_future)) {

      pof_future$age[i] <- age + i -1

    }

    return(pof_future)
  }



