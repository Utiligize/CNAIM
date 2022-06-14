#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 0.4kV Board
#' @description This function calculates the future
#' annual probability of failure per kilometer 0.4kV board.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams pof_switchgear_secondary
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric. Current probability of failure
#' per annum per kilometer.
#' @export
#' @examples
#' # Future annual probability of failure for 0.4kV board
# pof_future_board_04kv(
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
# normal_expected_life = 60,
# simulation_end_year = 100)

pof_future_board_04kv <-
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
           normal_expected_life = 60,
           simulation_end_year = 100) {

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

    # Future probability of failure -------------------------------------------

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

      future_health_Score <-
        current_health_score*exp((b2/ageing_reduction_factor) * t)

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

