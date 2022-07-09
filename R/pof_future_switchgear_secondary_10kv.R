#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 10kV Switchgear Secondary
#' @description This function calculates the future
#' annual probability of failure 10kV switchgear secondary.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @inheritParams pof_ehv_fittings
#' @param k_value Numeric. \code{k_value = 0.0069} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 60} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric. Current probability of failure per annum.
#' @export
#' @examples
#'  # future annual probability of failure for 10kV Switchgear secondary
# pof_future_switchgear_secondary_10kv(
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
# "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default")),
# reliability_factor = "Default",
# k_value = 0.0067,
# c_value = 1.087,
# normal_expected_life = 55,
# simulation_end_year = 100)

pof_future_switchgear_secondary_10kv <-
  function(placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           k_value = 0.0067,
           c_value = 1.087,
           normal_expected_life = 55,
           simulation_end_year = 100) {

    hv_asset_category <- "6.6/11kV CB (GM) Secondary"

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
    k <- k_value/100
    c <- c_value

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

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
    mci_table_names <- list("partial_discharge" = "mci_hv_swg_distr_prtl_dischrg",
                            "ductor_test" = "mci_hv_swg_distr_ductor_test",
                            "oil_test" = "mci_hv_swg_distr_oil_test",
                            "temp_reading" = "mci_hv_swg_distr_temp_reading",
                            "trip_test" = "mci_hv_swg_distr_trip_test")

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear(asset_category,
                                                     mci_table_names,
                                                     measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list("external_condition" = "oci_hv_swg_dist_swg_ext_cond",
                            "oil_gas" = "oci_hv_swg_dist_oil_lek_gas_pr",
                            "thermo_assment" = "oci_hv_swg_dist_thermo_assment",
                            "internal_condition" = "oci_hv_swg_dist_swg_int_cond",
                            "indoor_env" = "oci_hv_swg_dist_indoor_environ")

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


    # Future probability of failure -------------------------------------------

    # the Health Score of a new asset
    H_new <- 0.5

    # the Health Score of the asset when it reaches its Expected Life
    b2 <- beta_2(current_health_score, age)
    print(b2)
    if (b2 > 2*b1){
      b2 <- b1*2
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
      } else if (H < 4) {
        H <- 4
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

