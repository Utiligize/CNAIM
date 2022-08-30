#' @importFrom magrittr %>%
#' @title Future Probability of Failure for 30kV and 60kV Switchgear
#' @description This function calculates the future
#' annual probability of failure 30kV and 60kV switchgear.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param asset_type String Asset Type
#' @param placement String Placement
#' @param number_of_operations String Number of Operations
#' @param altitude_m String Altitude
#' @param distance_from_coast_km String Distance from coast
#' @param corrosion_category_index String Corrosion Category Index
#' @param measured_condition_inputs Named list observed_conditions_input
#' @param observed_condition_inputs Named list observed_conditions_input
#' @param age Numeric Age
#' @param reliability_factor String Reliability Factor
#' @param k_value Numeric. \code{k_value = 0.0077} by default. This number is
#' given in a percentage. The default value is accordingly to the standard
#' "DE-10kV apb kabler CNAIM" on p. 34.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 55} by default.
#' The default value is accordingly to the standard
#' "DE-10kV apb kabler CNAIM" on p. 33.
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Current probability of failure
#' per annum.
#' @export
#' @examples
#' # Future annual probability of failure for 30kV and 60kV Swicthgear
#' pof_future_switchgear_30_60kv(
#' asset_type = "30kV",
#' number_of_operations = "Default",
#' placement = "Default",
#' altitude_m = "Default",
#' distance_from_coast_km = "Default",
#' corrosion_category_index = "Default",
#' age = 10,
#' observed_condition_inputs =
#' list("external_condition" =
#' list("Condition Criteria: Observed Condition" = "Default"),
#' "oil_gas" = list("Condition Criteria: Observed Condition" = "Default"),
#' "thermo_assment" = list("Condition Criteria: Observed Condition" = "Default"),
#' "internal_condition" = list("Condition Criteria: Observed Condition" = "Default"),
#' "indoor_env" = list("Condition Criteria: Observed Condition" = "Default"),
#' "support_structure" = list("Condition Criteria: Observed Condition" = "Default")),
#' measured_condition_inputs =
#' list("partial_discharge" =
#' list("Condition Criteria: Partial Discharge Test Results" = "Default"),
#' "ductor_test" = list("Condition Criteria: Ductor Test Results" = "Default"),
#' "oil_test" = list("Condition Criteria: Oil Test Results" = "Default"),
#' "temp_reading" = list("Condition Criteria: Temperature Readings" = "Default"),
#' "trip_test" = list("Condition Criteria: Trip Timing Test Result" = "Default"),
#' "ir_test" = list("Condition Criteria: IR Test Results" = "Default" )),
#' reliability_factor = "Default",
#' k_value = "Default",
#' c_value = 1.087,
#' normal_expected_life = 55,
#' simulation_end_year = 100)
pof_future_switchgear_30_60kv <-
  function(asset_type = "30kV",
           placement = "Default",
           number_of_operations = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           measured_condition_inputs,
           observed_condition_inputs,
           reliability_factor = "Default",
           k_value = "Default",
           c_value = 1.087,
           normal_expected_life = 55,
           simulation_end_year = 100,
           gb_ref_given = NULL){

    ehv_asset_category <- "33kV RMU"
    `Asset Register Category` = `Health Index Asset Category` =
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
                      ehv_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref_taken$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref_taken$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()



    # Constants C and K for PoF function --------------------------------------

    # POF function asset category.
    if (k_value == "Default" && asset_type == "30kV" ) {
      k <- 0.0223/100
    } else if (k_value == "Default" && asset_type == "60kV" ) {
      k <- 0.0512/100
    } else {
      k <- k_value/100
    }

    c <- c_value

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- get_duty_factor_hv_switchgear_primary(number_of_operations,
                                                              gb_ref_taken)

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
    mci_table_names <- list("partial_discharge" = "mci_ehv_swg_partial_discharge",
                            "ductor_test" = "mci_ehv_swg_ductor_test",
                            "oil_test" = "mci_ehv_swg_oil_tests_gas_test",
                            "temp_reading" = "mci_ehv_swg_temp_readings",
                            "trip_test" = "mci_ehv_swg_trip_test",
                            "ir_test"= "mci_ehv_swg_ir_test")

    measured_condition_modifier <-
      get_measured_conditions_modifier_hv_switchgear(asset_category,
                                                     mci_table_names,
                                                     measured_condition_inputs,
                                                     gb_ref_taken = gb_ref_taken)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- list("external_condition" = "oci_ehv_swg_swg_ext_cond",
                            "oil_gas" = "oci_ehv_swg_oil_leak_gas_pr",
                            "thermo_assment" = "oci_ehv_swg_thermo_assessment",
                            "internal_condition" = "oci_ehv_swg_swg_int_cond_ops",
                            "indoor_env" = "oci_ehv_swg_indoor_environ",
                            "support_structure" = "oci_ehv_swg_support_structure")

    observed_condition_modifier <-
      get_observed_conditions_modifier_hv_switchgear(asset_category,
                                                     oci_table_names,
                                                     observed_condition_inputs,
                                                     gb_ref_taken = gb_ref_taken)

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
    future_health_score_list <- list()
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
      future_health_score_list[[paste(y)]] <- future_health_Score
      pof_year[[paste(y)]] <- k * (1 + (c * H) +
                                     (((c * H)^2) / factorial(2)) +
                                     (((c * H)^3) / factorial(3)))
    }

    pof_future <- data.frame(
      year=year,
      PoF=as.numeric(unlist(pof_year)),
      future_health_score = as.numeric(unlist(future_health_score_list)))
    pof_future$age <- NA
    pof_future$age[1] <- age

    for(i in 2:nrow(pof_future)) {

      pof_future$age[i] <- age + i -1

    }

    return(pof_future)
  }
