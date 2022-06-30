#' @importFrom magrittr %>%
#' @title Future Probability of Failure for Poles
#' @description This function calculates the future
#' annual probability of failure per kilometer for a poles.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 34 in CNAIM (2021).
#' @inheritParams pof_hv_poles
#' @param simulation_end_year Numeric. The last year of simulating probability
#'  of failure. Default is 100.
#' @return Numeric array. Future probability of failure
#' per annum per kilometre for poles.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Future annual probability of failure for HV Poles
# pof_future_poles(
# pole_asset_category = "20kV Poles",
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
# pole_decay = "Default",
# reliability_factor = "Default",
# simulation_end_year = 100)
pof_future_poles <-
  function(pole_asset_category = "20kV Poles",
           sub_division = "Wood",
           placement = "Default",
           altitude_m = "Default",
           distance_from_coast_km = "Default",
           corrosion_category_index = "Default",
           age,
           pole_decay = "default",
           observed_condition_inputs,
           reliability_factor = "Default",
           simulation_end_year = 100) {


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

    # Constants C and K for PoF function --------------------------------------

    # POF function asset category.

    pof_asset_category <- "Poles"

    k <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% pof_asset_category) %>%
      dplyr::select(`K-Value (%)`) %>%
      dplyr::pull()/100

    c <- gb_ref$pof_curve_parameters %>%
      dplyr::filter(`Functional Failure Category` %in% pof_asset_category) %>%
      dplyr::select(`C-Value`) %>%
      dplyr::pull()

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
    print(initial_health_score)
    # # Measured conditions
    # # The table data is same for all poles category
    # mci_table_names <- list("pole_decay" = "mci_ehv_pole_pole_decay_deter")

    # Pole decay ----------------------------------------------------
    mci_ehv_pole_pole_decay_deter <-
      gb_ref$mci_ehv_pole_pole_decay_deter

    ci_factor_pole_decay <-
      mci_ehv_pole_pole_decay_deter$`Condition Input Factor`[which(
        mci_ehv_pole_pole_decay_deter$
          `Condition Criteria: Degree of Decay/Deterioration` ==
          pole_decay)]

    ci_cap_pole_decay <-
      mci_ehv_pole_pole_decay_deter$`Condition Input Cap`[which(
        mci_ehv_pole_pole_decay_deter$
          `Condition Criteria: Degree of Decay/Deterioration` ==
          pole_decay)]

    ci_collar_pole_decay <-
      mci_ehv_pole_pole_decay_deter$`Condition Input Collar`[which(
        mci_ehv_pole_pole_decay_deter$
          `Condition Criteria: Degree of Decay/Deterioration` ==
          pole_decay)]

    # measured condition factor -----------------------------------------------
    measured_condition_factor <- ci_factor_pole_decay


    # The table data is same for all poles category
    asset_category_mmi <- "HV Poles"

    # Measured condition cap --------------------------------------------------
    measured_condition_cap <- ci_cap_pole_decay

    # Measured condition collar -----------------------------------------------
    measured_condition_collar <- ci_collar_pole_decay

    # Measured condition modifier ---------------------------------------------
    measured_condition_modifier <- data.frame(measured_condition_factor,
                                              measured_condition_cap,
                                              measured_condition_collar)


    # Observed conditions -----------------------------------------------------

    # The table data is same for all poles category
    oci_table_names <- list("visual_pole_cond" = "oci_hv_pole_visual_pole_cond",
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
                                     measured_condition_factor)
    print(observed_condition_modifier)
    print(measured_condition_factor)


    # Health score cap --------------------------------------------------------
    health_score_cap <- min(observed_condition_modifier$condition_cap,
                            measured_condition_cap)

    # Health score collar -----------------------------------------------------
    health_score_collar <-  max(observed_condition_modifier$condition_collar,
                                measured_condition_collar)

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
    print(current_health_score)
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


