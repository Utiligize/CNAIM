#' @importFrom magrittr %>%
#' @title Current Probability of Failure for 0.4-10kV OHL Conductors
#' @description This function calculates the current
#' annual probability of failure per kilometer 0.4-10kV OHL conductors.
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function. For more information about the
#' probability of failure function see section 6
#' on page 30 in CNAIM (2017).
#' @param lv_asset_category String.
#' A sting that refers to the specific asset category.
#' See See page 15, table 1 in CNAIM (2017).
#' Options:
#' \code{ohl_conductor = c("0.4kV OHL (Tower Line) Conductor",
#' "10kV OHL (Tower Line) Conductor")}.
#' The default setting is
#' \code{ohl_conductor = "10kV OHL (Tower Line) Conductor"}.
#' @inheritParams duty_factor_cables
#' @inheritParams location_factor
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
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 10kV OHL (Tower Line) Conductor
#'pof_lv_switchgear_and_other(
#'lv_asset_category = "LV Circuit Breaker",
#'placement = "Default",
#'altitude_m = "Default",
#'distance_from_coast_km = "Default",
#'corrosion_category_index = "Default",
#'age = 10,
#'observed_condition_inputs =list("external_condition" = list("Condition Criteria: Observed Condition" = "Default")),
#'measured_condition_inputs = list("operational_adequacy" = list("Condition Criteria: Operational Adequacy" = "Default")),
#'reliability_factor = "Default")

pof_lv_switchgear_and_other <-
  function(lv_asset_category = "LV Circuit Breaker",
           utilisation_pct = "Default",
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

    asset_category_mmi <- get_mmi_lv_switchgear_asset_category(lv_asset_category)

    # Measured conditions
    mci_table_names <- get_gb_ref_measured_conditions_table_names_lv_switchgear(asset_category_mmi)

    measured_condition_modifier <-
      get_measured_conditions_modifier_lv_switchgear(asset_category_mmi,
                                                    mci_table_names,
                                                    measured_condition_inputs)

    # Observed conditions -----------------------------------------------------

    oci_table_names <- get_gb_ref_observed_conditions_table_names_lv_switchgear(asset_category_mmi)

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


get_mmi_lv_switchgear_asset_category <- function(asset_category){
  if(grepl("LV Board", asset_category, fixed = T))
    return("LV Board (WM)")
  if(grepl("LV Pillar", asset_category, fixed = T))
    return("LV Pillars")
  if(grepl("LV Circuit Breaker", asset_category, fixed = T))
    return("LV Circuit Breaker")
}


get_gb_ref_measured_conditions_table_names_lv_switchgear <- function(asset_category_mmi){
  if(asset_category_mmi == "LV Board (WM)")
    return(list("operational_adequacy" = "mci_lv_board_wm_opsal_adequacy",
                "security" = "mci_lv_board_wm_security"))
  if(asset_category_mmi == "LV Pillars")
    return(list("operational_adequacy" = "mci_lv_pillar_opsal_adequacy"))
  if(asset_category_mmi == "LV Circuit Breaker")
    return(list("operational_adequacy" ="mci_lv_cb_opsal_adequacy"))
}


get_gb_ref_observed_conditions_table_names_lv_switchgear <- function(asset_category_mmi){
  if(asset_category_mmi == "LV Board (WM)")
    return(list("switchgear_external_condition" = "oci_lv_board_swg_ext_cond",
                "compound_leak" = "oci_lv_board_wm_compound_leak",
                "switchgear_internal_condition_and_operation" = "oci_lv_board_wm_swg_int_cond"))
  if(asset_category_mmi == "LV Pillars")
    return(list("compound_leak" = "oci_lv_pillar_compound_leak",
                "switchgear_external_condition" = "oci_lv_pillar_swg_ext_cond",
                "switchgear_internal_condition_and_operation" = "oci_lv_pillar_swg_int_cond_op",
                "insulation_condition" = "oci_lv_pillar_insulation_cond",
                "signs_heating" = "oci_lv_pillar_signs_heating",
                "phase_barrier" = "oci_lv_pillar_phase_barrier"
                ))
  if(asset_category_mmi == "LV Circuit Breaker")
    return(list("external_condition" ="oci_lv_circuit_breakr_ext_cond"))
}


get_measured_conditions_modifier_lv_switchgear <- function(asset_category_mmi, table_names, measured_condition_inputs){
  mcm_mmi_cal_df <-
    gb_ref$measured_cond_modifier_mmi_cal

  mcm_mmi_cal_df <-
    mcm_mmi_cal_df[which(
      mcm_mmi_cal_df$`Asset Category` == asset_category_mmi), ]

  factor_divider_1 <-
    as.numeric(
      mcm_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(
      mcm_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(
      mcm_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  # Measured inputs-----------------------------------------------------------
  factor_dfs <- list()
  for(table_name in names(table_names)){
    gb_ref_table_name <- table_names[[table_name]]
    mci_table <- gb_ref[[gb_ref_table_name]]
    mci_table_check_col_name <- names(measured_condition_inputs[[table_name]])[1]
    mci_table_check_col_val <- measured_condition_inputs[[table_name]][1]
    row_number <- which(mci_table[[mci_table_check_col_name]] ==
                          mci_table_check_col_val)
    factor_df <- mci_table[row_number,] %>%
      dplyr::select(c("Condition Input Factor", "Condition Input Cap",
                      "Condition Input Collar"))
    factor_dfs[[table_name]] <- factor_df
  }

  mci_factor_df <- factor_dfs %>% plyr::ldply()

  measured_condition_factor <- mmi(mci_factor_df[["Condition Input Factor"]],
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  measured_condition_cap <- min(mci_factor_df[["Condition Input Cap"]])

  measured_condition_collar <- max(mci_factor_df[["Condition Input Collar"]])

  # Measured condition modifier ---------------------------------------------
  measured_condition_modifier <- data.frame(condition_factor = measured_condition_factor,
                                            condition_cap = measured_condition_cap,
                                            condition_collar = measured_condition_collar)

  return(measured_condition_modifier)
}


get_observed_conditions_modifier_lv_switchgear <- function(asset_category_mmi, table_names, observed_condition_inputs){
  oci_mmi_cal_df <-
    gb_ref$observed_cond_modifier_mmi_cal

  oci_mmi_cal_df <-
    oci_mmi_cal_df[which(
      oci_mmi_cal_df$`Asset Category` == asset_category_mmi), ]

  factor_divider_1 <-
    as.numeric(
      oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 1`)

  factor_divider_2 <-
    as.numeric(
      oci_mmi_cal_df$`Parameters for Combination Using MMI Technique - Factor Divider 2`)

  max_no_combined_factors <-
    as.numeric(
      oci_mmi_cal_df$
        `Parameters for Combination Using MMI Technique - Max. No. of Combined Factors`
    )

  # Observed inputs-----------------------------------------------------------
  factor_dfs <- list()
  for(table_name in names(table_names)){
    gb_ref_table_name <- table_names[[table_name]]
    oci_table <- gb_ref[[gb_ref_table_name]]
    oci_table_check_col_name <- names(observed_condition_inputs[[table_name]])[1]
    oci_table_check_col_val <- observed_condition_inputs[[table_name]][1]
    row_number <- which(oci_table[[oci_table_check_col_name]] ==
                          oci_table_check_col_val)
    factor_df <- oci_table[row_number,] %>%
      dplyr::select(c("Condition Input Factor", "Condition Input Cap",
                      "Condition Input Collar"))
    factor_dfs[[table_name]] <- factor_df
  }

  oci_factor_df <- factor_dfs %>% plyr::ldply()

  observed_condition_factor <- mmi(oci_factor_df[["Condition Input Factor"]],
                                   factor_divider_1,
                                   factor_divider_2,
                                   max_no_combined_factors)

  observed_condition_cap <- min(oci_factor_df[["Condition Input Cap"]])

  observed_condition_collar <- max(oci_factor_df[["Condition Input Collar"]])

  # Observed condition modifier ---------------------------------------------
  observed_condition_modifier <- data.frame(condition_factor = observed_condition_factor,
                                            condition_cap = observed_condition_cap,
                                            condition_collar = observed_condition_collar)

  return(observed_condition_modifier)

}
