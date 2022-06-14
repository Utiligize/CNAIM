#' @title Financial cost of Failure for 30kV and 60kV Switchgear
#' @description This function calculates financial consequences of failure
#' Financial consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param ehv_asset_category String The type of 30kV and 60kV switchgear
#' @param access_factor_criteria String. Asses Financial factor criteria for 30kV and 60kV switchgear
#' setting
#' @return Numeric. Financial consequences of failure for 30kV and 60kV switchgear
#' @export
#' @examples
#' financial_cof_switchgear_30_60kv(ehv_asset_category = "30kV", access_factor_criteria = "Type A")
financial_cof_switchgear_30_60kv <- function(ehv_asset_category, access_factor_criteria){

  GBP_to_DKK <- 8.71
  if (ehv_asset_category == "30kV" ) {
    ehv_asset_category <- "33kV CB (Air Insulated Busbars)(ID) (GM)"
  } else if (ehv_asset_category == "60kV" ) {
    ehv_asset_category <- "66kV CB (Air Insulated Busbars)(ID) (GM)"
  } else {
    ehv_asset_category <- NULL
    warning("Wrong input, please go to CNAIM.io for more documentation")
  }
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ehv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factor <- 1

  # Access financial factor -------------------------------------------------
  access_financial_factor_category <- "EHV Switchgear (GM)"
  if("132kV" %in%  ehv_asset_category){
    access_financial_factor_category <- "132kV CBs"
  }

  access_financial_factors <- gb_ref$access_factor_swg_tf_asset

  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 access_financial_factor_category)

  if (access_factor_criteria == "Type A") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (access_factor_criteria == "Type B") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }
  else if (access_factor_criteria == "Type C") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type C Criteria - Underground substation`
  }


  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_finacial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost * GBP_to_DKK)
}


#' @title Safety cost of Failure for 30kV and 60kV Switchgear
#' @description This function calculates safety consequences of failure
#' Safety consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param ehv_asset_category String The type of 30kV and 60kV switchgear
#' @param location_risk String Type Financial factor criteria for 30kV and 60kV switchgear
#' @param type_risk String. Asses Financial factor criteria for 30kV and 60kV switchgear
#' setting
#' @return Numeric. Financial consequences of failure for 30kV and 60kV switchgear
#' @export
#' @examples
#' safety_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_switchgear_30_60kv <- function(ehv_asset_category,
                                          location_risk,
                                          type_risk){
  GBP_to_DKK <- 8.71
  if (ehv_asset_category == "30kV" ) {
    ehv_asset_category <- "33kV CB (Air Insulated Busbars)(ID) (GM)"
  } else if (ehv_asset_category == "60kV" ) {
    ehv_asset_category <- "66kV CB (Air Insulated Busbars)(ID) (GM)"
  } else {
    ehv_asset_category <- NULL
    warning("Wrong input, please go to CNAIM.io for more documentation")
  }
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ehv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`

  if (location_risk == "Default") location_risk <- "Medium (Default)"
  if (location_risk == "Medium") location_risk <- "Medium (Default)"
  if (type_risk == "Default") type_risk <- "Medium"

  safety_conseq_factor_sg_tf_oh <- gb_ref$safety_conseq_factor_sg_tf_oh

  row_no <- which(safety_conseq_factor_sg_tf_oh$
                    `Safety Consequence Factor - Switchgear, Transformers & Overhead Lines...2` ==
                    location_risk)

  col_no <- grep(type_risk, colnames(safety_conseq_factor_sg_tf_oh))

  safety_consequence_factor <- safety_conseq_factor_sg_tf_oh[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof * GBP_to_DKK)
}


#' @title Environmental cost of Failure for 30kV and 60kV Switchgear
#' @description This function calculates environmental consequences of failure
#' Environmental consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().#'
#' Outputted in DKK.
#' @return Numeric. Financial consequences of failure for 30kV and 60kV switchgear
#' @param ehv_asset_category String The type of EHV asset category
#' @param type_env_factor String The type environment factor of 30kV and 60kV switchgear
#' @param prox_water Numeric. Specify the proximity to a water course in meters.
#' A setting of \code{"Default"} will result in a proximity factor of 1. Thus
#' assume the proximity to a water course is between 80m and 120m
#' @param bunded String. Options: \code{bunded = c("Yes", "No", "Default")}.
#' A setting of \code{"Default"} will result in a bunding factor of 1.
#' @export
#' @examples
#' environmental_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
#' type_env_factor = "Oil",
#' prox_water = 95,
#' bunded = "Yes")
environmental_cof_switchgear_30_60kv <- function(ehv_asset_category,
                                             type_env_factor,
                                             prox_water,
                                             bunded){

  GBP_to_DKK <- 8.71
  if (ehv_asset_category == "30kV" ) {
    ehv_asset_category <- "33kV CB (Air Insulated Busbars)(ID) (GM)"
  } else if (ehv_asset_category == "60kV" ) {
    ehv_asset_category <- "66kV CB (Air Insulated Busbars)(ID) (GM)"
  } else {
    ehv_asset_category <- NULL
    warning("Wrong input, please go to CNAIM.io for more documentation")
  }
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ehv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type env factor -------------------------------------
  asset_type_env_factor <- gb_ref$type_enviromental_factor %>%
    dplyr::filter(`Type environment factor` == asset_category)

  type_environmental_factor <- asset_type_env_factor[[type_env_factor]]

  # Size env factor -------------------------------------
  size_environmental_factor <- 1

  # Location environmetal factor table 231 ----------------------------------
  location_environ_al_factor <- gb_ref$location_environ_al_factor

  location_environ_al_factor_tf <- dplyr::filter(location_environ_al_factor,
                                                 `Asset Register Category` ==
                                                   asset_category)

  # Bunded "Yes", "No", "Default" ?
  if (bunded == "Default") {
    bunding_factor <- 1
  } else if (bunded == "Yes") {
    bunding_factor <-
      location_environ_al_factor_tf$`Bunding Factor: Bunded`
  } else if (bunded == "No") {
    bunding_factor <-
      location_environ_al_factor_tf$`Bunding Factor: Not bunded`
  }

  # Proximity to water.
  if(prox_water == "Default") {
    prox_factor <- 1
  } else if (prox_water >= 40 && prox_water < 80) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Close to Water Course (between 40m and 80m)`
  } else if (prox_water >= 80 && prox_water < 120) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Moderately Close to Water Course (between 80m and 120m)`
  } else if (prox_water > 120) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Not Close to Water Course (>120m) or No Oil`
  } else if (prox_water < 40) {
    prox_factor <- location_environ_al_factor_tf$
      `Proximity Factor: Very Close to Water Course (<40m)`
  }

  # Location environmental factor
  location_environmental_factor <- prox_factor * bunding_factor

  environmental_consequences_factor <- (type_environmental_factor *
                                          size_environmental_factor *
                                          location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof * GBP_to_DKK)
}


#' @title Network cost of Failure for 30kV and 60kV Switchgear
#' @description This function calculates network cost of failure for
#' 30kV and 60kV switchgear.
#' Network cost of failureis used in the derivation
#'  of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param ehv_asset_category String The type of 30kV and 60kV switchgear category
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @return Numeric. Network cost of failure.
#' @export
#' @examples
#' network_cof_switchgear_30_60kv(ehv_asset_category = "30kV",
#' actual_load_mva = 15)
network_cof_switchgear_30_60kv <- function(ehv_asset_category,
                                           actual_load_mva,
                                           secure = T) {
  GBP_to_DKK <- 8.71
  if (ehv_asset_category == "30kV" ) {
    ehv_asset_category <- "33kV CB (Air Insulated Busbars)(ID) (GM)"
  } else if (ehv_asset_category == "60kV" ) {
    ehv_asset_category <- "66kV CB (Air Insulated Busbars)(ID) (GM)"
  } else {
    ehv_asset_category <- NULL
    warning("Wrong input, please go to CNAIM.io for more documentation")
  }
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ehv_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_ehv_df <- gb_ref$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             ehv_asset_category)

  load_factor <- actual_load_mva/ref_nw_perf_cost_fail_ehv_single_row_df$`Maximum Demand Used To Derive Reference Cost (MVA)`

  # Network type factor -----------------------------------
  network_type_factor <- 1

  if(!secure){
    network_type_factor <- 2.5
  }

  # Network perfomance consequence factor -----------------------------------

  network_performance_consequence_factor <- load_factor *
    network_type_factor

  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof * GBP_to_DKK)

}
