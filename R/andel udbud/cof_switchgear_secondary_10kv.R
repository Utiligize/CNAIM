#' @title Financial cost of Failure for 10 kV Swicthgear Secondary
#' @description This function calculates financial consequences of failure
#' Financial consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param access_factor_criteria String. Asses Financial factor criteria for LV switchgear
#' setting
#' @export
#' @examples
#' financial_cof_switchgear_secondary_10kv(
#' access_factor_criteria = "Type A")
financial_cof_switchgear_secondary_10kv <- function(access_factor_criteria) {

  GBP_to_DKK <- 8.71
  hv_asset_category <- "6.6/11kV CB (GM) Secondary"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == hv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   hv_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factor <- 1

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref$access_factor_swg_tf_asset
  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 "HV Switchgear (GM) - Distribution")

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


#' @title Safety cost of Failure for 10 kV Switchgear Secondary
#' @description This function calculates safety consequences of failure.
#'Safety consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param location_risk String Type Financial factor criteria for 10kV switchgear secondary
#' (cf. section D1.2.1, page 178, CNAIM, 2021).
#' @param type_risk String. Asses Financial factor criteria for 10kV switchgear secondary
#' setting
#' @return Numeric. Financial consequences of failure for 10kV switchgear secondary
#' @export
#' @examples
#' safety_cof_switchgear_secondary_10kv(
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_switchgear_secondary_10kv <- function(location_risk,
                                                  type_risk){

  GBP_to_DKK <- 8.71
  hv_asset_category <- "6.6/11kV CB (GM) Secondary"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == hv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   hv_asset_category)

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


#' @title Environmental cost of Failure for 10kV Switchgear Secondary
#' @description This function calculates environmental consequences of failure.
#' Environmental consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' Financial consequences of failure for 10 kV switchgear secondary
#' @param type_env_factor String The type environment factor of HV asset category
#' @param prox_water Numeric. Specify the proximity to a water course in meters.
#' A setting of \code{"Default"} will result in a proximity factor of 1. Thus
#' assume the proximity to a water course is between 80m and 120m
#' @param bunded String. Options: \code{bunded = c("Yes", "No", "Default")}.
#' A setting of \code{"Default"} will result in a bunding factor of 1.
#' @export
#' @examples
#' environmental_cof_switchgear_secondary_10kv(
#' type_env_factor = "Oil", prox_water = 95,
#' bunded = "Yes")
environmental_cof_switchgear_secondary_10kv <- function(type_env_factor,
                                                         prox_water,
                                                         bunded){

  GBP_to_DKK <- 8.71
  hv_asset_category <- "6.6/11kV CB (GM) Secondary"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == hv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   hv_asset_category)

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


#' @title Network cost of Failure for 10kV Switchgear Secondary
#' @description This function calculates network cost of failure for
#' 10kV Switchgear Secondary
#' Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param no_customers Numeric. The number of customers
#' fed by an individual asset.
#' @param kva_per_customer Numeric. If the asset have an exceptionally high
#' demand per customer type in kVA per customer. A setting of \code{"Default"}
#' results in a multiplication factor of 1 (cf. table 18, page 90, CNAIM, 2021).
#' @return Numeric. Network cost of failure.
#' @export
#' @examples
#' network_cof_switchgear_secondary_10kv(
#' no_customers = 750, kva_per_customer = 51)
network_cof_switchgear_secondary_10kv <- function(no_customers,
                                                   kva_per_customer = "Default") {

  GBP_to_DKK <- 8.71
  hv_asset_category <- "6.6/11kV CB (GM) Secondary"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == hv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   hv_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Customer factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_lv_hv <- gb_ref$ref_nw_perf_cost_fail_lv_hv
  ref_nw_perf_cost_fail_lv_hv_tf <- dplyr::filter(ref_nw_perf_cost_fail_lv_hv,
                                                  `Asset Category` ==
                                                    asset_category)

  ref_no_cust <-
    ref_nw_perf_cost_fail_lv_hv_tf$`Reference Number of Connected Customers`

  customer_no_adjust_lv_hv_asset <- gb_ref$customer_no_adjust_lv_hv_asset


  for (n in 1:nrow(customer_no_adjust_lv_hv_asset)){
    if (kva_per_customer == 'Default'){
      adj_cust_no <- 1
      break
    } else if (kva_per_customer >= as.numeric(
      customer_no_adjust_lv_hv_asset$Lower[n]) &
      kva_per_customer < as.numeric(
        customer_no_adjust_lv_hv_asset$Upper[n])){
      adj_cust_no <-
        customer_no_adjust_lv_hv_asset$
        `No. of Customers to be used in the derivation of Customer Factor`[n]
      break
    }
  }

  adj_cust_no <-
    adj_cust_no %>% stringr::str_match_all("[0-9]+") %>% unlist %>% as.numeric

  customer_factor <- (adj_cust_no * no_customers) / ref_no_cust


  # Customer sensitivity factor ---------------------------------------------
  customer_sensitivity_factor <- 1 # See section 7.6.2.2, p. 89 in CNAIM (2021)


  # Network perfomance consequence factor -----------------------------------

  network_performance_consequence_factor <- customer_factor *
    customer_sensitivity_factor


  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof * GBP_to_DKK)

}
