#' @title Financial cost of Failure for Poles OHL Support 50kV
#' @description This function calculates financial consequences of failure
#' Financial consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param pole_asset_category String The type of Pole asset category
#' @param type_financial_factor_criteria String. Type Financial factor criteria for Pole
#' @param access_factor_criteria String. Asses Financial factor criteria for Pole
#' setting.
#' @return Numeric. Financial consequences of failure for Poles
#' @export
#' @examples
#' financial_cof_poles_ohl_support_50kv(
#' type_financial_factor_criteria = "Small footprint steel masts",
#' access_factor_criteria = "Type A")
financial_cof_poles_ohl_support_50kv <- function(pole_asset_category,
                                type_financial_factor_criteria,
                                access_factor_criteria){

  GBP_to_DKK <- 8.71
   pole_asset_category <- "66kV Pole"
  `Asset Register Category` = `Health Index Asset Category` =
    `Type Financial Factor Criteria` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == pole_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   pole_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factors <- gb_ref$type_financial_factors
  type_financial_factors_tf <- dplyr::filter(type_financial_factors,
                                             `Asset Register Category` == pole_asset_category,
                                             `Type Financial Factor Criteria` == type_financial_factor_criteria)

  type_financial_factor <- type_financial_factors_tf$`Type Financial Factor`[1]

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref$access_factor_ohl

  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 asset_category)

  if (access_factor_criteria == "Type A") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (access_factor_criteria == "Type B") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type B Criteria - Major Crossing (e.g. associated span crosses railway line, major road, large waterway etc.)`
  }

  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_finacial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost * GBP_to_DKK)
}


#' @title Safety cost of Failure for Poles OHL Support 50kV
#' @description This function calculates safety consequences of failure
#' Safety consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' Outputted in DKK.
#' @param pole_asset_category String The type of pole asset category
#' @param location_risk String Type Financial factor criteria for Pole
#' @param type_risk String. Asses Financial factor criteria for pole
#' setting.
#' @return Numeric. Safety consequences of failure for poles
#' @export
#' @examples
#' safety_cof_poles_ohl_support_50kv(
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_poles_ohl_support_50kv <- function(pole_asset_category,
                             location_risk,
                             type_risk){
  GBP_to_DKK <- 8.71
  pole_asset_category <- "66kV Pole"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == pole_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   pole_asset_category)

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


#' @title Environmental cost of Failure for Poles OHL Support 50kV
#' @description This function calculates environmental consequences of failure
#' Environmental consequences of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().#' @return Numeric.
#' Financial consequences of failure for Poles OHL support 50kV
#' Outputted in DKK.
#' @param pole_asset_category String The type of Pole
#' @export
#' @examples
#' environmental_cof_poles_ohl_support_50kv()
environmental_cof_poles_ohl_support_50kv <- function(){

  GBP_to_DKK <- 8.71
  pole_asset_category <- "66kV Pole"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == pole_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   pole_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type env factor -------------------------------------
  type_environmental_factor <- 1

  # Size env factor -------------------------------------
  size_environmental_factor <- 1

  # Location environmetal factor table 222 ----------------------------------

  location_environmental_factor <- 1

  environmental_consequences_factor <- (type_environmental_factor *
                                          size_environmental_factor *
                                          location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof * GBP_to_DKK)
}


#' @title Network cost of Failure for Poles OHL Support 50kV
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' Network cost of failure is used in the derivation
#'  of consequences of failure see \code{\link{cof}}().
#'  Outputted in DKK.
#' @param pole_asset_category String The type of Pole
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @return Numeric. Network cost of failure.
#' @export
#' @examples
#' network_cof_poles_ohl_support_50kv(
#' actual_load_mva = 15)
network_cof_poles_ohl_support_50kv<- function(pole_asset_category,
                                              actual_load_mva,
                                              secure = T) {

  GBP_to_DKK <- 8.71
  pole_asset_category <- "66kV Pole"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   pole_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_ehv_df <- gb_ref$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             pole_asset_category)

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
