#' @title Financial cost of Failure for 50kV Overhead Line Conductors
#' @description This function calculates financial consequences of failure
#' Outputted in DKK
#' @param access_factor_criteria String. Asses Financial factor criteria for
#' Overhead Line Conductors.
#' Options: \code{access_factor_criteria = c("Type A", "Type B")}.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Financial consequences of failure for Overhead Line Conductors
#' @export
#' @examples
#' financial_cof_ohl_cond_50kv(
#' access_factor_criteria = "Type A")
financial_cof_ohl_cond_50kv <- function(access_factor_criteria,
                                        gb_ref_given = NULL) {

  GBP_to_DKK <- 8.71
  ohl_cond_asset_category <- "66kV OHL (Tower Line) Conductor"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ohl_cond_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factor <- 1

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref_taken$access_factor_ohl
  access_finacial_factor_asset_category <- "EHV OHL Conductors (Tower Lines)"

  if(asset_category == "132kV OHL Conductor (Tower Lines)") {
    access_finacial_factor_asset_category <- "132kV OHL Conductors (Tower Lines)"
  }

  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                               access_finacial_factor_asset_category)

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


#' @title Safety cost of Failure for 50kV Overhead Line Conductors
#' @description This function calculates safety consequences of failure
#' Outputted in DKK
#' @param location_risk String Type Financial factor criteria for Overhead Line Conductors
#' Options: \code{location_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{location_risk = "Medium"}.
#' @param type_risk String. Asses Financial factor criteria for Overhead Line Conductors
#' Options: \code{type_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{type_risk = "Medium"}.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Safety consequences of failure for Overhead Line Conductors
#' @export
#' @examples
#' safety_cof_ohl_cond_50kv(
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_ohl_cond_50kv <- function(location_risk,
                                     type_risk,
                                     gb_ref_given = NULL) {

  GBP_to_DKK <- 8.71
  ohl_cond_asset_category <- "66kV OHL (Tower Line) Conductor"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ohl_cond_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`

  if (location_risk == "Default") location_risk <- "Medium (Default)"
  if (location_risk == "Medium") location_risk <- "Medium (Default)"
  if (type_risk == "Default") type_risk <- "Medium"

  safety_conseq_factor_sg_tf_oh <- gb_ref_taken$safety_conseq_factor_sg_tf_oh

  row_no <- which(safety_conseq_factor_sg_tf_oh$
                    `Safety Consequence Factor - Switchgear, Transformers & Overhead Lines...2` ==
                    location_risk)

  col_no <- grep(type_risk, colnames(safety_conseq_factor_sg_tf_oh))

  safety_consequence_factor <- safety_conseq_factor_sg_tf_oh[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof * GBP_to_DKK)
}


#' @title Environmental cost of Failure for 50kV Overhead Line Conductors
#' @description This function calculates environmental consequences of failure
#' Outputted in DKK
#' @param gb_ref_given optional parameter to use custom reference values
#' @export
#' @examples
#' environmental_cof_ohl_cond_50kv()
environmental_cof_ohl_cond_50kv <- function(gb_ref_given = NULL) {

  GBP_to_DKK <- 8.71
  ohl_cond_asset_category <- "66kV OHL (Tower Line) Conductor"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref_taken$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ohl_cond_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  # Type env factor -------------------------------------
  type_environmental_factor <- 1

  # Size env factor -------------------------------------
  size_environmental_factor <- 1

  # Location environmetal factor table 231 ----------------------------------

  location_environmental_factor <- 1

  environmental_consequences_factor <- (type_environmental_factor *
                                          size_environmental_factor *
                                          location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof * GBP_to_DKK)
}


#' @title Network cost of Failure for 50kV Overhead Line Conductors
#' @description This function calculates network cost of failure
#' Outputted in DKK
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Network cost of failure.
#' @export
#' @examples
#' network_cof_ohl_cond_50kv(
#' actual_load_mva = 15)
network_cof_ohl_cond_50kv<- function(actual_load_mva,
                                     secure = T,
                                     gb_ref_given = NULL) {

  GBP_to_DKK <- 8.71
  ohl_cond_asset_category <- "33kV OHL (Tower Line) Conductor"
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref_taken$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_ehv_df <- gb_ref_taken$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             ohl_cond_asset_category)

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
