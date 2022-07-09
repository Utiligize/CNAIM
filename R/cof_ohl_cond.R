#' @title Financial cost of Failure for Overhead Line Conductors
#' @description This function calculates financial consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Financial consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param ohl_cond_asset_category String The type of Pole asset category
#' Options: \code{ohl_cond_asset_category = c("33kV OHL (Tower Line) Conductor",
#' "66kV OHL (Tower Line) Conductor", "132kV OHL (Tower Line) Conductor")}.
#' @param access_factor_criteria String. Asses Financial factor criteria for Pole
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' Options: \code{access_factor_criteria = c("Type A", "Type B")}.
#' @return Numeric. Financial consequences of failure for Poles
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' financial_cof_ohl_cond(
#' ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor",
#' access_factor_criteria = "Type A")
financial_cof_ohl_cond <- function(ohl_cond_asset_category,
                                 access_factor_criteria){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ohl_cond_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factor <- 1

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref$access_factor_ohl
  access_finacial_factor_asset_category <- "EHV OHL Conductors (Tower Lines)"

  if(asset_category == "132kV OHL Conductor (Tower Lines)"){
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
  return(fc_factor * fcost)
}


#' @title Safety cost of Failure for Overhead Line Conductors
#' @description This function calculates safety consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Safety consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param ohl_cond_asset_category String The type of Pole asset category
#' Options: \code{ohl_cond_asset_category = c("33kV OHL (Tower Line) Conductor",
#' "66kV OHL (Tower Line) Conductor", "132kV OHL (Tower Line) Conductor")}.
#' @param location_risk String Type Financial factor criteria for Overhead Line Conductors
#' (cf. section D1.2.1, page 178, CNAIM, 2021).
#' \code{location_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{location_risk = "Medium"}.
#' @param type_risk String. Asses Financial factor criteria for Overhead Line Conductors
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' \code{type_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{type_risk = "Medium"}.
#' @return Numeric. Safety consequences of failure for Overhead Line Conductors
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' safety_cof_ohl_cond(
#' ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor",
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_ohl_cond <- function(ohl_cond_asset_category,
                              location_risk,
                              type_risk){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ohl_cond_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

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

  return(safety_cof)
}


#' @title Environmental cost of Failure for Overhead line conductors
#' @description This function calculates environmental consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Environmental consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().#'
#' @return Numeric. Financial consequences of failure for LV switchgear
#' @param ohl_cond_asset_category String The type of Pole asset category
#' Options: \code{ohl_cond_asset_category = c("33kV OHL (Tower Line) Conductor",
#' "66kV OHL (Tower Line) Conductor", "132kV OHL (Tower Line) Conductor")}.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' environmental_cof_ohl_cond(ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor")
environmental_cof_ohl_cond <- function(ohl_cond_asset_category){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == ohl_cond_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
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
  return(environmental_cof)
}


#' @title Network cost of Failure for Overhead Line Conductors
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param ohl_cond_asset_category String The type of Pole asset category
#' Options: \code{ohl_cond_asset_category = c("33kV OHL (Tower Line) Conductor",
#' "66kV OHL (Tower Line) Conductor", "132kV OHL (Tower Line) Conductor")}.
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' network_cof_ohl_cond(ohl_cond_asset_category = "33kV OHL (Tower Line) Conductor",
#' actual_load_mva = 15)
network_cof_ohl_cond<- function(ohl_cond_asset_category,
                             actual_load_mva,
                             secure = T) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   ohl_cond_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_ehv_df <- gb_ref$ref_nw_perf_cost_of_fail_ehv
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

  return(network_cof)

}
