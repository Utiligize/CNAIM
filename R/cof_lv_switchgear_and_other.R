#' @title Financial cost of Failure for LV swicthgear and others
#' @description This function calculates financial consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Financial consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param lv_asset_category String The type of LV asset category
#' Options: \code{lv_asset_category = c("LV Board (WM)",
#' "LV Board (X-type Network) (WM)", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)", "LV Pillar (OD not at a Substation)")}
#' @param type_financial_factor_criteria String Type Financial factor criteria for LV switchgear
#' (cf. section D1.2.1, page 178, CNAIM, 2021).
#' Options: \code{type_financial_factor_criteria = c("Non Asbestos clad",
#' "Asbestos clad")}
#' @param access_factor_criteria String. Asses Financial factor criteria for LV switchgear
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' Options: \code{access_factor_criteria = c("Type A", "Type B", "Type C")}.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Financial consequences of failure for LV switchgear
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' financial_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
#' type_financial_factor_criteria = "Asbestos clad",
#' access_factor_criteria = "Type A")
financial_cof_lv_switchgear_and_other <- function(lv_asset_category,
                                                  type_financial_factor_criteria,
                                                  access_factor_criteria,
                                                  gb_ref_given = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type Financial Factor Criteria` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == lv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   lv_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  if(lv_asset_category == "LV Board (WM)" || lv_asset_category == "LV Board (X-type Network) (WM)"){
    type_financial_factor <- 1
  }else{
    type_financial_factors <- gb_ref$type_financial_factors
    type_financial_factors_tf <- dplyr::filter(type_financial_factors,
                                               `Asset Register Category` == lv_asset_category,
                                               `Type Financial Factor Criteria` == type_financial_factor_criteria)

    type_financial_factor <- type_financial_factors_tf$`Type Financial Factor`[1]
  }

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref$access_factor_swg_tf_asset
  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                               `Asset Category` ==
                                                 "LV Switchgear")

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
  return(fc_factor * fcost)
}


#' @title Safety cost of Failure for LV swicthgear and others
#' @description This function calculates safety consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Safetyr consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param lv_asset_category String The type of LV asset category
#' Options: \code{lv_asset_category = c("LV Board (WM)",
#' "LV Board (X-type Network) (WM)", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)", "LV Pillar (OD not at a Substation)")}
#' @param location_risk String Type Financial factor criteria for LV switchgear
#' (cf. section D1.2.1, page 178, CNAIM, 2021).
#' Options: \code{location_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{location_risk = "Medium"}.
#' @param type_risk String. Asses Financial factor criteria for LV switchgear
#' setting (cf. table 221, page 180, CNAIM, 2021).
#' Options: \code{type_risk = c("Low", "Medium", "High")}.
#' The default setting is
#' \code{type_risk = "Medium"}.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Numeric. Financial consequences of failure for LV switchgear
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' safety_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
#' location_risk = "Default",
#' type_risk = "Default")
safety_cof_lv_switchgear_and_other <- function(lv_asset_category,
                                               location_risk,
                                               type_risk,
                                               gb_ref_given = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == lv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   lv_asset_category)

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


#' @title Environmental cost of Failure for LV swicthgear and others
#' @description This function calculates environmental consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Environmental consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().#' @return Numeric. Financial consequences of failure for LV switchgear
#' @param lv_asset_category String The type of LV asset category
#' Options: \code{lv_asset_category = c("LV Board (WM)",
#' "LV Board (X-type Network) (WM)", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)", "LV Pillar (OD not at a Substation)")}
#' @param gb_ref_given optional parameter to use custom reference values
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' environmental_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)")
environmental_cof_lv_switchgear_and_other <- function(lv_asset_category,
                                                      gb_ref_given = NULL){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == lv_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   lv_asset_category)

  # Reference financial cost of failure -------------------------------------
  ecost <- reference_costs_of_failure_tf$`Environmental - (GBP)`

  type_environmental_factor <- 1
  size_environmental_factor <- 1
  location_environmental_factor <- 1

  environmental_consequences_factor <- (type_environmental_factor *
                                          size_environmental_factor *
                                          location_environmental_factor)

  # Environmental consequences ----------------------------------------------
  environmental_cof <- environmental_consequences_factor * ecost
  return(environmental_cof)
}


#' @title Network cost of Failure for LV swicthgear and others
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param lv_asset_category String The type of LV asset category
#' Options: \code{lv_asset_category = c("LV Board (WM)",
#' "LV Board (X-type Network) (WM)", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)", "LV Pillar (OD not at a Substation)")}
#' @param no_customers Numeric. The numner of customers
#' fed by an individual asset.
#' @param kva_per_customer Numeric. If the asset have an exceptionally high
#' demand per customer type in kVA per customer. A setting of \code{"Default"}
#' results in a multiplication factor of 1 (cf. table 18, page 89, CNAIM, 2021).
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @param gb_ref_given optional parameter to use custom reference values
#' @export
#' @examples
#' network_cof_lv_switchgear_and_other(lv_asset_category = "LV Board (WM)",
#' no_customers = 750, kva_per_customer = 51)
network_cof_lv_switchgear_and_other <- function(lv_asset_category,
                                                no_customers,
                                                kva_per_customer = "Default",
                                                gb_ref_given = NULL) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- get_mmi_lv_switchgear_asset_category(lv_asset_category)

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   lv_asset_category)

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

  adj_cust_no <- gsub("([0-9]+).*$", "\\1", adj_cust_no) %>% as.numeric()

  customer_factor <- (adj_cust_no * no_customers) / ref_no_cust


  # Customer sensitivity factor ---------------------------------------------
  customer_sensitivity_factor <- 1 # See section 7.6.2.2, p. 89 in CNAIM (2021)


  # Network perfomance consequence factor -----------------------------------

  network_performance_consequence_factor <- customer_factor *
    customer_sensitivity_factor


  # Network performance cost of failure -------------------------------------
  network_cof <- network_performance_consequence_factor * ncost

  return(network_cof)

}
