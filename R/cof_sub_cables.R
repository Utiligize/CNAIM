#' @title Financial cost of Failure for Sub cables
#' @description This function calculates financial consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Financial consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param sub_cable_asset_category String The type of Submarine cable asset category
#' Options: \code{sub_cable_asset_category = c("HV Sub Cable",
#' "EHV Sub Cable", "132kV Sub Cable")}.
#' @return Numeric. Financial consequences of failure for Sub cables
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' financial_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
financial_cof_sub_cables <- function(sub_cable_asset_category){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == sub_cable_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   sub_cable_asset_category)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factor <- 1

  # Access financial factor -------------------------------------------------
  access_financial_factor <- 1

  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_financial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}


#' @title Safety cost of Failure for Sub cables
#' @description This function calculates safety consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Safety consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param sub_cable_asset_category String The type of Submarine cable asset category
#' Options: \code{sub_cable_asset_category = c("HV Sub Cable",
#' "EHV Sub Cable", "132kV Sub Cable")}.
#' @return Numeric. Safety consequences of failure for Sub cables
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' safety_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
safety_cof_sub_cables <- function(sub_cable_asset_category){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == sub_cable_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   sub_cable_asset_category)

  # Reference financial cost of failure -------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`

  safety_consequence_factor <- 1

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof)
}


#' @title Environmental cost of Failure for sub cables
#' @description This function calculates environmental consequences of failure
#' (cf. section 7.3, page 79, CNAIM, 2021). Environmental consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @return Numeric. Financial consequences of failure for sub cables
#' @param sub_cable_asset_category String The type of Submarine cable asset category
#' Options: \code{sub_cable_asset_category = c("HV Sub Cable",
#' "EHV Sub Cable", "132kV Sub Cable")}.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2017:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' environmental_cof_sub_cables(sub_cable_asset_category = "HV Sub Cable")
environmental_cof_sub_cables <- function(sub_cable_asset_category){
  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Type environment factor` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == sub_cable_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   sub_cable_asset_category)

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
  return(environmental_cof)
}


#' @title Network cost of Failure for HV Sub cables
#' @description This function calculates network cost of failure for
#' Sub cables
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param sub_cable_asset_category String The type of Submarine cable asset category
#' Option: \code{sub_cable_asset_category = "HV Sub Cable"}.
#' @param no_customers Numeric. The number of customers
#' fed by an individual asset.
#' @param kva_per_customer Numeric. If the asset have an exceptionally high
#' demand per customer type in kVA per customer. A setting of \code{"Default"}
#' results in a multiplication factor of 1 (cf. table 18, page 89, CNAIM, 2021).
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' network_cof_hv_sub_cables(sub_cable_asset_category = "HV Sub Cable",
#' no_customers = 750, kva_per_customer = 51)
network_cof_hv_sub_cables<- function(sub_cable_asset_category,
                                   no_customers,
                                   kva_per_customer = "Default") {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == sub_cable_asset_category) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   sub_cable_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Customer factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_lv_hv <- gb_ref$ref_nw_perf_cost_fail_lv_hv
  ref_nw_perf_cost_fail_lv_hv_tf <- dplyr::filter(ref_nw_perf_cost_fail_lv_hv,
                                                  `Asset Category` ==
                                                    sub_cable_asset_category)

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

  return(network_cof)

}



#' @title Network cost of Failure for EHV /132 kV sub cables
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param sub_cable_asset_category String The type of Submarine cable asset category
#' Options: \code{sub_cable_asset_category = c( "EHV Sub Cable", "132kV Sub Cable")}.
#' @param actual_load_mva Numeric. The actual load on the asset
#' @param secure Boolean If the asset is in a secure network or not
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' network_cof_ehv_sub_cable(sub_cable_asset_category = "EHV Sub Cable",
#' actual_load_mva = 15, secure = TRUE)
network_cof_ehv_sub_cable<- function(sub_cable_asset_category,
                                actual_load_mva,
                                secure = T) {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` =
    `Maximum Demand Used To Derive Reference Cost (MVA)` = NULL

  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   sub_cable_asset_category)

  # Reference financial cost of failure -------------------------------------
  ncost <- reference_costs_of_failure_tf$`Network Performance - (GBP)`

  # Load factor ---------------------------------------------------------
  ref_nw_perf_cost_fail_ehv_df <- gb_ref$ref_nw_perf_cost_of_fail_ehv
  ref_nw_perf_cost_fail_ehv_single_row_df <- dplyr::filter(ref_nw_perf_cost_fail_ehv_df,
                                                           `Asset Category` ==
                                                             sub_cable_asset_category)

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
