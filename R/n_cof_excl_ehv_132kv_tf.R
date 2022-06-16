#' @importFrom magrittr %>%
#' @title Network cost of Failure for all Assets Categories excl.
#' EHV and 132kV Transformers
#' @description This function calculates network cost of failure for
#' all asset categories exclusive the assets EHV and 132kV transformers.
#' (cf. section 7.6, page 87, CNAIM, 2021). Network cost of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param asset_type_ncf String.
#  'Options:
#' \code{asset_type_ncf = c("LV Poles", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)",
#' "LV Pillar (OD not at a Substation)", "LV Board (WM)",
#' "LV UGB", "LV Board (X-type Network) (WM)", "6.6/11kV Poles",
#' "20kV Poles", "HV Sub Cable", "6.6/11kV CB (GM) Primary",
#' "6.6/11kV CB (GM) Secondary", "6.6/11kV Switch (GM)", "6.6/11kV RMU",
#' "6.6/11kV X-type RMU", "20kV CB (GM) Primary", "20kV CB (GM) Secondary",
#' "20kV Switch (GM)", "20kV RMU", "6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Pole", "66kV Pole",
#' "33kV OHL (Tower Line) Conductor", "33kV Tower", "33kV Fittings",
#' "66kV OHL (Tower Line) Conductor", "66kV Tower", "66kV Fittings",
#' "33kV UG Cable (Non Pressurised)", "33kV UG Cable (Oil)",
#' "33kV UG Cable (Gas)", "66kV UG Cable (Non Pressurised)",
#' "66kV UG Cable (Oil)", "66kV UG Cable (Gas)",
#' "33kV CB (Air Insulated Busbars)(ID) (GM)",
#' "33kV CB (Air Insulated Busbars)(OD) (GM)",
#' "33kV CB (Gas Insulated Busbars)(ID) (GM)",
#' "33kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Switch (GM)",
#' "33kV RMU", "66kV CB (Air Insulated Busbars)(ID) (GM)",
#' "66kV CB (Air Insulated Busbars)(OD) (GM)",
#' "66kV CB (Gas Insulated Busbars)(ID) (GM)",
#' "66kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Transformer (GM)",
#' "66kV Transformer (GM)")}
#' @param no_customers Numeric. The numner of customers
#' fed by an individual asset.
#' @param kva_per_customer Numeric. If the asset have an exceptionally high
#' demand per customer type in kVA per customer. A setting of \code{"Default"}
#' results in a multiplication factor of 1 (cf. table 18, page 90, CNAIM, 2021).
#' @return Numeric. Network cost of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Network cost of failure for a 6.6/11 kV transformer with 750 customers
#' # and 51 kVA per customer.
#' n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
#' no_customers = 750, kva_per_customer = 51)

n_cof_excl_ehv_132kv_tf <- function(asset_type_ncf,
                                    no_customers,
                                    kva_per_customer = "Default") {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL
  # due to NSE notes in R CMD check

  # Get category ------------------------------------------------------------
  asset_type_ncf <- "6.6/11kV Transformer (GM)"

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == asset_type_ncf) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure <-
    gb_ref$reference_costs_of_failure

  reference_costs_of_failure_tf <- dplyr::filter(reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   asset_type_ncf)


  # Reference network cost of failure -------------------------------------
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
