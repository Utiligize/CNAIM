#' @importFrom magrittr %>%
#' @title Financial Consequences of Failure for a 6.6/11 kV Transformer
#' @description This function calculates financial consequences of failure
#' (cf. section 7.3, page 75, CNAIM, 2017). Financial consequences
#' of failure is used in
#' the derivation of consequences of failure see \code{\link{cof}}().
#' @param kva Numeric. The rated transformer capacity measured in kVA
#' for a 6.6/11 kV transformer. Rated capacity is used to derive the
#' type financial factor. For a general description of type financial factor see
#' section 7.3.3.1 on page 76 in CNAIM (2017). A setting of \code{"Default"}
#' will result in a type financial factor equal to 1
#' (cf. section D1.2.1, page 162, CNAIM, 2017).
#' @param type String. Relates to the accessibility of the transformer
#' Options: \code{type = c("Type A", "Type B", "Type C", "Default")}.
#' A setting of \code{"Type A"} - Normal access.
#' A setting of \code{"Type B"} - Constrained access or confined working space.
#' A setting of \code{"Type C"} - Underground substation.
#' A setting of \code{"Default"} - Normal access thus same as \code{"Type A"}
#' setting (cf. table 214, page 164, CNAIM, 2017).
#' @return Numeric. Financial consequences of failure for a 6.6/11 kV transformer.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Financial consequences of failure for a 6.6/11 kV transformer
#' f_cof_transformer_11kv(kva = 700, type = "Default")

f_cof_transformer_11kv <- function(kva = "Default",
                                   type = "Default") {

  `Asset Register Category` = `Health Index Asset Category` = `Asset Category` = NULL
  # due to NSE notes in R CMD check

  # Get category ------------------------------------------------------------
  asset_type <- "6.6/11kV Transformer (GM)"

  asset_category <- gb_ref$categorisation_of_assets %>%
    dplyr::filter(`Asset Register Category` == asset_type) %>%
    dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

  # Reference cost of failure table 16 --------------------------------------
  reference_costs_of_failure_tf <- dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   asset_type)

  # Reference financial cost of failure -------------------------------------
  fcost <- reference_costs_of_failure_tf$`Financial - (GBP)`

  # Type financial factor ---------------------------------------------------
  type_financial_factors <- gb_ref$type_financial_factors
  type_financial_factors_tf <- dplyr::filter(type_financial_factors,
                                             `Asset Register Category` ==
                                               asset_type)

  if (kva == 'Default'){
    type_financial_factor <- 1
  } else {
    for (n in 1:nrow(type_financial_factors_tf)){
      lower <- as.numeric(type_financial_factors_tf$Lower[n])
      upper <- as.numeric(type_financial_factors_tf$Upper[n])
      if (kva >= lower & kva < upper){
        type_financial_factor <- type_financial_factors_tf$`Type Financial Factor`[n]
        break
      }
    }
  }

  # Access financial factor -------------------------------------------------
  access_financial_factors <- gb_ref$access_factor_swg_tf_asset
  access_financial_factors_tf <- dplyr::filter(access_financial_factors,
                                             `Asset Category` ==
                                               asset_category)

  if (type == 'Default') type <- "Type A"
  if (type == "Type A") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type A Criteria - Normal Access ( & Default Value)`
  }
  else if (type == "Type B") {
    access_finacial_factor <-
      access_financial_factors_tf$
`Access Factor: Type B Criteria - Constrained Access or Confined Working Space`
  }
  else if (type == "Type C") {
    access_finacial_factor <-
      access_financial_factors_tf$
      `Access Factor: Type C Criteria - Underground substation`
  }

  # Financial consequences factor -------------------------------------------
  fc_factor <- type_financial_factor * access_finacial_factor

  # Financial consequences of failure ---------------------------------------
  return(fc_factor * fcost)
}
