#' @title Safety Consequences of Failure for Switchgears, Transformers &
#' Overhead Lines
#' @description This function calculates safety consequences of failure
#' for switchgear, transformers and overhead lines
#' (cf. section 7.4, page 75, CNAIM, 2017). Safety consequences of failure
#' is used in the derivation of consequences of failure see \code{\link{cof}}().
#' @param type_risk String. Risk that the asset presents to the
#' public by its characteristics and particular situation. Options:
#' \code{type_risk = c("Low", "Medium", "High", "Default")}
#' (cf. table 218, page 168, CNAIM, 2017).
#' A setting of \code{"Default"} equals a setting of \code{"Medium"}.
#' @param location_risk String. Proximity to areas that may affect its
#' likelihood of trespass or interference. Options:
#' \code{location_risk = c("Low", "Medium", "High", "Default")}
#' (cf. table 218, page 168, CNAIM, 2017).
#' A setting of \code{"Default"} equals a setting of \code{"Medium"}.
#' @param asset_type_scf String.
#' Options:
#' \code{asset_type_scf = c("LV Poles", "LV Circuit Breaker",
#' "LV Pillar (ID)", "LV Pillar (OD at Substation)",
#' "LV Pillar (OD not at a Substation)", "LV Board (WM)",
#' "LV UGB", "LV Board (X-type Network) (WM)", "6.6/11kV Poles",
#' "20kV Poles", "6.6/11kV CB (GM) Primary",
#' "6.6/11kV CB (GM) Secondary", "6.6/11kV Switch (GM)", "6.6/11kV RMU",
#' "6.6/11kV X-type RMU", "20kV CB (GM) Primary", "20kV CB (GM) Secondary",
#' "20kV Switch (GM)", "20kV RMU", "6.6/11kV Transformer (GM)",
#' "20kV Transformer (GM)", "33kV Pole", "66kV Pole",
#' "33kV OHL (Tower Line) Conductor", "33kV Tower", "33kV Fittings",
#' "66kV OHL (Tower Line) Conductor", "66kV Tower", "66kV Fittings",
#' "33kV CB (Air Insulated Busbars)(ID) (GM)",
#' "33kV CB (Air Insulated Busbars)(OD) (GM)",
#' "33kV CB (Gas Insulated Busbars)(ID) (GM)",
#' "33kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Switch (GM)",
#' "33kV RMU", "66kV CB (Air Insulated Busbars)(ID) (GM)",
#' "66kV CB (Air Insulated Busbars)(OD) (GM)",
#' "66kV CB (Gas Insulated Busbars)(ID) (GM)",
#' "66kV CB (Gas Insulated Busbars)(OD) (GM)", "33kV Transformer (GM)",
#' "66kV Transformer (GM)", "132kV OHL (Tower Line) Conductor",
#' "132kV Tower", "132kV Fittings",
#' "132kV CB (Air Insulated Busbars)(ID) (GM)",
#' "132kV CB (Air Insulated Busbars)(OD) (GM)",
#' "132kV CB (Gas Insulated Busbars)(ID) (GM)",
#' "132kV CB (Gas Insulated Busbars)(OD) (GM)", "132kV Transformer (GM)")
#'}
#' @return Numeric. Safety consequences of failure for
#' switchgear, transformers and overhead lines.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Safety consequences failure for a 6.6/11 kV transformer
#' s_cof_swg_tf_ohl(type_risk = "Default", location_risk = "Default",
#'                  asset_type_scf = "6.6/11kV Transformer (GM)")

s_cof_swg_tf_ohl <- function(type_risk = "Default",
                            location_risk = "Default",
                            asset_type_scf) {

  `Asset Register Category` = NULL
  # due to NSE notes in R CMD check

  # Get category ------------------------------------------------------------
  reference_costs_of_failure_tf <-
    dplyr::filter(gb_ref$reference_costs_of_failure,
                                                 `Asset Register Category` ==
                                                   asset_type_scf)

  # Reference safety cost of failure ----------------------------------------
  scost <- reference_costs_of_failure_tf$`Safety - (GBP)`


  #  Safety Consequence factor ----------------------------------------------
  safety_conseq_factor_sg_tf_oh <- gb_ref$safety_conseq_factor_sg_tf_oh

  if (location_risk == "Default") location_risk <- "Medium (Default)"
  if (location_risk == "Medium") location_risk <- "Medium (Default)"
  if (type_risk == "Default") type_risk <- "Medium"

  row_no <- which(safety_conseq_factor_sg_tf_oh$
  `Safety Consequence Factor - Switchgear, Transformers & Overhead Lines...2` ==
    location_risk)

  col_no <- grep(type_risk, colnames(safety_conseq_factor_sg_tf_oh))

  safety_consequence_factor <- safety_conseq_factor_sg_tf_oh[row_no, col_no]

  # Safety consequence of failure -------------------------------------------
  safety_cof <- safety_consequence_factor * scost

  return(safety_cof)
}
