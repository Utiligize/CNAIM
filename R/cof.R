#' @title Consequences of Failure
#' @description This function calculates consequences of failure
#' (cf.section 7, page 71, CNAIM, 2017).
#' @param financial_cof Numeric. Financial consequences of failure.
#' @param safety_cof Numeric. Safety consequences of failure.
#' @param environmental_cof Numeric. Environmental consequences of failure.
#' @param network_cof Numeric. Network cost of failure.
#' @return Numeric. Consequences of failure.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # Consequences of failure
#' # cof(financial_cof, safety_cof, environmental_cof, network_cof)
#'
cof <- function(financial_cof, safety_cof, environmental_cof, network_cof) {
  return(financial_cof + safety_cof + environmental_cof + network_cof)
}
