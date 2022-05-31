#' @title cumulative discounted PoF
#' @description This function calculates the present value of future risk.
#' See section 5.5 on page 32 in CNAIM (2021).
#' @param pof The probability of failure of the asset
#' @param cof The consequence of failure of the asset
#' @param n number of future years considered
#' @param i number of years subsequent to current year (where current year is year 0)
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#
#'
present_value_future_risk <- function(pof, cof, n, i) {
  #discount rate stated on page 33
  r <- 0.035

  return((sum(pof(i)*(1+r)^-(i)))*cof)
}
