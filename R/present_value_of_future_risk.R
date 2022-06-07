#' @title Present Value of Future Risk
#' @description This function calculates the present value of future risk.
#' See section 5.5 on page 32 in CNAIM (2021).
#' @param pof A vector of the probability of failure of the asset over years
#' @param cof The consequence of failure of the asset
#' @param r discount rate
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' present_value_future_risk(c(0.1, 0.2, 0.5), 100)
present_value_future_risk <- function(pof, cof, r = 0.035) {
  n <- length(pof)
  sum_pof <- lapply(1:length(pof), function(i){
    pof[i] * (1+r)^(-i)
  }) %>% unlist() %>% sum()
  return(sum_pof * cof)
}
