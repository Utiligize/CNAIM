#' @title Forecast Ageing Rate
#' @description This function calculates the forecast Ageing Rate for an
#' electric network asset. See section 6.1.8 on page 38 in CNAIM (2021).
#' @param current_health_score Numeric. The output returned by the
#' function \code{\link{current_health}}().
#' @param age Numeric. Age of the asset.
#' @return Numeric. Forecast ageing rate for an electric network asset.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#'\url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#'beta_2(current_health_score = 1, age = 25)

beta_2 <- function(current_health_score, age) {
  # the Health Score of a new asset
  h_new <- 0.5

  return(log(current_health_score / h_new) / age)
}

