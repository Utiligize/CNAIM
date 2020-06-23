#' @title Initial Health
#' @description Calculating the initial health score for a given asset.
#' See section 6.1.6 on page 32 in CNAIM (2017).
#' @param b1 Numeric. The output returned by the function
#' \code{\link{beta_1}}().
#' @param age Numeric. The crurrent age of the asset.
#' @return Numeric. Initial health for an electric network asset.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Healh & Criticality - Version 1.1, 2017:
#'\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' # 6.6/ 11 kv transformer age 10 years and an initial age rate of 0.05
#' initial_health(b1 = 0.05,
#'                age = 10)


initial_health <- function(b1, age) {
  # the Health Score of a new asset
  h_new <- 0.5
  # initial_health_score is capped at 5.5
  return(pmin(5.5, h_new * exp(b1 * age)))
}
