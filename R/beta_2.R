#' @title Forecast Ageing Rate
#' @description This function calculates the forecast Ageing Rate for an
#' electric network asset. See section 6.1.8 on page 34 in CNAIM (2017).
#' @param current_health_score Numeric. The output returned by the
#' function \code{\link{current_health}}().
#' @inheritParams pof_future_transformer_11kv
#' @return Numeric. Forecast ageing rate for an electric network asset.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#'\url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#'beta_2(current_health_score = 1, age = 25)

beta_2 <- function(current_health_score, age) {
  # the Health Score of a new asset
  h_new = 0.5

  return(log(current_health_score / h_new) / age)
}

