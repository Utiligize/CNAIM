#' @title Duty Factor for 6.6/11kV and 20kV Transformers
#' @description This function calculates the duty factor for 6.6/11kV and 20kV
#' transformers depending on the maximum percentage utilisation under normal
#' operating conditions.
#' The duty factor is used in the deriviation of the expected life of an asset.
#' See e.g. \code{\link{expected_life}()}. For more general information about
#' the derivation of the duty factor see section 6.6 on page 51 in CNAIM (2021)
#' @param utilisation_pct Numeric. The max percentage of utilisation
#' under normal operating conditions.
#' @return Numeric. Duty factor for 6.6/11kV or 20kV transformer.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' duty_factor_transformer_11_20kv(utilisation_pct = 95)

duty_factor_transformer_11_20kv <- function(utilisation_pct = "Default") {

  duty_factor_table <- gb_ref$duty_factor_lut_distrib_tf

  for (n in 1:nrow(duty_factor_table)){
    if (utilisation_pct == 'Default'){
      duty_factor <- duty_factor_table$`Duty Factor`[nrow(duty_factor_table)]
      break
    } else if (utilisation_pct > as.numeric(duty_factor_table$Lower[n]) &
        utilisation_pct <= as.numeric(duty_factor_table$Upper[n])){
      duty_factor <- duty_factor_table$`Duty Factor`[n]
      break
    }
  }

  return(duty_factor)
}
