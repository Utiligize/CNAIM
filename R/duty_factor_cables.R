#' @title Duty Factor for all cables (incl. submarine cables).
#' @description This function calculates the duty factor for under all types of
#' cables depending on the maximum
#' percentage utilisation under normal operating conditions.
#' The duty factor is used in the deriviation of the expected life of an asset.
#' See e.g. \code{\link{expected_life}()}. For more general information about
#' the derivation of the duty factor see section 6.6 on page 51 in CNAIM (2021)
#' @param utilisation_pct Numeric. The max percentage of utilisation
#' under normal operating conditions.
#' @param operating_voltage_pct Numeric. The ratio in percent of
#' operating/design voltage.
#' @param voltage_level String. Specify the voltage level. Options:
#' \code{voltage_level = c("EHV", "HV")}.
#' Choose \code{"EHV"} for cables \code{>= 33kV}  and \code{"HV"}
#' for cables \code{< 33kV} .
#' @return Numeric. Duty factor for cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' duty_factor_cables(utilisation_pct = "Default",
#' operating_voltage_pct = "Default",
#' voltage_level = "EHV")

duty_factor_cables <- function(utilisation_pct = "Default",
                               operating_voltage_pct = "Default",
                               voltage_level = "EHV") {

  `Duty Factor (HV)` = `Duty Factor (EHV & 132kV)` = NULL
  if (voltage_level == "EHV") {
    duty_factor_table1 <- gb_ref$duty_factor_lut_cables_df1 %>% dplyr::select(
      !`Duty Factor (HV)`)
    duty_factor_table1$`Duty Factor` <-
      duty_factor_table1$`Duty Factor (EHV & 132kV)`

  } else if(voltage_level == "HV") {
    duty_factor_table1 <- gb_ref$duty_factor_lut_cables_df1 %>% dplyr::select(
      !`Duty Factor (EHV & 132kV)`)
    duty_factor_table1$`Duty Factor` <-
      duty_factor_table1$`Duty Factor (HV)`
  }


  for (n in 1:nrow(duty_factor_table1)){
    if (utilisation_pct == 'Default'){
      duty_factor1 <- duty_factor_table1$`Duty Factor`[nrow(duty_factor_table1)]
      break
    } else if (utilisation_pct > as.numeric(duty_factor_table1$Lower[n]) &
               utilisation_pct <= as.numeric(duty_factor_table1$Upper[n])){
      duty_factor1 <- duty_factor_table1$`Duty Factor`[n]
      break
    }
  }

  duty_factor_table2 <- gb_ref$duty_factor_lut_cables_df2

  for (n in 1:nrow(duty_factor_table2)){
    if (operating_voltage_pct == 'Default'){
      duty_factor2 <- duty_factor_table2$`Duty Factor`[nrow(duty_factor_table2)]
      break
    } else if (operating_voltage_pct > as.numeric(duty_factor_table2$Lower[n]) &
               operating_voltage_pct <=
               as.numeric(duty_factor_table2$Upper[n])){
      duty_factor2 <- duty_factor_table2$`Duty Factor`[n]
      break
    }
  }


  return(0.5 * (duty_factor1 + duty_factor2))
}
