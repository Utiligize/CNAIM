#' @title Duty Factor for all cables under 66kV
#' @description This function calculates the duty factor for under
#' cables under 66kV depending on the maximum
#' percentage utilisation under normal operating conditions.
#' The duty factor is used in the deriviation of the expected life of an asset.
#' See e.g. \code{\link{expected_life}}(). For more general information about
#' the derivation of the duty factor see section 6.6 on page 47 in CNAIM (2017)
#' @param utilisation_pct Numeric. The max percentage of utilisation
#' under normal operating conditions.
#' @param operating_voltage_pct Numeric. The percentage of operating/design
#' voltage.
#' @param voltage_level String. Specify the voltage level. Options:
#' \code{voltage_level = c("EHV", "LV & HV")}.
#' Choose \code{"EHV"} for 33-66kV cables and \code{"LV & HV"} for cables under
#' 33kV.
#' @return Numeric. Duty factor for cables under 66kV.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf}
#' @export
#' @examples
#' duty_factor_cables_u66kv(utilisation_pct = "Default",
#' operating_voltage_pct = "Default",
#' voltage_level = "EHV")

duty_factor_cables_u66kv <- function(utilisation_pct = "Default",
                                     operating_voltage_pct = "Default",
                                     voltage_level = "EHV") {

  if (voltage_level == "EHV") {
    duty_factor_table1 <- gb_ref$duty_factor_lut_cables_df1 %>% dplyr::select(
      !`Duty Factor (LV & HV)`)
    duty_factor_table1$`Duty Factor` <-
      duty_factor_table1$`Duty Factor (EHV & 132kV)`

  } else if(voltage_level == "LV & HV") {
    duty_factor_table1 <- gb_ref$duty_factor_lut_cables_df1 %>% dplyr::select(
      !`Duty Factor (EHV & 132kV)`)
    duty_factor_table1$`Duty Factor` <-
      duty_factor_table1$`Duty Factor (LV & HV)`
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
