#' @importFrom magrittr %>%
#' @title Cumulative discounted PoF (0-n)
#' @description This function calculates the cumulative discounted PoF.
#' In determining the ‘cumulative discounted PoF’, the current year PoF and
#' future PoF for a period of 30 years shall be considered.
#' A discount rate of 3.5% shall be applied for each year. This discounting rate
#' is consistent with the Social Time Preference Rate in the HM Treasury Green
#' Book (2020) [Ref. 11] and Ofgem CBA methodology for RIIO-ED2.
#' The function is a cubic curve that is based on
#' For more information about the cumulative discounted PoF
#' function see section 5.5 on page 32 in CNAIM (2021).
#' @param hv_lv_cable_type String.
#' A sting that refers to the specific asset category.
#' Options:
#' \code{hv_lv_cable_type = c("10-20kV cable, PEX","10-20kV cable, APB",
#' "0.4kV cable")}. The default setting is
#' \code{hv_lv_cable_type = "10-20kV cable, PEX"}.
#' @param sub_division String. Refers to material the sheath and conductor is
#' made of. Options:
#' \code{sub_division = c("Aluminium sheath - Aluminium conductor",
#' "Aluminium sheath - Copper conductor",
#' "Lead sheath - Aluminium conductor", "Lead sheath - Copper conductor")
#'}
#' @inheritParams duty_factor_cables
#' @param sheath_test String. Only applied for non pressurised cables.
#' Indicating the state of the sheath. Options:
#' \code{sheath_test = c("Pass", "Failed Minor", "Failed Major",
#' "Default")}. See page 141, table 168 in CNAIM (2017).
#' @param partial_discharge String. Only applied for non pressurised cables.
#' Indicating the level of partial discharge. Options:
#' \code{partial_discharge = c("Low", "Medium", "High",
#'  "Default")}. See page 141, table 169 in CNAIM (2017).
#' @param fault_hist Numeric. Only applied for non pressurised cables.
#' The calculated fault rate for the cable in the period per kilometer.
#' A setting of \code{"No historic faults recorded"}
#' indicates no fault. See page 141, table 170 in CNAIM (2017).
#' @inheritParams current_health
#' @param age Numeric. The current age in years of the cable.
#' @param normal_expected_life_cable Numeric. The normal expected life for the
#' cable type.
#' @return Numeric. Current probability of failure
#' per annum for 20/10/0.4kV cables.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 1.1, 2017:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # Current annual probability of failure for 10-20kV cable, APB, 50 years old
#'pof_cables_10kV_APB <-
#'pof_cables_20_10_04kv(hv_lv_cable_type = "10-20kV cable, APB",
#'sub_division = "Lead sheath - Copper conductor",
#'utilisation_pct = 80,
#'operating_voltage_pct = 60,
#'sheath_test = "Default",
#'partial_discharge = "Default",
#'fault_hist = "Default",
#'reliability_factor = "Default",
#'age = 50,
#'normal_expected_life_cable = 80) * 100
#'
#'paste0(sprintf("Probability of failure %.4f", pof_cables_10kV_APB),
#'" percent per annum")
