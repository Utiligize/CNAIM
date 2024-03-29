#' @importFrom magrittr %>%
#' @title Oil Test Modifier
#' @description This function calculates the FFA test modifier based on the
#' levels of furfuraldehyde in the transformer oil. This function applies for
#' 33/10kV, 66/10kV and 132kV transformers. See e.g. section 6.13 on page
#' 71 in CNAIM (2021).
#' @param furfuraldehyde Numeric. Refers to the furfuraldehyde level in the
#' transformer oil. furfuraldehyde levels are measured in ppm.
#' A setting of \code{"Default"} will result in the best possible result.
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Data table.
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' # FFA test modifier
#' ffa_test_modifier(furfuraldehyde = 50)

ffa_test_modifier <- function(furfuraldehyde = "Default",
                              gb_ref_given = NULL) {

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  if (furfuraldehyde == "Default") furfuraldehyde <- -0.01

  ffa_test_factor <-
    gb_ref_taken$ffa_test_factor

  ffa_test_factor$Lower[1] <- -Inf

  for (n in 1:nrow(ffa_test_factor)){

    if (furfuraldehyde > as.numeric(ffa_test_factor$Lower[n]) &
        furfuraldehyde <= as.numeric(ffa_test_factor$Upper[n])) {
      ffa_test_factor <- ffa_test_factor$`FFA Test Factor`[n]
      break

    }
  }

  ffa_test_cap <- 10

  ffa_test_collar <- ifelse(is.na(2.33 * furfuraldehyde^0.68), 0.5,
                            2.33 * furfuraldehyde^0.68)

  ffa_test_collar <- ifelse(ffa_test_collar > 7, 7, ffa_test_collar)

  ffa_test_mod <- data.frame(ffa_test_factor,
                             ffa_test_cap,
                             ffa_test_collar)



  return(ffa_test_mod)}


