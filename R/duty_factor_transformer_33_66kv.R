#' @title Duty Factor for 33/10kV and 66/10kV Transformers and Tapchanger
#' @description This function calculates the duty factor for
#' 33/10kV and 66/10kV transformers depending on the maximum
#' percentage utilisation under normal operating conditions. And the tapchanger
#' depending on the average number of daily taps.
#' The duty factor is used in the derivation of the expected life of an asset.
#' See e.g. \code{\link{expected_life}}(). For more general information about
#' the derivation of the duty factor see section 6.6 on page 51 in CNAIM (2021)
#' @param utilisation_pct Numeric. The max percentage of utilisation
#' under normal operating conditions.
#' @param no_taps Numeric. Average number of daily taps (tapchanger).
#' @param gb_ref_given optional parameter to use custom reference values
#' @return Data table. Duty factor for the transformer and for the tapcharger
#' @source DNO Common Network Asset Indices Methodology (CNAIM),
#' Health & Criticality - Version 2.1, 2021:
#' \url{https://www.ofgem.gov.uk/sites/default/files/docs/2021/04/dno_common_network_asset_indices_methodology_v2.1_final_01-04-2021.pdf}
#' @export
#' @examples
#' duty_factor_transformer_33_66kv(utilisation_pct = 95,
#' no_taps = 25)

duty_factor_transformer_33_66kv <- function(utilisation_pct = "Default",
                                            no_taps = "Default",
                                            gb_ref_given = NULL) {

  if(is.null(gb_ref_given)){
    gb_ref_taken <- gb_ref
  }else{
    check_gb_ref_given(gb_ref_given)
    gb_ref_taken <- gb_ref_given
  }

  duty_factor_table <- gb_ref_taken$duty_factor_lut_grid_prim_tf

  duty_factor_table_tf <- duty_factor_table[1:5,]
  duty_factor_table_tc <- duty_factor_table[6:10,]


  # Transformer
  for (n in 1:nrow(duty_factor_table_tf)){
    if (utilisation_pct == 'Default'){
      duty_factor_tf <- duty_factor_table_tf$`Duty Factor`[nrow(duty_factor_table_tf)]
      break
    } else if (utilisation_pct > as.numeric(duty_factor_table_tf$Lower[n]) &
               utilisation_pct <= as.numeric(duty_factor_table_tf$Upper[n])){
      duty_factor_tf <- duty_factor_table_tf$`Duty Factor`[n]
      break
    }
  }

  # Tapchanger
  for (n in 1:nrow(duty_factor_table_tc)){
    if (no_taps == 'Default'){
      duty_factor_tc <- duty_factor_table_tc$`Duty Factor`[nrow(duty_factor_table_tc)]
      break
    } else if (no_taps > as.numeric(duty_factor_table_tc$Lower[n]) &
               no_taps <= as.numeric(duty_factor_table_tc$Upper[n])){
      duty_factor_tc <- duty_factor_table_tc$`Duty Factor`[n]
      break
    }
  }


  duty_factor <- data.frame(matrix(ncol = 2, nrow = 2) )
  names(duty_factor) <- c("category", "duty_factor")
  duty_factor$category <- c("transformer", "tapchanger")

  duty_factor$duty_factor[which(duty_factor$category == "transformer")] <-
    duty_factor_tf

  duty_factor$duty_factor[which(duty_factor$category == "tapchanger")] <-
    duty_factor_tc


  return(duty_factor = duty_factor)
}
