#' @importFrom magrittr %>%
#' @title Current Probability of Failure for RTU
#' @description This function calculates the current
#' annual probability of failure RTU
#' The function is a cubic curve that is based on
#' the first three terms of the Taylor series for an
#' exponential function.
#' @param age  Numeric. The current age in years of the conductor.
#' @inheritParams current_health
#' @param k_value Numeric. \code{k_value = 0.0128} by default. This number is
#' given in a percentage. The default value is accordingly to the CNAIM standard
#' on p. 110.
#' @param c_value Numeric. \code{c_value = 1.087} by default.
#' The default value is accordingly to the CNAIM standard see page 110
#' @param normal_expected_life Numeric. \code{normal_expected_life = 50} by default.
#' The default value is accordingly to the CNAIM standard on page 107.
#' @return Numeric. Current probability of failure per annum.
#' @export
#' @examples
#' # Current annual probability of failure for RTU
#' pof_rtu_res <-
#' pof_rtu(
#' age = 10,
#' k_value = 0.128,
#' c_value = 1.087,
#' normal_expected_life = 20) * 100
#' paste0(sprintf("Probability of failure %.4f", pof_rtu_res),
#' " percent per annum")

pof_rtu <-
  function(age,
           k_value = 0.128,
           c_value = 1.087,
           normal_expected_life = 20) {

    hv_asset_category <- "6.6/11kV CB (GM) Secondary"
    `Asset Register Category` = `Health Index Asset Category` =
      `Generic Term...1` = `Generic Term...2` = `Functional Failure Category` =
      `K-Value (%)` = `C-Value` = `Asset Register  Category` = NULL
    # due to NSE notes in R CMD check

    asset_category <- gb_ref$categorisation_of_assets %>%
      dplyr::filter(`Asset Register Category` ==
                      hv_asset_category) %>%
      dplyr::select(`Health Index Asset Category`) %>% dplyr::pull()

    generic_term_1 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...1`) %>% dplyr::pull()

    generic_term_2 <- gb_ref$generic_terms_for_assets %>%
      dplyr::filter(`Health Index Asset Category` == asset_category) %>%
      dplyr::select(`Generic Term...2`) %>% dplyr::pull()


    # Constants C and K for PoF function --------------------------------------
    k <- k_value/100
    c <- c_value

    # Duty factor -------------------------------------------------------------

    duty_factor_cond <- 1

    # Location factor ----------------------------------------------------
    location_factor_cond <- 1

    # Expected life ------------------------------
    expected_life_years <- expected_life(normal_expected_life,
                                         duty_factor_cond,
                                         location_factor_cond)

    # b1 (Initial Ageing Rate) ------------------------------------------------
    b1 <- beta_1(expected_life_years)

    # Initial health score ----------------------------------------------------
    initial_health_score <- initial_health(b1, age)

    # Current health score ----------------------------------------------------
    current_health_score <-
      current_health(initial_health_score,
                     1,     # Condition Input Factor
                     10,    # Condition Input Cap
                     0.5,   # Condition Input Collar
                     1)     # Realiability factor
    # Probability of failure ---------------------------------------------------
    probability_of_failure <- k *
      (1 + (c * current_health_score) +
         (((c * current_health_score)^2) / factorial(2)) +
         (((c * current_health_score)^3) / factorial(3)))


    return(probability_of_failure)
  }
