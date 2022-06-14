#' @title Consequences of Failure for a 0.4/10 kV Transformer
#' @description This function calculates consequences of failure
#' @inheritParams f_cof_transformer_11kv
#' @inheritParams s_cof_swg_tf_ohl
#' @inheritParams e_cof_tf
#' @inheritParams n_cof_excl_ehv_132kv_tf
#' @return Numeric. Consequences of failure for a 0.4/10 kV transformer.
#' @examples
#' # Consequences of failure for a 0.4/10 kV transformer
# cof_transformer_04_10kv(kva = 500, type = "Type C",
#                      type_risk = "High", location_risk = "High",
#                      prox_water = 50, bunded = "No",
#                      no_customers = 500, kva_per_customer = 1)


cof_transformer_04_10kv <- function(kva, type,
                                 type_risk, location_risk,
                                 prox_water, bunded,
                                 no_customers, kva_per_customer) {

  GBP_to_DKK <- 8.71
  finance <- f_cof_transformer_11kv(kva, type)

  safety <- s_cof_swg_tf_ohl(type_risk, location_risk,
                             asset_type_scf = "6.6/11kV Transformer (GM)")

  environmental <-  e_cof_tf(asset_type_tf = "6.6/11kV Transformer (GM)",
                             rated_capacity = kva,
                             prox_water, bunded)

  network <-
    n_cof_excl_ehv_132kv_tf(asset_type_ncf = "6.6/11kV Transformer (GM)",
                            no_customers, kva_per_customer)

  return((finance + safety + environmental + network)*GBP_to_DKK)
}
