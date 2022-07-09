#' @title Plot of probability of failure
#' @description This function is plotting the probability of failure for an
#' electric network asset in a percentage.
#' @import ggplot2
#' @param pof_function String. Choosing an pof function,  Options:
#' \code{pof_function = c(pof_cables_04kv_pex, pof_cables_10kv_pex, pof_cables_10kv_oil,
#' pof_cables_60_30kv, pof_ohl_cond_50kv, pof_submarine_cables_10kv_oil,
#' pof_submarine_cables_10kv_pex, pof_submarine_cables_30_60kv_oil,
#' pof_submarine_cables_30_60kv_pex, pof_transformer_04_10kv, pof_building,
#' pof_serviceline, "Default")}.
#' @examples
#' # probability of failure curve
# plot_pof(pof_function = pof_cables_04kv_pex)


plot_pof <- function(pof_function = "Default") {

  dat <- data.frame(matrix(ncol=2, nrow = 100))
  names(dat) <- c("age", "pof")
  dat$age <- 1:100

  i = 1
  for(i in 1:100) {

    res <- pof_function(age = i)
    dat$pof[i] <-  res*100
  }

  ggplot(dat, aes(x=age, y=pof)) +
     geom_line(colour="red") + ggtitle("Plotting PoF of default settings for chosen asset" ) # for the main title
}
