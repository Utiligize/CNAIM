#' @title Makes a default risk matrix structure
#' @description This function makes a simple matrix structure that can be used
#' as an input to the risk_matrix_points and risk_matrix_summary functions
#' @param cols Number of columns
#' @param rows Number of rows
#' @param value Default value of each cell
#' @export
#'
risk_matrix_structure <- function(cols,rows,value=NA){
  risk_data_matrix <- expand.grid(x = 1:cols, y = 1:rows)
  risk_data_matrix$value <- value
  return(risk_data_matrix)
}
