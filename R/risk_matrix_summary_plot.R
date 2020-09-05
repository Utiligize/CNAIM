#' @title Make a risk matrix with non-linear spacing
#' @description This function makes a D3 visualization of monetary risk with
#' non-linear x and y intervals.
#' @param risk_data_matrix  Long format matrix data.
#' @param x_intervals An array of x spacing in percent (sum to 100)
#' @param y_intervals An array of y spacing in percent (sum to 100)
#' @export

risk_matrix_summary_plot <- function(risk_data_matrix,
                                x_intervals = rep(20,5),
                                y_intervals = rep(25,4)){

  # risk_matrix_summary(CNAIM:::example_risk_matrix)

  if (is.null(risk_data_matrix$mouse_over_text) &
      max(risk_data_matrix$x) == 5 &
      max(risk_data_matrix$y) == 4){
    risk_data_matrix$mouse_over_text <- example_risk_matrix$mouse_over_text
  }

  # Non-linear bins -------
  x_intervals = x_intervals/sum(x_intervals)
  x_intervals = round(x_intervals,2)

  y_intervals = y_intervals/sum(y_intervals)
  y_intervals = round(y_intervals,2)

  adj_mat_2 = matrix_adjusted_intervals(risk_data_matrix,x_intervals,y_intervals)

  risk_matrix <- r2d3::r2d3(
    data = jsonlite::toJSON(adj_mat_2),
    script = "javascript/src/matrix_upper_level_nonlinear.js",
    dependencies = "javascript/src/matrix_d3script_nonlinear_aa.js",
    container = "div"
  )

  risk_matrix
}
