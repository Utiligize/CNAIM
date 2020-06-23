#' @title Make a risk matrix with individual asset points
#' @description This function makes a D3 visualization of monetary risk with
#' each asset as a point on the grid.
#' @param risk_data_matrix  Long format matrix data.
#' @param dots_vector Coordinates of the dots.
#' @param dot_radius Radius of the dots.
#' @export
#'
risk_matrix_points_plot <- function(risk_data_matrix,
                               dots_vector,
                               dot_radius){

  # risk_matrix_points(CNAIM:::example_risk_matrix)

  adj_mat_1 = matrix_adjusted_circles(risk_data_matrix, dots_vector, dot_radius)

  risk_matrix <- r2d3::r2d3(
    data = jsonlite::toJSON(adj_mat_1),
    script = "javascript/src/matrix_upper_level_circles.js",
    dependencies = "javascript/src/matrix_d3script_circles_aa.js",
    container = "div"
  )

  risk_matrix
}
