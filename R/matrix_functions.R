#' @importFrom magrittr %>%
#' @importFrom dplyr mutate ungroup rename row_number full_join left_join
#' @title Adjust circles for matrix visualization
#' @description This function manipulates the data structure before inputting
#' into javascript D3 risk matrix visualization
#' @param risk_data_matrix  Long format matrix data.
#' @param dots_vector Coordinates of the dots.
#' @param dot_radius Radius of the dots.
#' @export
matrix_adjusted_circles = function(risk_data_matrix, dots_vector, dot_radius){

  name = value = x = y = point_x = point_y = NULL
  # due to NSE notes in R CMD check

  risk_data_matrix <- risk_data_matrix %>%
    ungroup() %>%
    mutate(id = row_number())

  dot_radius = tibble::enframe(dot_radius) %>%
    rename(id = name, dot_radius = value)

  risk_data_matrix <- risk_data_matrix %>%
    full_join(dots_vector ) %>%
    mutate(x = ifelse(is.na(x), 'na', x)) %>%
    mutate(y = ifelse(is.na(y), 'na', y)) %>%
    mutate(value = ifelse(is.na(value), 'na', value)) %>%
    mutate(point_x = ifelse(is.na(point_x), 'na',point_x)) %>%
    mutate(point_y = ifelse(is.na(point_y), 'na',  point_y)) %>%
    left_join(dot_radius) %>%
    mutate(dot_radius = ifelse(is.na(dot_radius), 'na', dot_radius))

  risk_data_matrix
}

#' @importFrom magrittr %>%
#' @title Adjust banding for matrix visualization
#' @description This function manipulates the data structure before inputting
#' into javascript D3 risk matrix visualization
#' @param risk_data_matrix  Long format matrix data.
#' @param x_intervals An array of x spacing in percent (sum to 100)
#' @param y_intervals An array of y spacing in percent (sum to 100)
#' @export
matrix_adjusted_intervals = function(risk_data_matrix, x_intervals, y_intervals){

  name = value = NULL
  # due to NSE notes in R CMD check

  risk_data_matrix <- risk_data_matrix %>%
    ungroup() %>%
    mutate(id = row_number())

  x_intervals = tibble::enframe(x_intervals) %>%
    rename(id = name, x_intervals = value)

  y_intervals = tibble::enframe(y_intervals) %>%
    rename(id = name, y_intervals = value)

  risk_data_matrix <- risk_data_matrix %>%
    full_join(x_intervals ) %>%
    mutate(x_intervals = ifelse(is.na(x_intervals), 'na', x_intervals)) %>%
    full_join(y_intervals ) %>%
    mutate(y_intervals = ifelse(is.na(y_intervals), 'na', y_intervals))

  risk_data_matrix
}
