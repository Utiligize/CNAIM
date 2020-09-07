matrix_adjusted_circles = function(risk_data_matrix, dots_vector, dot_radius){

  risk_data_matrix <- risk_data_matrix %>%
    ungroup() %>%
    mutate(id = row_number())

  dot_radius = tibble::enframe(dot_radius) %>%
    rename(id = name, dot_radius = value)

  risk_data_matrix <- risk_data_matrix %>%
    full_join(dots_vector ) %>%
    mutate(x = ifelse(is.na(x), 'na',x)) %>%
    mutate(y = ifelse(is.na(y), 'na',y)) %>%
    mutate(value = ifelse(is.na(value), 'na',value)) %>%
    mutate(point_x = ifelse(is.na(point_x), 'na',point_x)) %>%
    mutate(point_y = ifelse(is.na(point_y), 'na',point_y)) %>%
    left_join(dot_radius) %>%
    mutate(dot_radius = ifelse(is.na(dot_radius), 'na',dot_radius))

  risk_data_matrix

}


matrix_adjusted_intervals = function(risk_data_matrix, x_intervals, y_intervals){

  risk_data_matrix <- risk_data_matrix %>%
    ungroup() %>%
    mutate(id = row_number())

  x_intervals = tibble::enframe(x_intervals) %>%
    rename(id = name, x_intervals = value)

  y_intervals = tibble::enframe(y_intervals) %>%
    rename(id = name, y_intervals = value)

  risk_data_matrix <- risk_data_matrix %>%
    full_join(x_intervals ) %>%
    mutate(x_intervals = ifelse(is.na(x_intervals), 'na',x_intervals)) %>%
    full_join(y_intervals ) %>%
    mutate(y_intervals = ifelse(is.na(y_intervals), 'na',y_intervals))

  risk_data_matrix

}


matrix_combined = function(risk_data_matrix, dots_vector,
                           dot_radius, x_intervals, y_intervals){

  risk_data_matrix <- risk_data_matrix %>%
    ungroup() %>%
    mutate(id = row_number())

  dot_radius = tibble::enframe(dot_radius) %>%
    rename(id = name, dot_radius = value)

  x_intervals = tibble::enframe(x_intervals) %>%
    rename(id = name, x_intervals = value)

  y_intervals = tibble::enframe(y_intervals) %>%
    rename(id = name, y_intervals = value)

  risk_data_matrix <- risk_data_matrix %>%
    full_join(dots_vector ) %>%
    mutate(x = ifelse(is.na(x), 'na',x)) %>%
    mutate(y = ifelse(is.na(y), 'na',y)) %>%
    mutate(value = ifelse(is.na(value), 'na',value)) %>%
    mutate(point_x = ifelse(is.na(point_x), 'na',point_x)) %>%
    mutate(point_y = ifelse(is.na(point_y), 'na',point_y)) %>%
    left_join(dot_radius) %>%
    mutate(dot_radius = ifelse(is.na(dot_radius), 'na',dot_radius)) %>%
    group_by(x) %>%
    mutate(sums_x = sum(value)) %>%
    ungroup() %>%
    mutate(sums_x = ifelse(y != 1, 'na',sums_x )) %>%
    group_by(y) %>%
    mutate(sums_y = sum(value)) %>%
    ungroup() %>%
    mutate(sums_y = ifelse(x != 1, 'na',sums_y )) %>%
    full_join(x_intervals ) %>%
    mutate(x_intervals = ifelse(is.na(x_intervals), 'na',x_intervals)) %>%
    full_join(y_intervals ) %>%
    mutate(y_intervals = ifelse(is.na(y_intervals), 'na',y_intervals))

  return(risk_data_matrix)
}
