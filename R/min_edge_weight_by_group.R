min_edge_weight_by_group <- function(networks_by_sex) {
  library(purrr)
  library(dplyr)
  min_edge_weight_mat <- function(adj_matrix, name){
    # Reemplazar ceros con NA
    adj_matrix_no_zeros <- replace(adj_matrix, adj_matrix == 0, NA)

    # Obtener el valor mínimo absoluto no-NA
    min_value <- min(abs(adj_matrix_no_zeros), na.rm = TRUE)

    # Crear un data frame con el resultado
    tibble(name = name, value = min_value)
  }

  # Aplicar la función y luego combinar los resultados en un tibble
  list_of_min <- map2(networks_by_sex, names(networks_by_sex), ~ min_edge_weight_mat(.x$graph, .y)) %>%
    bind_rows()

  return(list_of_min)
}
