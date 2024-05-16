max_edge_weight_by_group <- function(networks_by_sex) {
  library(purrr)
  library(dplyr)
  max_edge_weight_mat <- function(adj_matrix, name){
    max_value <- max(abs(adj_matrix))
    tibble(name = name, value = max_value)
  }

  # Aplicar la función y luego combinar los resultados en un tibble
  list_of_max <- map2(networks_by_sex, names(networks_by_sex), ~ max_edge_weight_mat(.x$graph, .y)) %>%
    bind_rows()

  return(list_of_max)
}
