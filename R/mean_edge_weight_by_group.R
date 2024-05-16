mean_edge_weight_by_group <- function(networks_by_sex) {
  library(purrr)
  library(dplyr)
  mean_edge_weight_mat <- function(adj_matrix, name){
    mean_value <- mean(abs(adj_matrix))
    tibble(name = name, value = mean_value)
  }

  # Aplicar la función y luego combinar los resultados en un tibble
  list_of_means <- map2(networks_by_sex, names(networks_by_sex), ~ mean_edge_weight_mat(.x$graph, .y)) %>%
    bind_rows()

  return(list_of_means)
}
