Densidad_Grafo_groups <- function(networks_by_sex) {
  library(purrr)
  library(dplyr)
  Densidad_Grafo <- function(edge.matrix, name) {
    n <- nrow(edge.matrix)
    Total_Density <- n * (n - 1) / 2
    Conexiones_Diff_cero <- sum(qgraph::getWmat(edge.matrix) != 0) / 2
    Densidad <- Conexiones_Diff_cero / Total_Density * 100
    resultado <- paste(Conexiones_Diff_cero, " of ", Total_Density, " edges were distinct from zero (", format(Densidad, digits = 2, nsmall = 2), "% of density).", sep = "")

    result_df <- tibble(
      name = name,
      result = resultado
    )

    return(result_df)
  }

  # Aplicar la función y luego convertir los resultados a un tibble
  list_of_densities <- map2_dfr(networks_by_sex, names(networks_by_sex), ~ Densidad_Grafo(.x$graph, name = .y))

  return(list_of_densities)
}
