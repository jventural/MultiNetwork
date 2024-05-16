plot_centrality_by_group <- function(networks_by_sex, replacements, measure_spec) {
  library(dplyr)
  library(ggplot2)
  # Generar la tabla de centralidad y ajustarla
  cents <- centralityTable(networks_by_sex[[1]]$graph, networks_by_sex[[2]]$graph) %>%
    rename(group = graph) %>%
    mutate(group = case_when(
      group == "graph 1" ~ replacements[1],
      group == "graph 2" ~ replacements[2],
      TRUE ~ group  # Mantener el valor original si no es "graph 1" o "graph 2"
    )) %>%
    select(-type) %>%
    filter(measure == measure_spec)

  # Crear el gráfico utilizando ggplot
  Figure2 <- ggplot(data = cents, aes(x = reorder(node, value), y = value, group = group, color = group)) +
    geom_line(aes(linetype = group)) +
    geom_point(aes(shape = group, fill = group), size = 3) +
    xlab(" ") + ylab(paste(unique(cents$measure))) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 1),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 11)) +
    coord_flip()

  # Retornar el gráfico creado
  return(Figure2)
}
