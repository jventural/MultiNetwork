plot_networks_by_sex <- function(networks_by_sex, output_dir = "Figuras",
                                 width = 14, height = 8, units = "in", res = 1000,
                                 groups = list("Factor1" = 1:6, "Factor2" = 7:12)) {
  L <- averageLayout(networks_by_sex[[1]]$graph, networks_by_sex[[2]]$graph)

  Sex <- c("1", "2")
  if (!file.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Configura el dispositivo JPEG para guardar una imagen con ambos gráficos
  jpeg(file.path(output_dir, "Figure_2_izquierda_combinada.jpg"),
       width = width, height = height, units = units, res = res)

  # Configura la disposición de los gráficos en 1 fila y 2 columnas
  par(mfrow = c(1, 2))

  for(i in 1:2) {
    qgraph(networks_by_sex[[i]]$graph,
           layout = L,
           edge.color = "black",
           palette = "ggplot2",
           groups = groups,
           labels = networks_by_sex[[i]][["labels"]],
           title = paste0(Sex[i]),
           title.cex = 2.0,  # Ajusta el tamaño del título aquí
           edge.labels = TRUE,
           edge.label.cex = 1.5,
           edge.label.position = 0.5,
           border.width = 2.5,
           label.cex = 1,
           legend.cex = 0.4,
           vsize = 12,
           curveAll = 2,
           minimum = 0.10)
  }

  # Restablece la configuración gráfica por defecto y cierra el dispositivo
  dev.off()
  par(mfrow = c(1, 1))
}
