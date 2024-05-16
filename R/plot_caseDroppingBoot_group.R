plot_caseDroppingBoot_group <- function(results, nombres_groups,statistics, cantidad_grupos) {
  library(ggplot2)
  library(gridExtra)
  # Lista para almacenar los plots
  plot_caseDroppingBoot = list()

  # Bucle para generar los plots
  for (i in 1:cantidad_grupos) {
    p = plot(results$caseDroppingBoot[[i]], statistics = statistics) +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.10),
                         labels = scales::number_format(accuracy = 0.01)) +
      labs(title = paste0(nombres_groups[i]))

    plot_caseDroppingBoot[[i]] = p
    print(p)
  }

  # Usar grid.arrange para combinar los plots
  do.call(grid.arrange, c(plot_caseDroppingBoot))
}
