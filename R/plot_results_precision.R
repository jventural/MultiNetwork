plot_results_precision <- function(resultados_Precision, nombres_groups, cantidad_grupos) {
  library(ggplot2)
  library(gridExtra)
  outPlots <- list()
  for(i in 1:cantidad_grupos) {
    p = plot(resultados_Precision[[4]][[i]], labels = TRUE, order = 'sample', statistics = 'edge') +
      ggplot2::scale_x_continuous(limits = c(-0.3, 1), breaks = seq(-0.3, 1, by = 0.20),
                                  labels = scales::number_format(accuracy = 0.01)) +
      ggplot2::labs(title = paste0(nombres_groups[i]))
    outPlots[[i]] <- p
    print(p)
  }

  # Combinar y mostrar los plots
  Precision.Plots <- gridExtra::grid.arrange(grobs=outPlots, ncol = 2)
}
