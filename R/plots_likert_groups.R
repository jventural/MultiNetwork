plots_likert_groups <- function(data, group_column, prefix_item, range_items) {
  library(dplyr)
  library(PsyMetricTools)
  library(gridExtra)
  library(ggplot2)

  # Lista para almacenar los plots
  plot_list <- list()

  # Obtener los valores únicos del grupo directamente dentro de la función
  group_values <- unique(data[[group_column]])

  # Bucle para generar los gráficos para cada grupo y guardarlos en la lista
  for (group in group_values) {
    df_group <- data %>%
      filter(!!sym(group_column) == group) %>%  # Usar el nombre de la columna dinámicamente
      select(-!!sym(group_column))             # Excluir la columna de grupo después de filtrar

    # Construir los nombres de las columnas de los ítems basados en el prefijo y el rango proporcionado
    items <- paste0(prefix_item, range_items)

    plot <- Plot_Likert(df_group, prefix_item, range_items, exclude = NULL) +
      labs(x = "Response rates")

    # Agregar el nombre del grupo al plot
    plot <- plot + labs(title = paste(group))

    plot_list[[as.character(group)]] <- plot  # Asegúrate de convertir el grupo a caracter si es numérico
  }

  # Organizar y mostrar los plots en una única grilla
  combined_plot <- grid.arrange(grobs = plot_list, ncol = 2)

  return(combined_plot)
}
