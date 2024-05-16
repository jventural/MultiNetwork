estimate_networks_by_group <- function(data,
                                       group_var,
                                       item_prefix = "nsss",
                                       default = "ggmModSelect",
                                       stepwise = TRUE,
                                       corMethod = "spearman") {
  library(dplyr)
  library(bootnet)
  # Filtrar y preparar los datos
  unique_groups <- unique(data[[group_var]])
  list_of_data <- data %>%
    filter(!!sym(group_var) %in% unique_groups) %>%
    select(starts_with(item_prefix), !!sym(group_var)) %>% # Utiliza el argumento item_prefix
    group_by(!!sym(group_var)) %>%
    group_split() %>%
    map(~ select(.x, -!!sym(group_var))) %>%
    setNames(unique_groups)

  # Estimar redes por grupo
  list_of_networks <- list_of_data %>%
    map(~ estimateNetwork(.x, default = default, stepwise = stepwise, corMethod = corMethod))

  # Retorna la lista de redes
  return(list_of_networks)
}
