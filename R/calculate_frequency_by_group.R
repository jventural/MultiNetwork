calculate_frequency_by_group <- function(data, group, prefix) {
  library(dplyr)
  library(psych)
  library(purrr)
  # Genera los grupos únicos de la columna especificada
  groups <- unique(data[[group]])

  # Construye el rango de nombres de columnas basado en el prefijo
  columns <- grep(paste0("^", prefix), names(data), value = TRUE)

  # Aplica las operaciones por cada grupo
  resultados.freq <- map(setNames(groups, groups), ~ data %>%
                           filter(.[[group]] == .x) %>%
                           select(all_of(columns)) %>%
                           psych::responseFrequency() %>%
                           as.data.frame() %>%
                           mutate(across(everything(), ~ round(.x * 100, 2))) %>%
                           select(-miss))

  # Devuelve la lista de data frames con frecuencias por grupo
  return(resultados.freq)
}
