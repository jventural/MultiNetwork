calcular_CS_coefficients <- function(resultados_estabilidad, nombres_groups, filter_ind = "strength") {
  library(tibble)
  library(dplyr)
  library(bootnet)
  # Calcular los coeficientes de CorStability
  CS_coefficients <- lapply(resultados_estabilidad$caseDroppingBoot, function(x) {
    # Asegurarse de que x es adecuado para corStability, este paso puede necesitar ajustes
    corEs <- corStability(x)
    tibble(strength = corEs) %>% round(2)
  }) %>%
    bind_rows(.id = "pair")

  # Seleccionar la información importante y asignar los años, filtrando por el indicador especificado
  CS_coefficients_summary <- stack(CS_coefficients$strength) %>%
    mutate(name = rep(nombres_groups, each = length(CS_coefficients$strength) / length(nombres_groups))) %>%
    filter(ind == filter_ind)

  return(CS_coefficients_summary)
}
