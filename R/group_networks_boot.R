group_networks_boot <- function(Data_Final, variables, variable_agrupacion, communities,
                                nBoots = 1000, nCores = 1, default = 'ggmModSelect',
                                stepwise = TRUE, corMethod = "spearman", type = "case") {
  library(dplyr)
  library(purrr)
  library(bootnet)
  library(qgraph)

  # Preprocesar y estimar redes por grupo
  resultados <- Data_Final %>%
    select(all_of(variables), all_of(variable_agrupacion)) %>%
    group_by(!!sym(variable_agrupacion)) %>%
    group_nest() %>%
    mutate(data = map(data, ~ select(., where(~ !all(is.na(.))))),
           Estimacion = map(data, ~ estimateNetwork(.x, default = default,
                                                    stepwise = stepwise, corMethod = corMethod,
                                                    tuning = 0, criterion = "ebic")),
           caseDroppingBoot = map(Estimacion, ~ bootnet(.x, nBoots = nBoots, nCores = nCores,
                                                        type = type, default = default,
                                                        corMethod = corMethod,
                                                        statistics = c("edge", "strength", "expectedInfluence"),
                                                        communities = communities)))
  return(resultados)
}
