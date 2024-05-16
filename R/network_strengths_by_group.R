network_strengths_by_group <- function(networks) {
  library(NetworkComparisonTest)

  # Extract the data from each network in the list
  data_list <- lapply(networks, function(x) x$data)

  # Use the keys from the list as the group names directly
  names(data_list) <- names(networks)

  # Get all possible combinations of pairs
  combinaciones <- combn(names(data_list), 2)

  # Create an empty matrix to store the results of the comparisons
  comparison_strength <- matrix(NA, nrow = length(data_list), ncol = length(data_list), dimnames = list(names(data_list), names(data_list)))

  # Set seed for reproducibility
  set.seed(2024)

  # Perform comparisons for each pair
  for (i in 1:ncol(combinaciones)) {
    comp_data1 <- data_list[[combinaciones[1, i]]]
    comp_data2 <- data_list[[combinaciones[2, i]]]
    comparacion <- NCT(data1 = comp_data1, data2 = comp_data2, it = 1000, binary.data = FALSE,
                       paired = FALSE, weighted = TRUE, test.edges = TRUE, edges = 'all', progressbar = TRUE)
    comparison_strength[combinaciones[1, i], combinaciones[2, i]] <- round(comparacion$glstrinv.pval, 2)
    comparison_strength[combinaciones[2, i], combinaciones[1, i]] <- round(comparacion$glstrinv.pval, 2)
  }

  # Extract the upper diagonal of the matrix
  comparison_strength_upper <- comparison_strength
  comparison_strength_upper[lower.tri(comparison_strength_upper)] <- NA

  return(comparison_strength_upper)
}
