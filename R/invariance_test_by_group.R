invariance_test_by_group <- function(networks) {
  library(NetworkComparisonTest)

  # Extract the data from each network in the list
  data_list <- lapply(networks, function(x) x$data)

  # Define the names for elements based on the keys of the list
  names(data_list) <- names(networks)

  # Get all possible combinations of pairs
  combinaciones <- combn(names(data_list), 2)

  # Create an empty matrix to store the results of the comparisons
  comparison_topology <- matrix(NA, nrow = length(data_list), ncol = length(data_list), dimnames = list(names(data_list), names(data_list)))

  # Set seed for reproducibility
  set.seed(2023)

  # Perform comparisons for each pair
  for (i in 1:ncol(combinaciones)) {
    data1 <- data_list[[combinaciones[1, i]]]
    data2 <- data_list[[combinaciones[2, i]]]
    comparacion <- NCT(data1 = data1, data2 = data2, it = 1000, binary.data = FALSE, paired = FALSE, weighted = TRUE, test.edges = TRUE, edges = 'all', progressbar = TRUE)
    comparison_topology[combinaciones[1, i], combinaciones[2, i]] <- round(comparacion$nwinv.pval, 2)
    comparison_topology[combinaciones[2, i], combinaciones[1, i]] <- round(comparacion$nwinv.pval, 2)
  }

  # Extract the upper diagonal of the matrix
  comparison_topology_upper <- comparison_topology
  comparison_topology_upper[lower.tri(comparison_topology_upper)] <- NA

  return(comparison_topology_upper)
}
