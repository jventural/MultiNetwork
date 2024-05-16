\name{spearman_matrix_by_groups}
\alias{spearman_matrix_by_groups}
\title{Compare Network Data Across Groups Using Spearman's Correlation}

\description{
This function performs a comparative analysis of network data across multiple groups using the Spearman's rank correlation method. By implementing the Network Comparison Test (NCT), the function evaluates the statistical differences in network structures among the groups, providing a detailed assessment of network relationships.
}

\usage{
spearman_matrix_by_groups(networks)
}

\arguments{
  \item{networks}{A list containing network objects for each group. Each object must include a component named `data` that contains the network data appropriate for analysis. This list should have named elements that identify each group, which are used in the analysis to label the results.}
}

\details{
Upon extracting network data from the provided list of groups, the function computes all possible pairwise comparisons among these groups. Each pair undergoes a Network Comparison Test (NCT) to determine significant differences in their network's Spearman correlation coefficients. The function is configured to handle weighted, non-binary data and includes all edges in its testing scope, ensuring a comprehensive network structure analysis. Results are displayed in a symmetric matrix with p-values, signifying the strength and significance of the pairwise comparisons.
}

\value{
Returns an upper triangular matrix of p-values from the Spearman's correlation comparisons among the groups. The matrix simplifies the comparison process by setting the lower triangle to NA, focusing only on unique pairwise results and reducing visual redundancy.
}

\examples{
# Define a list of networks with respective group names
networks <- list(
  Men = list(data = networks_by_sex[["1"]]$data),
  Women = list(data = networks_by_sex[["2"]]$data)
)

# Execute the function
spearman_results <- spearman_matrix_by_groups(networks)
print(spearman_results)
}

\author{
Dr. José Ventura-León
}
