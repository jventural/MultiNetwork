\name{invariance_test_by_group}
\alias{invariance_test_by_group}
\title{Evaluate Network Invariance Across Groups Using Network Comparison Test}

\description{
This function conducts a network invariance test across multiple groups to determine the statistical equivalence of network structures. By employing the Network Comparison Test (NCT), it assesses whether network data from different groups differ significantly, offering insights into the consistency of network characteristics.
}

\usage{
invariance_test_by_group(networks)
}

\arguments{
  \item{networks}{A list containing network objects for each group. Each object must include a component named `data` that contains the network data appropriate for analysis.}
}

\details{
The function extracts network data from each group specified in the `networks` list. It systematically compares all possible pairs of networks to evaluate their invariance. For each pair, the function applies the NCT, which assesses differences in edge weights and overall network structure under non-parametric settings. Results are stored in a symmetric matrix that provides p-values for each comparison, indicating the probability that networks from two different groups are statistically equivalent. The seed for random number generation is set to ensure reproducibility of the test results.
}

\value{
Produces an upper triangular matrix of p-values from the Network Comparison Test. This matrix helps visualize which group comparisons show significant differences, with the lower triangle set to NA to simplify interpretation and focus on unique pairwise comparisons.
}

\examples{
# Assuming list_of_networks is your list containing the network objects for each year
list_of_networks <- list(
  Men = list(data = networks_by_sex[["1"]]$data),
  Women = list(data = networks_by_sex[["2"]]$data))
# Call the function
comparison_results <- invariance_test_by_group(list_of_networks)
print(comparison_results)
}

\author{
Dr. José Ventura-León
}
