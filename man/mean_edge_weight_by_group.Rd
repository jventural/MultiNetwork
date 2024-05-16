\name{mean_edge_weight_by_group}
\alias{mean_edge_weight_by_group}
\title{Calculate Mean Edge Weight for Networks by Group}

\description{
This function calculates the mean edge weight for each group-specific network provided in the input. Edge weights in a network can provide insights into the strength and significance of the connections between nodes, making this function particularly useful for comparative analysis across different categorical groups.
}

\usage{
mean_edge_weight_by_group(networks_by_sex)
}

\arguments{
  \item{networks_by_sex}{A list where each element is a network object consisting of an adjacency matrix, with each network corresponding to a different group categorized by a variable such as sex.}
}

\details{
The function includes a nested helper function, \code{mean_edge_weight_mat}, which computes the mean of the absolute values of the adjacency matrix for a single network. This mean represents the average strength of connections in the network. The main function applies this helper function to each network in the list using \code{purrr::map2}, which also passes the names of the groups. It then aggregates the results into a single tibble for easy comparison.
}

\value{
A tibble (data frame) that consolidates the mean edge weight for each group, facilitating straightforward comparisons across different groups.
}

\examples{
# Assuming 'networks_by_sex' is a pre-defined list of network matrices by group
mean_weights <- mean_edge_weight_by_group(networks_by_sex)
print(mean_weights)
}

\author{
Dr. José Ventura-León
}
