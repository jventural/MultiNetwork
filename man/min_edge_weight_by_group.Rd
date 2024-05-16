\name{min_edge_weight_by_group}
\alias{min_edge_weight_by_group}
\title{Calculate Minimum Non-Zero Edge Weight for Networks by Group}

\description{
This function calculates the minimum non-zero edge weight for each network within a specified group. By focusing on non-zero weights, this function effectively highlights the weakest active connections in each network, which can be crucial for understanding the underlying structure and dynamics of the network in different groups.
}

\usage{
min_edge_weight_by_group(networks_by_sex)
}

\arguments{
  \item{networks_by_sex}{A list of network objects, each represented by an adjacency matrix and categorized by a variable such as sex.}
}

\details{
The function includes a nested helper function, \code{min_edge_weight_mat}, which processes an adjacency matrix to calculate the minimum absolute value of the edge weights, ignoring zero values (treated as non-connections or missing data). Zeros in the adjacency matrix are replaced with NA to exclude them from the calculation. The main function applies this helper function to each network using \code{purrr::map2}, passing both the networks and their corresponding group names, and then compiles the results into a single tibble.
}

\value{
A tibble (data frame) that provides the minimum non-zero edge weight for each group, allowing for easy comparison of the weakest active connections across different groups.
}

\examples{
# Assuming 'networks_by_sex' is a list of network matrices grouped by sex
min_weights <- min_edge_weight_by_group(networks_by_sex)
print(min_weights)
}

\author{
Dr. José Ventura-León
}
