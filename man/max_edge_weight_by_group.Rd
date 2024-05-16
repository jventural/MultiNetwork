\name{max_edge_weight_by_group}
\alias{max_edge_weight_by_group}
\title{Calculate Maximum Edge Weight for Networks by Group}

\description{
This function calculates the maximum edge weight for each network within a specified group. Edge weights reflect the strength of the connections between nodes in a network. By determining the maximum edge weight, this function helps highlight the strongest connection within each group's network, useful for comparative analyses across different categorical groups.
}

\usage{
max_edge_weight_by_group(networks_by_sex)
}

\arguments{
  \item{networks_by_sex}{A list where each element represents a network object consisting of an adjacency matrix. Each network corresponds to a group categorized by variables such as sex.}
}

\details{
Within the function, a nested helper function called \code{max_edge_weight_mat} is defined to compute the maximum of the absolute values of the adjacency matrix for a single network. This calculation identifies the strongest connection in terms of weight. The main function applies this helper function to each network in the input list, using \code{purrr::map2} to process networks along with their group names, and then combines the results into a single tibble.
}

\value{
A tibble (data frame) that consolidates the maximum edge weight for each group, making it easy to compare the strongest connections across different groups.
}

\examples{
# Assuming 'networks_by_sex' is a pre-defined list of network matrices grouped by sex
max_weights <- max_edge_weight_by_group(networks_by_sex)
print(max_weights)
}

\author{
Dr. José Ventura-León
}
