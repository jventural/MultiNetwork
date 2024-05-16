
\name{Densidad_Grafo_groups}
\alias{Densidad_Grafo_groups}
\title{Calculate Network Density for Different Groups}

\description{
This function calculates and reports the network density for each group specified in the input. Network density is defined as the proportion of actual connections to possible connections in a network, which can provide insights into the complexity and connectivity of the network structure across different groups.
}

\usage{
Densidad_Grafo_groups(networks_by_sex)
}

\arguments{
  \item{networks_by_sex}{A list of network objects by group, where each network is represented by an edge matrix and grouped by a categorical variable such as sex.}
}

\details{
The function internally defines a helper function, \code{Densidad_Grafo}, which calculates the network density for a single network. It computes the total possible connections, the actual non-zero connections, and the percentage density of the network. This helper function is then applied to each group-specific network in the input list using \code{purrr::map2_dfr}, generating a tidy data frame with results for each group.
}

\value{
A tibble (data frame) where each row corresponds to a group and contains the calculated network density results in a readable format, allowing for easy comparison across groups.
}

\examples{
# Assuming you have a list of networks grouped by sex
# Calculate network densities for these groups
density_results <- Densidad_Grafo_groups(my_network_list)
# View the results
print(density_results)
}

\author{
Dr. José Ventura-León
}
