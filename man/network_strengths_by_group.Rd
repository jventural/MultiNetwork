\name{network_strengths_by_group}
\alias{network_strengths_by_group}
\title{Compare Network Strength Across Groups Using Network Comparison Test}

\description{
This function assesses the network strength across multiple groups by utilizing the Network Comparison Test (NCT). It constructs a matrix of p-values that reflect the statistical significance of differences in network connections between each pair of groups.
}

\usage{
network_strengths_by_group(networks)
}

\arguments{
  \item{networks}{A list containing the network data for each group. Each element in the list must be an object that includes a component named `data` with the network data.}
}

\details{
The function starts by extracting the network data from each group specified in the `networks` list. It then identifies all possible pairs of data sets to compare across the provided networks. For each pair, the function performs the Network Comparison Test to evaluate significant differences in network strength, utilizing settings that cater to both binary and weighted data analysis. The results of these comparisons are stored in a symmetric matrix, where each entry corresponds to the p-value of the comparison between two groups. To ensure reproducibility, a random seed is set before the comparisons begin.
}

\value{
Returns an upper triangular matrix with p-values from the Network Comparison Test, facilitating easy comparison of network strength differences between groups. The lower triangle of the matrix is set to NA to avoid redundancy.
}

\examples{
# Define the list of networks with direct group naming
networks <- list(
  Men = list(data = networks_by_sex[["1"]]$data),
  Women = list(data = networks_by_sex[["2"]]$data)
)

# Running the comparison without specifying group names
result <- compare_network_strengths(networks)
print(result)
}

\author{
Dr. José Ventura-León
}
