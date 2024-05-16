\name{estimate_networks_by_group}
\alias{estimate_networks_by_group}
\title{Estimate Psychological Networks by Group}

\description{
This function estimates psychological networks for different groups within a dataset. It filters the data by a specified group variable and analyzes the network structure for each unique group using specified item variables. This is particularly useful in psychological or sociological research where understanding the interconnections between variables within distinct groups is needed.
}

\usage{
estimate_networks_by_group(data, group_var, item_prefix = "nsss", default = "ggmModSelect", stepwise = TRUE, corMethod = "spearman")
}

\arguments{
  \item{data}{The dataset containing the variables of interest.}
  \item{group_var}{The column in the dataset used for grouping the data, such as 'gender', 'age_group', or 'condition'.}
  \item{item_prefix}{The prefix of column names to be included in the network analysis, allowing dynamic selection based on naming conventions. Default is 'nsss'.}
  \item{default}{The default estimation method for network analysis, set to 'ggmModSelect' by default.}
  \item{stepwise}{Logical, indicating whether stepwise model selection should be applied. Default is TRUE.}
  \item{corMethod}{The method of correlation to be used, with 'spearman' as the default.}
}

\details{
The function starts by identifying unique values within the group variable to segment the dataset. It then applies a network estimation procedure on each subset, removing the group variable and selecting the relevant items based on the specified prefix. This method allows for the comparison of network structures across different groups, adjusted for the specified correlation method and model selection process.
}

\value{
A list of network objects for each group, where each network represents the estimated relationships between the selected items within the group.
}

\examples{
# Estimate networks by gender for items starting with 'nsss' in a dataset 'my_data'
network_results <- estimate_networks_by_group(my_data, "gender", "nsss")
# Print the results for inspection
print(network_results)
}

\author{
Dr. José Ventura León
}
