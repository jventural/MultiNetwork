\name{Table_combined}
\alias{Table_combined}
\title{Consolidate Network Comparison Results Across Multiple Tests}

\description{
This function compiles and organizes results from multiple network comparison tests—Spearman's correlation, strength tests, and invariance tests—into a single comprehensive table. By integrating results from different tests, it provides a unified view of network comparisons across various metrics, enhancing the analytical utility of network data.
}

\usage{
Table_combined(Spearman_by_group, strengths_by_group, invariance_test_by_group)
}

\arguments{
  \item{Spearman_by_group}{A matrix containing the Spearman correlation results between groups.}
  \item{strengths_by_group}{A matrix detailing the network strengths comparison results between groups.}
  \item{invariance_test_by_group}{A matrix displaying the results of invariance tests between groups.}
}

\details{
The function processes each input matrix separately, converting them from wide format to long format to facilitate data manipulation. It then merges these matrices into a single data frame, ensuring that results from similar groups are aligned together in one row. Subsequently, the data is pivoted wider to create a structured table where each column represents a different test metric, and each row represents a group comparison. This allows for easy interpretation and analysis of multiple network metrics simultaneously.
}

\value{
Produces a cleaned and organized data frame where each row represents a group, and columns represent the results from Spearman's correlation, strength, and invariance tests. This structured format simplifies cross-metric comparisons and enhances the readability of the analysis.
}

\examples{
# Assuming the matrices Spearman_by_group, strengths_by_group, and invariance_test_by_group are predefined:
combined_results <- Table_combined(Spearman_by_group, strengths_by_group, invariance_test_by_group)
print(combined_results)
}

\author{
Dr. José Ventura-León
}
