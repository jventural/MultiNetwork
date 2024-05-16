\name{calcular_CS_coefficients}
\alias{calcular_CS_coefficients}
\title{Calculate CorStability Coefficients for Network Stability Analysis}

\description{
This function calculates the CorStability coefficients from bootstrap results for network stability analysis. CorStability coefficients measure the stability of estimated network statistics, such as strength, across bootstrap samples.
}

\usage{
calcular_CS_coefficients(resultados_estabilidad, nombres_groups, filter_ind = "strength")
}

\arguments{
  \item{resultados_estabilidad}{A list containing the results of bootstrap analyses, specifically the `caseDroppingBoot` output from the `bootnet` package.}
  \item{nombres_groups}{A vector of names corresponding to the groups in the bootstrap results. These names are used for labeling in the summary output.}
  \item{filter_ind}{The specific statistic to filter for in the stability results. Default is 'strength', but other indices like 'betweenness' or 'closeness' could also be used if included in the bootstrap analysis.}
}

\details{
The function begins by iterating over the `caseDroppingBoot` results within `resultados_estabilidad`, applying the `corStability` function to each. It extracts the CorStability coefficients, which are then rounded for clarity and compiled into a tibble. This tibble is then processed to include group names and filtered by the specified statistic for detailed analysis. The resulting summary table is particularly useful for comparing the stability of network statistics across different groups or conditions.
}

\value{
A tibble summarizing the CorStability coefficients for the specified statistic across groups, making it easy to compare stability across different network configurations.
}

\examples{
# Assuming 'resultados_estabilidad' contains the bootnet output with caseDroppingBoot results
summary_table <- calcular_CS_coefficients(resultados_estabilidad, c("Group1", "Group2"), "strength")
print(summary_table)
}

\author{
Dr. José Ventura-León
}
