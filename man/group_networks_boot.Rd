\name{group_networks_boot}
\alias{group_networks_boot}
\title{Estimate and Bootstrap Network Models by Group}

\description{
This function preprocesses the input dataset and estimates network models for specified groups. It then performs a bootstrapping procedure to assess the stability and accuracy of the network estimates, including measures such as edge weights, node strength, and expected influence within each group.
}

\usage{
group_networks_boot(Data_Final, variables, variable_agrupacion, communities,
                    nBoots = 1000, nCores = 1, default = 'ggmModSelect',
                    stepwise = TRUE, corMethod = "spearman", type = "case")
}

\arguments{
  \item{Data_Final}{The dataset containing the variables of interest.}
  \item{variables}{A vector of names for the variables included in the network analysis.}
  \item{variable_agrupacion}{The name of the variable used to group the data for separate network analyses.}
  \item{communities}{A list that specifies the node communities for network partitioning in the analysis.}
  \item{nBoots}{The number of bootstrap samples to generate. Default is 1000.}
  \item{nCores}{The number of cores to use for parallel computation. Default is 1.}
  \item{default}{The default estimation method for the network analysis. Default is 'ggmModSelect'.}
  \item{stepwise}{Logical, indicating whether stepwise model selection should be applied. Default is TRUE.}
  \item{corMethod}{The correlation method to be used in the network estimation. Default is "spearman".}
  \item{type}{The type of bootstrap to perform. Default is "case".}
}

\details{
The function begins by selecting the necessary variables and grouping the dataset according to the specified grouping variable. It then processes each group to handle missing data and estimates network models using the specified parameters. After network estimation, it applies a bootstrapping procedure to assess the stability of various network statistics, which is crucial for validating the inferred network structures.
}

\value{
A data frame containing the results from the network estimation and bootstrapping for each group, which includes metrics like edge stability and centrality measures.
}

\examples{
# Define node communities and variables to include
communities = list("Self" = 1:6, "In the couple" = 7:12)
variables = Data_new %>% select(nsss1:nsss20) %>% names()

# Run network estimation and bootstrapping
resultados_estabilidad = group_networks_boot(Data_new,
                                             variables, "sexo",
                                             communities,
                                             nBoots = 1000,
                                             nCores = 12,
                                             default = 'ggmModSelect',
                                             stepwise = TRUE,
                                             corMethod = "spearman",
                                             type = "case")
}

\author{
Dr. José Ventura-León
}
