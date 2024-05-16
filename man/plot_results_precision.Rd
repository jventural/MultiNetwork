\name{plot_results_precision}
\alias{plot_results_precision}
\title{Plot Precision Results for Network Groups}

\description{
This function visualizes the precision results of network analyses for specified groups. It creates individual plots for each group, displaying the precision of network edges as estimated from statistical analyses, and combines all plots into a single composite display for easy comparison.
}

\usage{
plot_results_precision(resultados_Precision, nombres_groups, cantidad_grupos)
}

\arguments{
  \item{resultados_Precision}{A list containing the precision results for each group, where each element corresponds to the output from statistical analysis like bootstrapping or precision calculations.}
  \item{nombres_groups}{A vector of names for the groups, used for labeling the plots.}
  \item{cantidad_grupos}{The number of groups for which the results are available and plots will be generated.}
}

\details{
The function iterates through the precision results for the specified number of groups. For each group, it generates a plot using `ggplot2`, setting up the x-axis to reflect a range of precision values and configuring the plot to display labels and statistics related to network edges. The individual plots are then combined using `gridExtra::grid.arrange` into a layout suitable for presentation, facilitating direct comparison of precision across different groups or conditions.
}

\value{
The function does not return a value but directly outputs a grid of plots to the R graphical device for display.
}

\examples{
# Assuming 'resultados_Precision' contains precision results for network analysis
# and 'nombres_groups' has been defined for two groups
plot_results_precision(resultados_Precision, c("Group1", "Group2"), 2)
}

\author{
Dr. José Ventura-León
}
