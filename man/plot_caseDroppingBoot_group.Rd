\name{plot_caseDroppingBoot_group}
\alias{plot_caseDroppingBoot_group}
\title{Plot Bootstrap Results for Network Groups}

\description{
This function visualizes the results of case-dropping bootstrap analyses for specified network groups. It creates individual plots for each group, displaying stability estimates for network statistics such as edge weights or node strength, and then combines all plots into a single display for comparative analysis.
}

\usage{
plot_caseDroppingBoot_group(results, nombres_groups, statistics, cantidad_grupos)
}

\arguments{
  \item{results}{A data frame or list containing the results of the bootstrap analysis for each group.}
  \item{nombres_groups}{A vector of names for the groups, used for labeling the plots.}
  \item{statistics}{The network statistics to plot, such as 'edge' or 'strength'.}
  \item{cantidad_grupos}{The number of groups to plot, which should correspond to the length of the results list.}
}

\details{
For each group specified by `cantidad_grupos`, the function retrieves the bootstrap results from the `results` object, applies ggplot2 to create a plot for the specified `statistics`, and adjusts the y-axis to range from -1 to 1 for consistency across plots. Titles are added based on `nombres_groups`. Finally, all plots are arranged into a grid using `gridExtra::grid.arrange`, allowing for easy visual comparison across different groups.
}

\value{
The function does not return a value but outputs a grid of plots directly to the R graphical device for display.
}

\examples{
# Assuming 'results' is pre-loaded with bootstrap results, and you have names and statistics defined
plot_caseDroppingBoot_group(results, c("Group1", "Group2"), "edge", 2)
}

\author{
Dr. José Ventura-León
}
