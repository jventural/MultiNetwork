\name{plot_networks_by_sex}
\alias{plot_networks_by_sex}
\title{Plot Network Graphs by Sex}

\description{
This function plots network graphs for two groups categorized by sex, using a consistent layout across the groups to facilitate comparison. It is particularly useful for visualizing and comparing the structure of networks in a gender-based study.
}

\usage{
plot_networks_by_sex(networks_by_sex, output_dir = "Figuras",
                     width = 14, height = 8, units = "in", res = 1000,
                     groups = list("Factor1" = 1:6, "Factor2" = 7:12))
}

\arguments{
  \item{networks_by_sex}{A list containing two network objects, each consisting of an adjacency matrix and labels, with each network corresponding to a different sex.}
  \item{output_dir}{The directory where the output JPEG files will be saved. Defaults to "Figuras".}
  \item{width}{The width of the output JPEG image in inches. Defaults to 14.}
  \item{height}{The height of the output JPEG image in inches. Defaults to 8.}
  \item{units}{The unit of measure used for width and height. Defaults to "in".}
  \item{res}{The resolution of the output JPEG image in DPI. Defaults to 1000.}
  \item{groups}{A list defining node groups for coloring within each network plot. Defaults to two groups named "Factor1" and "Factor2".}
}

\details{
The function first checks if the specified output directory exists and creates it if it doesn't. It then calculates an average layout (`L`) using the first two networks to ensure that node positions are consistent across plots for comparison purposes. Each network is plotted side by side in a single row to compare the network structures directly. Settings for the network appearance, such as colors, labels, and edge characteristics, are configured within the function. After plotting, the graphics device is closed, and default graphics settings are restored.
}

\value{
This function does not return a value but generates JPEG images saved in the specified directory, containing the plotted network graphs.
}

\examples{
# Assuming 'networks_by_sex' is a pre-defined list of network matrices with labels
plot_networks_by_sex(networks_by_sex)
}

\author{
Dr. José Ventura-León
}
