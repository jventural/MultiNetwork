\name{plot_centrality_by_group}
\alias{plot_centrality_by_group}
\title{Plot Centrality Measures for Networks by Group}

\description{
This function plots centrality measures for network graphs, categorized by group. It is designed to visualize differences in node centrality across specified network groups, aiding in the analysis of node importance or influence within different network structures.
}

\usage{
plot_centrality_by_group(networks_by_sex, replacements, measure_spec)
}

\arguments{
  \item{networks_by_sex}{A list of network objects, each represented by an adjacency matrix, categorized by variables such as sex.}
  \item{replacements}{A vector of replacement names for the groups to be used in the plot labels, replacing default names like 'graph 1' and 'graph 2'.}
  \item{measure_spec}{The specific centrality measure to be plotted, such as 'degree', 'closeness', or 'betweenness'.}
}

\details{
The function first computes centrality measures for each network using the `centralityTable` function. It then renames the groups based on user-provided replacements and filters the centrality measures according to the specified type. The result is a data frame that is used to create a line and point plot with `ggplot2`, highlighting the differences in centrality across groups. The plot is flipped horizontally for better visualization of the nodes along the y-axis.
}

\value{
A ggplot object that visually represents the centrality measures across different groups, making it easy to compare the centrality metrics visually.
}

\examples{
# Assuming 'networks_by_sex' is a list of network matrices and 'replacements' is c("Men", "Women")
plot_centrality_by_group(networks_by_sex, c("Men", "Women"), "degree")
}

\author{
Dr. José Ventura-León
}
