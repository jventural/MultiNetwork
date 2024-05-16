\name{plots_likert_groups}
\alias{plots_likert_groups}
\title{Plot Likert Scale Responses Grouped by Specified Column}

\description{
This function generates plots for Likert scale responses in a dataset, grouping the data by a specified column. It is designed for visualizing how response distributions for different questionnaire items vary across various demographic or categorical groups within the dataset. This functionality is particularly useful for exploring trends or differences in attitudes or perceptions across groups.
}

\usage{
plots_likert_groups(data, group_column, prefix_item, range_items)
}

\arguments{
  \item{data}{The dataset containing the Likert scale response data.}
  \item{group_column}{The column in the dataset used for grouping the data. Examples include 'gender', 'age_group', 'department', etc.}
  \item{prefix_item}{The prefix of the column names that contain the Likert scale items, allowing for dynamic selection of related columns.}
  \item{range_items}{A vector indicating the range or specific item numbers to be included in the analysis.}
}

\details{
The function begins by identifying all unique values within the specified group column to create subsets of the dataset. For each subset, it filters and removes the group column, then selects the columns based on the given prefix and item range. The function uses the \code{PsyMetricTools::Plot_Likert} for generating individual plots for each group, each plot labeled with the group name. These plots are combined into a grid layout, facilitating easy comparison across groups.
}

\value{
A combined grid plot object containing the Likert scale plots for each group, organized in a grid layout. The number of columns in the grid can be adjusted within the function code.
}

\examples{
# Example usage with a dataset 'Data_New', grouping by 'gender', and analyzing Likert scale items with prefix 'nsss' for items 1-6 and 13-20.
combined_plot <- plots_likert_groups(Data_New, "gender", "nsss", c(1,2,3,4,5,6,13,16,17,18,19,20))

# Display the combined plot
print(combined_plot)
}

\author{
Professor José Ventura León
}
