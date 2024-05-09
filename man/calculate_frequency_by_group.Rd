\name{calculate_frequency_by_group}
\alias{calculate_frequency_by_group}
\title{Calculate Response Frequencies by Group in a Dataset}

\description{
This function calculates response frequencies for a specified range of questionnaire items in a dataset, grouping the data based on a specified column. It allows for flexible analysis by enabling the user to specify the column used for grouping and the prefix of the questionnaire items. The function is ideal for exploratory data analysis to examine how response distributions vary across different demographic or categorical groups within the dataset.
}

\usage{
calculate_frequency_by_group(data, group, prefix)
}

\arguments{
  \item{data}{The dataset containing the response data to be analyzed.}
  \item{group}{The column in the dataset used for grouping the data. Examples include 'year', 'sex', 'state', etc.}
  \item{prefix}{The prefix of the columns that contain the questionnaire items. This allows the function to dynamically select columns for analysis.}
}

\details{
The function first identifies all unique values in the specified group column to define the subsets of the dataset. For each subset, it selects the columns starting with the given prefix and calculates response frequencies using the \code{psych::responseFrequency} function. Each frequency is then converted to a percentage, and the results are rounded for clarity. This process is repeated for each group, providing a comprehensive view of how responses vary by group.
}

\value{
A list of data frames, each corresponding to a different group identified by the group column. Each data frame contains the calculated response frequencies as percentages for the specified questionnaire items.
}

\examples{
# Example usage with a dataset 'Data_Final', grouping by 'year', and analyzing items with prefix 'phq'
resultados_freq_year <- calculate_frequency_by_group(Data_Final, "year", "phq")

# To display the frequency results for the year 2020
print(resultados_freq_year[["2020"]])
}

\author{
Professor José Ventura León
}
