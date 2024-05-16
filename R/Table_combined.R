Table_combined <- function(Spearman_by_group, strengths_by_group, invariance_test_by_group) {
  library(dplyr)
  library(tidyr)

  # Function to process each matrix
  process_matrix <- function(matrix, value_name) {
    as.data.frame(matrix) %>%
      pivot_longer(cols = everything(), names_to = "Group", values_to = value_name) %>%
      mutate(Test = value_name)
  }

  # Process each input matrix
  spearman_df <- process_matrix(spearman_by_group, "Spearman")
  strengths_df <- process_matrix(strengths_by_group, "Strength")
  invariance_df <- process_matrix(invariance_test_by_group, "Invariance")

  # Combine all data frames
  combined_df <- full_join(spearman_df, strengths_df, by = c("Group", "Test")) %>%
    full_join(., invariance_df, by = c("Group", "Test"))

  # Pivot wider to make it more readable
  wide_data <- combined_df %>%
    pivot_longer(cols = c(Spearman, Strength, Invariance), names_to = "Metric", values_to = "Value") %>%
    filter(!is.na(Value)) %>%
    pivot_wider(names_from = Metric, values_from = Value) %>%
    select(-Test) %>%
    distinct()  # Ensures no duplicate rows # Remove duplicate rows

  return(wide_data)
}
