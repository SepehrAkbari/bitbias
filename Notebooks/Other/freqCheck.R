library(tidyverse)

# Initialize a list to store the datasets
randoms_list <- list()

# Read all the datasets into the list
for (i in 61:80) {
  file_name <- paste0("Data/MT-RN/randoms-", i, ".csv")
  randoms_list[[i]] <- read_csv(file_name)
}

calculate_frequency <- function(data) {
  data %>%
    count(n) %>%
    spread(n, nn, fill = 0)
}
frequency_list <- lapply(randoms_list, calculate_frequency)

frequency_df <- bind_rows(frequency_list)

average_frequency <- frequency_df %>%
  summarise(across(starts_with("0"):starts_with("10"), mean, na.rm = TRUE))

print(average_frequency)
