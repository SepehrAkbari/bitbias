library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

block_size <- 5

calculate_statistic <- function(data, block_size) {
  blocks <- split(data, ceiling(seq_along(data) / block_size))
  
  block_means <- sapply(blocks, mean)
  return(mean(block_means))
}

permutation_test <- function(data, block_size, num_permutations = 1000) {
  observed_stat <- calculate_statistic(data, block_size)
  
  permuted_stats <- replicate(num_permutations, {
    permuted_data <- sample(data)
    calculate_statistic(permuted_data, block_size)
  })
  
  p_value <- mean(abs(permuted_stats) >= abs(observed_stat))
  
  return(list(observed_stat = observed_stat, p_value = p_value, permuted_stats = permuted_stats))
}

perm_test <- permutation_test(randoms, block_size)

cat("\nPermutation test result for randoms:\n")
cat("Observed Statistic:", perm_test$observed_stat, "\n")
cat("P-value:", perm_test$p_value, "\n")