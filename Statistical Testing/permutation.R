library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

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

perm_test1 <- permutation_test(randoms1, block_size)
perm_test2 <- permutation_test(randoms2, block_size)

cat("Permutation test result for randoms1:\n")
cat("Observed Statistic:", perm_test1$observed_stat, "\n")
cat("P-value:", perm_test1$p_value, "\n")

cat("\nPermutation test result for randoms2:\n")
cat("Observed Statistic:", perm_test2$observed_stat, "\n")
cat("P-value:", perm_test2$p_value, "\n")