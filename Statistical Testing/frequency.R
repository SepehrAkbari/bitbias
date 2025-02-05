# Load necessary libraries
library(dplyr)
library(tidyverse)

# Read the CSV files
randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

# Define bin count (e.g., 10 bins)
num_bins <- 10  

# Create bins for each dataset
breaks1 <- seq(min(randoms1), max(randoms1), length.out = num_bins + 1)
breaks2 <- seq(min(randoms2), max(randoms2), length.out = num_bins + 1)

# Count occurrences in each bin
freq1 <- table(cut(randoms1, breaks = breaks1, include.lowest = TRUE))
freq2 <- table(cut(randoms2, breaks = breaks2, include.lowest = TRUE))

# Expected uniform frequency per bin
expected1 <- rep(length(randoms1) / num_bins, num_bins)
expected2 <- rep(length(randoms2) / num_bins, num_bins)

# Perform chi-square goodness-of-fit test
chi_test1 <- chisq.test(freq1, p = expected1 / sum(expected1))
chi_test2 <- chisq.test(freq2, p = expected2 / sum(expected2))

# Print results
print(chi_test1)
print(chi_test2)