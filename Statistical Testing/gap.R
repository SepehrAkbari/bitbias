library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

num_bins <- 10

breaks1 <- seq(min(randoms1), max(randoms1), length.out = num_bins + 1)
breaks2 <- seq(min(randoms2), max(randoms2), length.out = num_bins + 1)

binned1 <- cut(randoms1, breaks = breaks1, include.lowest = TRUE)
binned2 <- cut(randoms2, breaks = breaks2, include.lowest = TRUE)

calculate_gaps <- function(binned_data) {
  bin_indices <- as.integer(binned_data)
  
  gaps <- diff(which(diff(bin_indices) != 0))
  return(gaps)
}

gaps1 <- calculate_gaps(binned1)
gaps2 <- calculate_gaps(binned2)

gap_test1 <- chisq.test(table(gaps1))
gap_test2 <- chisq.test(table(gaps2))

cat("Gap test result for randoms1 (raw data):\n")
print(gap_test1)

cat("Gap test result for randoms2 (raw data):\n")
print(gap_test2)