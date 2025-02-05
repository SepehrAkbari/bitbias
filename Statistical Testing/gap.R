library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

num_bins <- 10

breaks <- seq(min(randoms), max(randoms), length.out = num_bins + 1)

binned <- cut(randoms, breaks = breaks, include.lowest = TRUE)

calculate_gaps <- function(binned_data) {
  bin_indices <- as.integer(binned_data)
  
  gaps <- diff(which(diff(bin_indices) != 0))
  return(gaps)
}

gaps <- calculate_gaps(binned)

gap_test <- chisq.test(table(gaps))

cat("Gap test result for randoms:\n")
print(gap_test)