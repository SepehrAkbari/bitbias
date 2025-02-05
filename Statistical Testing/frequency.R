library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

num_bins <- 10

breaks <- seq(min(randoms), max(randoms), length.out = num_bins + 1)

freq <- table(cut(randoms, breaks = breaks, include.lowest = TRUE))

expected <- rep(length(randoms) / num_bins, num_bins)

chi_test <- chisq.test(freq, p = expected / sum(expected))

print(chi_test)