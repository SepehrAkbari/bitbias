#' Chi-Square Test
#' 
#' 

library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

bins <- cut(randoms, breaks = seq(0, 10, length.out = 11), include.lowest = TRUE)

freq <- table(bins)

expected <- rep(length(randoms) / length(freq), length(freq))

chi_test <- chisq.test(freq, p = expected / sum(expected))

print(chi_test)