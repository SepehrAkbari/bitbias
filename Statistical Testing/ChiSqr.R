library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

bins1 <- cut(randoms1, breaks = seq(0, 1000, length.out = 11), include.lowest = TRUE)
bins2 <- cut(randoms2, breaks = seq(0, 10, length.out = 11), include.lowest = TRUE)

freq1 <- table(bins1)
freq2 <- table(bins2)

expected1 <- rep(length(randoms1) / length(freq1), length(freq1))
expected2 <- rep(length(randoms2) / length(freq2), length(freq2))

chi_test1 <- chisq.test(freq1, p = expected1 / sum(expected1))
chi_test2 <- chisq.test(freq2, p = expected2 / sum(expected2))

print(chi_test1)
print(chi_test2)