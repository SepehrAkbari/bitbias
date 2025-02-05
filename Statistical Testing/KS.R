library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

randoms1_scaled <- (randoms1 - min(randoms1)) / (max(randoms1) - min(randoms1))
randoms2_scaled <- (randoms2 - min(randoms2)) / (max(randoms2) - min(randoms2))

ks_test1 <- ks.test(randoms1_scaled, "punif") 
ks_test2 <- ks.test(randoms2_scaled, "punif") 

print(ks_test1)
print(ks_test2)