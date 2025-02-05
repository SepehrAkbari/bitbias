library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

randoms_scaled <- (randoms - min(randoms)) / (max(randoms) - min(randoms))

ks_test <- ks.test(randoms_scaled, "punif")

print(ks_test)