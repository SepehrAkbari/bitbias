library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

randoms1_scaled <- (randoms1 - min(randoms1)) / (max(randoms1) - min(randoms1))
randoms2_scaled <- (randoms2 - min(randoms2)) / (max(randoms2) - min(randoms2))

# lag-1 autocorrelation
lag_1_autocorrelation <- function(data) {
  return(cor(data[-length(data)], data[-1]))
}

serial_test1 <- lag_1_autocorrelation(randoms1_scaled)
serial_test2 <- lag_1_autocorrelation(randoms2_scaled)

cat("Lag-1 autocorrelation for randoms1 (scaled to [0,1]):", serial_test1, "\n")
cat("Lag-1 autocorrelation for randoms2 (scaled to [0,1]):", serial_test2, "\n")

# WOW! but is this lag-1 really a thing?