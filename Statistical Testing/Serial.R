library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

randoms_scaled <- (randoms - min(randoms)) / (max(randoms) - min(randoms))

# lag-1 autocorrelation
lag_1_autocorrelation <- function(data) {
  return(cor(data[-length(data)], data[-1]))
}

serial_test <- lag_1_autocorrelation(randoms_scaled)

cat("Lag-1 autocorrelation for randoms (scaled to [0,1]):", serial_test, "\n")