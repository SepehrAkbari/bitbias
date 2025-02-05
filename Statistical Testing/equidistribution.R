library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

randoms1_scaled <- (randoms1 - min(randoms1)) / (max(randoms1) - min(randoms1))
randoms2_scaled <- (randoms2 - min(randoms2)) / (max(randoms2) - min(randoms2))

empirical_mean <- function(x) {
  return(mean(x)) 
}

# its over [0,1]
integral_f <- function() {
  return(0.5) 
}

empirical_mean1 <- empirical_mean(randoms1_scaled)
empirical_mean2 <- empirical_mean(randoms2_scaled)

cat("Empirical mean for randoms1 (scaled to [0,1]):", empirical_mean1, "\n")
cat("Empirical mean for randoms2 (scaled to [0,1]):", empirical_mean2, "\n")

cat("Expected integral value of f(x) = x over [0,1]:", integral_f(), "\n")

diff1 <- abs(empirical_mean1 - integral_f())
diff2 <- abs(empirical_mean2 - integral_f())

cat("Difference for randoms1:", diff1, "\n")
cat("Difference for randoms2:", diff2, "\n")