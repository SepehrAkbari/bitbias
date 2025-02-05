library(dplyr)
library(tidyverse)

randoms2 <- read.csv("Data/randoms2.csv")$n

randoms_scaled <- (randoms - min(randoms)) / (max(randoms) - min(randoms))

empirical_mean <- function(x) {
  return(mean(x)) 
}

# its over [0,1]
integral_f <- function() {
  return(0.5) 
}

empirical_mean <- empirical_mean(randoms_scaled)

cat("Empirical mean for randoms (scaled to [0,1]):", empirical_mean, "\n")

cat("Expected integral value of f(x) = x over [0,1]:", integral_f(), "\n")

diff <- abs(empirical_mean - integral_f())

cat("Difference for randoms:", diff, "\n")