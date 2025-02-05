library(dplyr)
library(tidyverse)

randoms1 <- read.csv("Data/randoms1.csv")$n
randoms2 <- read.csv("Data/randoms2.csv")$n

randoms1_scaled <- (randoms1 - min(randoms1)) / (max(randoms1) - min(randoms1))
randoms2_scaled <- (randoms2 - min(randoms2)) / (max(randoms2) - min(randoms2))

# Shannon entropy = H(x) = sum(p(x_i) * log2(p(x_i)))
shannon_entropy <- function(data) {
  freq_table <- table(data)
  
  prob <- freq_table / sum(freq_table)
  
  entropy <- -sum(prob * log2(prob))
  
  return(entropy)
}

entropy1 <- shannon_entropy(randoms1_scaled)
entropy2 <- shannon_entropy(randoms2_scaled)

cat("Entropy for randoms1 (scaled to [0,1]):", entropy1, "\n")
cat("Entropy for randoms2 (scaled to [0,1]):", entropy2, "\n")

# higher the entropy lower the periodicity (higher the randomness??predictability)