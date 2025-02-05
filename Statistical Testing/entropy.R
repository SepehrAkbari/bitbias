library(dplyr)
library(tidyverse)

randoms <- read.csv("Data/randoms2.csv")$n

randoms_scaled <- (randoms - min(randoms)) / (max(randoms) - min(randoms))

# Shannon entropy = H(x) = sum(p(x_i) * log2(p(x_i)))
shannon_entropy <- function(data) {
  freq_table <- table(data)
  
  prob <- freq_table / sum(freq_table)
  
  entropy <- -sum(prob * log2(prob))
  
  return(entropy)
}

entropy <- shannon_entropy(randoms_scaled)

cat("Entropy for randoms (scaled to [0,1]):", entropy, "\n")