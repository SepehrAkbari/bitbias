# Package installation
required_packages <- c("dplyr", "tidyverse")
installed_packages <- installed.packages()[, "Package"]
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
}

# Load libraries
library(dplyr)
library(tidyverse)

# Chi-squared test for uniformity
chisqr_test <- function(data, min_val = 0, max_val = 10) {
  bins <- cut(data, breaks = seq(min_val, max_val + 1, by = 1), 
              include.lowest = TRUE, right = FALSE)
  freq <- table(bins)
  expected <- rep(length(data) / (max_val - min_val + 1), max_val - min_val + 1)
  chi_test <- chisq.test(freq, p = expected / sum(expected))
  return(c(chisqr_p = chi_test$p.value,
           chisqr_X2 = chi_test$statistic,
           chisqr_df = chi_test$parameter))
}

# Kolmogorov-Smirnov test
ks_test <- function(data, min_val = 0, max_val = 10) {
  data_scaled <- (data - min_val) / (max_val - min_val)
  ks_test <- ks.test(data_scaled, "punif", min = 0, max = 1)
  return(c(ks_p = ks_test$p.value,
           ks_D = ks_test$statistic))
}

# Frequency test with chi-squared
freq_test <- function(data, min_val = 0, max_val = 10, num_bins = 11) {
  breaks <- seq(min_val, max_val + 1, length.out = num_bins + 1)
  freq <- table(cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE))
  expected <- rep(length(data) / num_bins, num_bins)
  chi_test <- chisq.test(freq, p = expected / sum(expected))
  return(c(freq_p = chi_test$p.value,
           freq_X2 = chi_test$statistic,
           freq_df = chi_test$parameter))
}

# Equal distribution test
eqdist_test <- function(data, min_val = 0, max_val = 10) {
  data_scaled <- (data - min_val) / (max_val - min_val)
  empirical_mean_val <- mean(data_scaled)
  expected_mean <- 0.5
  diff <- abs(empirical_mean_val - expected_mean)
  return(c(eqdist_empiricalMean = empirical_mean_val,
           eqdist_diff = diff))
}

# Gap test
gap_test <- function(data, min_val = 0, max_val = 10, num_bins = 11) {
  breaks <- seq(min_val, max_val + 1, length.out = num_bins + 1)
  binned <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = FALSE)
  calculate_gaps <- function(binned_data) {
    gaps_list <- list()
    for (bin in unique(binned_data)) {
      indices <- which(binned_data == bin)
      if (length(indices) > 1) {
        gaps <- diff(indices)
        gaps_list <- c(gaps_list, gaps)
      }
    }
    return(unlist(gaps_list))
  }
  gaps <- calculate_gaps(binned)
  if (length(gaps) == 0) {
    return(c(gap_p = NA, gap_X2 = NA, gap_df = NA))
  }
  gap_freq <- table(gaps)
  expected <- rep(sum(gap_freq) / length(gap_freq), length(gap_freq))
  gap_test <- chisq.test(gap_freq, p = expected / sum(expected))
  return(c(gap_p = gap_test$p.value,
           gap_X2 = gap_test$statistic,
           gap_df = gap_test$parameter))
}

# Serial correlation test
serial_test <- function(data, min_val = 0, max_val = 10) {
  data_scaled <- (data - min_val) / (max_val - min_val)
  if (var(data) == 0) {
    return(c(serial_autocorrelation = NA))
  }
  serial_test <- cor(data_scaled[-length(data_scaled)], data_scaled[-1])
  return(c(serial_autocorrelation = serial_test))
}

# Permutation test
permute_test <- function(data, block_size = 5) {
  calculate_statistic <- function(data, block_size) {
    num_blocks <- length(data) %/% block_size
    data_trimmed <- data[1:(num_blocks * block_size)]
    blocks <- matrix(data_trimmed, nrow = block_size, byrow = TRUE)
    block_means <- colMeans(blocks)
    return(mean(block_means))
  }
  permutation_test <- function(data, block_size, num_permutations = 1000) {
    observed_stat <- calculate_statistic(data, block_size)
    permuted_stats <- replicate(num_permutations, {
      permuted_data <- sample(data)
      calculate_statistic(permuted_data, block_size)
    })
    p_value <- mean(abs(permuted_stats - mean(permuted_stats)) >= 
                    abs(observed_stat - mean(permuted_stats)))
    return(c(perm_observed_stat = observed_stat, perm_p = p_value))
  }
  return(permutation_test(data, block_size))
}

# Entropy test
entropy_test <- function(data, min_val = 0, max_val = 10, num_bins = 11) {
  breaks <- seq(min_val, max_val + 1, length.out = num_bins + 1)
  bins <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
  freq_table <- table(bins)
  probabilities <- freq_table / sum(freq_table)
  entropy_value <- -sum(probabilities * log2(probabilities + 1e-10))
  return(c(entropy_val = entropy_value))
}

# Fourier transform test
ftt_test <- function(data) {
  n <- length(data)
  fft_result <- fft(data)
  magnitudes <- Mod(fft_result)
  frequencies <- (0:(n-1)) / n
  half_n <- floor(n / 2)
  if (half_n < 2) {
    return(c(fft_dominant_frequency = NA,
             fft_dominant_period = NA,
             fft_max_magnitude = NA))
  }
  dominant_index <- which.max(magnitudes[2:(half_n + 1)]) + 1
  dominant_magnitude <- magnitudes[dominant_index]
  dominant_frequency <- frequencies[dominant_index]
  dominant_period <- if (dominant_frequency > 0) 1 / dominant_frequency else NA
  return(c(fft_dominant_frequency = dominant_frequency,
           fft_dominant_period = dominant_period,
           fft_max_magnitude = dominant_magnitude))
}

### Main Function ###
main <- function() {
  print("Starting feature extraction...")
  print("Current directory is:")
  print(getwd())

  sequences <- list.files(path = "Data", pattern = "\\.csv$", full.names = TRUE)
  print("Got sequences...")

  result_list <- list()

  for (i in seq_along(sequences)) {
    print(paste("Processing sequence", i, "of", length(sequences)))
    file <- basename(sequences[i])
    seq_label <- tools::file_path_sans_ext(file)
    seq_num <- as.integer(sub("randoms-", "", seq_label))

    source <- if (seq_num <= 60) "QRNG" else "PRNG"
    generator <- case_when(
      seq_num <= 60 ~ "IBM Qiskit (Single-Qubit)",
      seq_num <= 80 ~ "Mersenne Twister (MT19937)",
      seq_num <= 100 ~ "Linear Congruential Generator (LCG)",
      seq_num <= 120 ~ "XORShift",
      TRUE ~ NA_character_
    )

    data <- read.csv(sequences[i], header = TRUE)
    seq_numbers <- data$n

    res_chisqr <- chisqr_test(seq_numbers)
    res_ks <- ks_test(seq_numbers)
    res_freq <- freq_test(seq_numbers)
    res_eqdist <- eqdist_test(seq_numbers)
    res_gap <- gap_test(seq_numbers)
    res_serial <- serial_test(seq_numbers)
    res_permute <- permute_test(seq_numbers)
    res_entropy <- entropy_test(seq_numbers)
    res_ftt <- ftt_test(seq_numbers)

    result_vector <- c(index = i,
                       sequence_label = seq_label,
                       source = source,
                       generator = generator,
                       res_chisqr,
                       res_ks,
                       res_freq,
                       res_eqdist,
                       res_gap,
                       res_serial,
                       res_permute,
                       res_entropy,
                       res_ftt)

    result_list[[i]] <- as.data.frame(t(result_vector),
                                      stringsAsFactors = FALSE)
  }

  features_df <- bind_rows(result_list) %>%
    mutate(across(where(is.character) & !c("sequence_label",
                                           "source",
                                           "generator"),
                  as.numeric))

  print("Saving features dataframe...")
  write.csv(features_df, "feature_vector.csv", row.names = FALSE)
  print("Feature extraction completed and saved to feature_vector.csv")

  return(features_df)
}

# Execute main function
main()