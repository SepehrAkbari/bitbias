required_packages <- c("dplyr", "tidyverse")
installed_packages <- installed.packages()[, "Package"]
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
}

library(dplyr)
library(tidyverse)

### Uniformity Test ###

chisqr_test <- function(data, min_val = 0, max_val = 10) {
  bins <- cut(data,
              breaks = seq(min_val, max_val, length.out = 11),
              include.lowest = TRUE)
  freq <- table(bins)
  expected <- rep(length(data) / length(freq), length(freq))
  chi_test <- chisq.test(freq, p = expected / sum(expected))
  return(c(chirsqr_p = chi_test$p.value, 
           chisqr_X2 = chi_test$statistic, 
           chisqr_df = chi_test$parameter))
}

ks_test <- function(data, min_val = 0, max_val = 10) {
  data_scaled <- (data - min(data)) / (max(data) - min(data))
  ks_test <- ks.test(data_scaled, "punif")
  return(c(ks_p = ks_test$p.value, 
           ks_D = ks_test$statistic))
}

freq_test <- function(data, min_val = 0, max_val = 10, num_bins = 10) {
  breaks <- seq(min(data),
                max(data),
                length.out = num_bins + 1)
  freq <- table(cut(data,
                    breaks = breaks,
                    include.lowest = TRUE))
  expected <- rep(length(data) / num_bins, num_bins)
  chi_test <- chisq.test(freq, p = expected / sum(expected))
  return(c(freq_p = chi_test$p.value, 
           freq_X2 = chi_test$statistic, 
           freq_df = chi_test$parameter))
}

eqdist_test <- function(data, min_val = 0, max_val = 10) {
  data_scaled <- (data - min(data)) / (max(data) - min(data))
  empirical_mean <- function(x) {
    return(mean(x))
  }
  empirical_mean_val <- empirical_mean(data_scaled)
  integral_f <- function() {
    return(0.5)
  }
  diff <- abs(empirical_mean_val - integral_f())
  return(c(eqdist_diff = diff, 
           eqdist_empiricalMean = empirical_mean_val))
}

### Pattern Test ###

gap_test <- function(data, num_bins = 10) {
  breaks <- seq(min(data), max(data), length.out = num_bins + 1)
  binned <- cut(data, breaks = breaks, include.lowest = TRUE, labels = FALSE)
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
  gap_freq <- table(gaps)
  expected <- rep(sum(gap_freq) / length(gap_freq), length(gap_freq))
  gap_test <- chisq.test(gap_freq, p = expected / sum(expected))
  return(c(gap_p = gap_test$p.value, 
           gap_X2 = gap_test$statistic, 
           gap_df = gap_test$parameter))
}

serial_test <- function(data) {
  data_scaled <- (data - min(data)) / (max(data) - min(data))
  lag_1_autocorrelation <- function(data) {
    if (var(data) == 0) {
      return(NA)
    }
    return(cor(data[-length(data)], data[-1]))
  }
  serial_test <- lag_1_autocorrelation(data_scaled)
  return(c(serial_autocorrelation = serial_test))
}

permute_test <- function(data, block_size = 5) {
  calculate_statistic <- function(data, block_size) {
    num_blocks <- length(data) %/% block_size
    data_trimmed <- data[1:(num_blocks * block_size)]
    blocks <- matrix(data_trimmed, nrow = block_size, byrow = TRUE)
    block_means <- colMeans(blocks)
    return(mean(block_means))
  }
  observed_stat <- calculate_statistic(data, block_size)
  permutation_test <- function(data, block_size, num_permutations = 1000) {
    observed_stat <- calculate_statistic(data, block_size)
    permuted_stats <- replicate(num_permutations, {
      permuted_data <- sample(data)
      calculate_statistic(permuted_data, block_size)
    })
    p_value <- mean(abs(permuted_stats) >= abs(observed_stat))
    return(list(observed_stat = observed_stat,
                p_value = p_value,
                permuted_stats = permuted_stats))
  }
  perm_test <- permutation_test(data, block_size)
  return(c(perm_observed_stat = perm_test$observed_stat, 
           perm_p = perm_test$p_value))
}

### Periodicity Test ###

entropy_test <- function(data, num_bins = 10) {
  bins <- cut(data, breaks = num_bins, include.lowest = TRUE)
  freq_table <- table(bins)
  probabilities <- freq_table / sum(freq_table)
  entropy_value <- -sum(probabilities * log2(probabilities))
  return(c(entropy_val = entropy_value))
}

ftt_test <- function(data) {
  n <- length(data)
  fft_result <- fft(data)
  magnitudes <- Mod(fft_result)
  frequencies <- (0:(n-1)) / n
  half_n <- floor(n / 2)

  if (half_n < 2) {
    warning("Not enough data points for Fourier analysis.")
    return(NA)
  }

  dominant_index <- which.max(magnitudes[2:(half_n + 1)]) + 1
  dominant_magnitude <- magnitudes[dominant_index]
  dominant_frequency <- frequencies[dominant_index]

  dominant_period <- if (dominant_frequency > 0) 1 / dominant_frequency else NA

  return(c(fft_dominant_frequency = dominant_frequency,
           fft_dominant_period = dominant_period,
           fft_max_magnitude = dominant_magnitude))
}

### MAIN ###

main <- function() {
  print("Starting...")
  print("current directory is:")
  print(getwd())
  sequences <- list.files(path = "Data", pattern = "\\.csv$")
  print("Got sequences...")

  result_list <- list()

  for (i in seq_along(sequences)) {
    print(paste("Processing sequence", i, "of", length(sequences)))
    file <- sequences[i]
    seq_label <- tools::file_path_sans_ext(file)

    data <- read.csv(file.path("Data", file), header = TRUE)
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

    result_vector <- c(sequence_label = seq_label,
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

  features_df <- dplyr::bind_rows(result_list)

  print("Saving features datafram")

  write.csv(features_df, "features.csv", row.names = FALSE)

  print("Feature extraction completed and saved to features.csv")

  return(features_df)
}

main()