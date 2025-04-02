# Average frequencies
frequencies <- c(45.5, 44.55, 44.91667, 44.8, 46.13333, 46.31667, 47.06667, 45.03333, 45.13333, 44.58333, 45.96667)

# Calculate the mean
mean_frequency <- mean(frequencies)

# Calculate the standard deviation
std_dev_frequency <- sd(frequencies)

# Print the results
cat("Mean Frequency:", mean_frequency, "\n")
cat("Standard Deviation:", std_dev_frequency, "\n")

# Observed frequencies
observed_freq <- frequencies

# Expected frequency (mean of observed frequencies)
expected_freq <- rep(mean(observed_freq), length(observed_freq))

# Perform chi-square test
chi_test <- chisq.test(observed_freq, p = expected_freq / sum(expected_freq))

# Print the results
cat("Chi-Square Statistic:", chi_test$statistic, "\n")
cat("Degrees of Freedom:", chi_test$parameter, "\n")
cat("P-Value:", chi_test$p.value, "\n")
