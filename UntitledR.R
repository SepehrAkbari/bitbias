library(tidyverse)
library(readxl)
library(dplyr)

fv <- read_csv("Data/feature_vector.csv")

fv_by_source <- fv %>%
  group_by(source) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  select(-index, -sequence_label, -generator)

fv_by_generator <- fv %>%
  group_by(generator) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  select(-source, -index, -sequence_label)

write_csv(fv_by_source, "Data/fv_by_source.csv")
write_csv(fv_by_generator, "Data/fv_by_generator.csv")