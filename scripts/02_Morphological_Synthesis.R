# R script 2/3 for submission "word-internal pauses"

# This script calculates synthesis scores from the raw CSV files from DoReCo

# The _wd csv files can be downloaded from DoReCo (https://doreco.huma-num.fr/) and should be placed in a folder "raw_data"

library(tidyverse)
library(here)

# Read CSV files from DoReCo 1.3
doreco_wd_csv_dir = here("raw_data","wd_csv")
doreco_wd_csv_files <- list.files(path = doreco_wd_csv_dir, pattern = "\\_wd.csv$", full.names = TRUE, recursive = TRUE)
doreco_wd_csv_data <- map_df(doreco_wd_csv_files, ~read_csv(.x, col_types = cols(speaker = col_character())))

# Calculate synthesis index based on mean number of morphs across word types
synthesis <- doreco_wd_csv_data %>%
  select(lang, core_extended, wd, mb, mb_ID, gl) %>%
  filter(!(lang %in% c("goem1240", "komn1238", "nngg1234", "nort2641", "sumi1235", "texi1237", "vera1241"))) %>%
  filter(!is.na(gl)) %>%
  filter(core_extended == "core") %>%
  mutate(next_to_wip = ifelse(
    lead(wd, n = 1) == "<<wip>>" | lead(wd, n = 2) == "<<wip>>" | lag(wd, n = 1) == "<<wip>>" | lag(wd, n = 2) == "<<wip>>", "yes", "no")) %>%
  filter(next_to_wip == "no") %>%
  filter(wd != "<p:>") %>%
  filter(!str_starts(wd, '<<')) %>%
  distinct(lang, wd, mb, .keep_all = T) %>%
  mutate(morph_count = str_count(mb_ID, "m")) %>%
  group_by(lang) %>%
  mutate(synthesis = round(mean(morph_count), 3)) %>%
  summarise(synthesis = unique(synthesis), .groups = 'drop')

# Write to a new CSV file
write_csv(synthesis, here("processed_data", "DoReCo_1_3_core_synthesis.csv"))

