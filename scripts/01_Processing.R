# R script 1/3 for submission "word-internal pauses"

library(tidyverse)
library(here)
library(lmerTest)
library(ggsignif)
library(xtable)
library(scales)

# 0) Optionally: Load workspace
#load(here("wordinternalpauses.RData"))



# 1) Read CSV files from DoReCo 2.0 ####

# The ph_csv and wd_csv files can be downloaded from DoReCo (https://doreco.huma-num.fr/) and should be placed in a folder "raw_data"

# Word-level CSV files
doreco_wd_csv_dir = here("raw_data","wd_csv")
doreco_wd_csv_files <- list.files(path = doreco_wd_csv_dir, pattern = "\\_wd.csv$", full.names = TRUE, recursive = TRUE)
doreco_wd_csv_data <- map_df(doreco_wd_csv_files, ~read_csv(.x, col_types = cols(speaker = col_character())))

# Phone-level CSV files
doreco_ph_csv_dir = here("raw_data","ph_csv")
doreco_ph_csv_files <- list.files(path = doreco_ph_csv_dir, pattern = "\\_ph.csv$", full.names = TRUE, recursive = TRUE)
doreco_ph_csv_data <- map_df(doreco_ph_csv_files, ~read_csv(.x, col_types = cols(speaker = col_character())))



# 2) Filter dataframe to contain only rows containing a word-internal pause and two adjacent rows ####
wip_indices <- which(doreco_wd_csv_data$wd == "<<wip>>")
rows_to_keep <- unique(sort(c(wip_indices, wip_indices - 1, wip_indices - 2, wip_indices + 1, wip_indices + 2)))
doreco_wip_and_context <- doreco_wd_csv_data %>%
  select(-ref,-core_extended,-"doreco-mb-algn",-"mc-zero",-refind,-isnref) %>%
  filter(row_number() %in% rows_to_keep)

# Write to file
write.csv(doreco_wip_and_context, file = here("processed_data", "01_Filtering.csv"), row.names = FALSE)



# 3) Manual coding ####
# addition of glosses for files with no or only partial annotation in DoReCo 2.0
# addition of normalized part of speech tags
# saved as "02_Manual_Additions.csv"



# 4) Further preprocessing for analysis ####
manual_additions <- read.csv(here("processed_data", "02_Manual_Additions.csv"), na.strings = "NA")
doreco_data_preprocessed <- manual_additions %>%
  # Pause duration
  mutate(pause_duration = round((end - start)*1000)) %>%  
  # Left and right context
  mutate(previous_1_wd = lag(wd, n = 1),
         previous_2_wd = lag(wd, n = 2),
         following_1_wd = lead(wd, n = 1),
         following_2_wd = lead(wd, n = 2),
         previous_1_mb = lag(mb, n = 1),
         previous_2_mb = lag(mb, n = 2),
         following_1_mb = lead(mb, n = 1),
         following_2_mb = lead(mb, n = 2),
         previous_1_gl = lag(gl, n = 1),
         previous_2_gl = lag(gl, n = 2),
         following_1_gl = lead(gl, n = 1),
         following_2_gl = lead(gl, n = 2),
        ) %>%
  # Normalize context
  mutate(first_part_wd_norm = if_else(startsWith(previous_1_wd, "<<") & (previous_1_mb == "****" | is.na(previous_1_mb)), previous_2_wd, previous_1_wd),
         second_part_wd_norm = if_else(startsWith(following_1_wd, "<<") & (following_1_mb == "****" | is.na(following_1_mb)), following_2_wd, following_1_wd),
         first_part_mb_norm = if_else(startsWith(previous_1_wd, "<<") & (previous_1_mb == "****" | is.na(previous_1_mb)), previous_2_mb, previous_1_mb),
         second_part_mb_norm = if_else(startsWith(following_1_wd, "<<") & (following_1_mb == "****" | is.na(following_1_mb)), following_2_mb, following_1_mb),
         first_part_gl_norm = if_else(startsWith(previous_1_wd, "<<") & (previous_1_mb == "****" | is.na(previous_1_mb)), previous_2_gl, previous_1_gl),
         second_part_gl_norm = if_else(startsWith(following_1_wd, "<<") & (following_1_mb == "****" | is.na(following_1_mb)), following_2_gl, following_1_gl),
        ) %>%
  # Remember disfluencies
  mutate(disfluency_adjacent = ifelse(startsWith(previous_1_wd, "<<fp") | startsWith(previous_2_wd, "<<fp") | startsWith(following_1_wd, "<<fp") | startsWith(following_2_wd, "<<fp") | startsWith(previous_1_wd, "<<fs") | startsWith(previous_2_wd, "<<fs") | startsWith(following_1_wd, "<<fs") | startsWith(following_2_wd, "<<fs"), "yes", "no")) %>%
  # Word duration
  mutate(first_part_wd_norm_duration = if_else(startsWith(previous_1_wd, "<<"), lag(pause_duration, n = 2), lag(pause_duration, n = 1)),
         second_part_wd_norm_duration = if_else(startsWith(following_1_wd, "<<"), lead(pause_duration, n = 2), lead(pause_duration, n = 1)),
         complete_wd_dur = first_part_wd_norm_duration + second_part_wd_norm_duration) %>%
  # Identify pause type (disruptive vs. ultra-disruptive)
  mutate(
    last_mb_previous = word(first_part_mb_norm, -1),
    first_mb_following = word(second_part_mb_norm, 1),
    pause_type = case_when(
      last_mb_previous == "****" | is.na(last_mb_previous) | first_mb_following == "****" | is.na(first_mb_following) ~ NA,
      first_mb_following == last_mb_previous ~ "ultra-disruptive",
      TRUE ~ "disruptive"
    )
  ) %>%
  # Remove columns not needed for further analysis
  select(lang, wd, pause_duration, first_part_wd_norm_duration, second_part_wd_norm_duration, complete_wd_dur, disfluency_adjacent, pause_type, ps, first_part_wd_norm, first_part_mb_norm, first_part_gl_norm, second_part_wd_norm, second_part_mb_norm, second_part_gl_norm) %>%
  # Only keep <<wip>> rows
  filter(wd == "<<wip>>")

# Replace glottocodes with human-readable language names
glottocodes <- c("anal1239", "apah1238", "arap1274", "bain1259", "beja1238", "bora1263", "cabe1245", "cash1254", "dolg1241", "even1259", "goem1240", "goro1270", "hoch1243", "jeha1242", "jeju1234", "kaka1265", "kama1351", "kark1256", "komn1238", "ligh1234", "lowe1385", "movi1243", "ngal1292", "nisv1234", "nngg1234", "nort2641", "nort2875", "orko1234", "pnar1238", "port1286", "resi1247", "ruul1235", "sadu1234", "sanz1248", "savo1255", "sout2856", "sout3282", "stan1290", "sumi1235", "svan1243", "taba1259", "teop1238", "texi1237", "trin1278", "tsim1256", "urum1249", "vera1241", "warl1254", "yong1270", "yuca1254", "yura1255", "toto1304", "guri1247")
language_names <- c("Anal", "Yali", "Arapaho", "Baïnounk Gubëeher", "Beja", "Bora", "Cabécar", "Cashinahua", "Dolgan", "Evenki", "Goemai", "Gorwaa", "Hoocąk", "Jahai", "Jejuan", "Kakabe", "Kamas", "Tabaq", "Komnzo", "Light Warlpiri", "Lower Sorbian", "Movima", "Dalabon", "Nisvai", "Nǁng", "Northern Kurdish (Kurmanji)", "Northern Alta", "Fanbyak", "Pnar", "Daakie", "Resígaro", "Ruuli", "Sadu", "Sanzhi Dargwa", "Savosavo", "Nafsan (South Efate)", "English (Southern England)", "French (Swiss)", "Sümi", "Svan", "Tabasaran", "Teop", "Texistepec Popoluca", "Mojeño Trinitario", "Asimjeeg Datooga", "Urum", "Vera'a", "Warlpiri", "Yongning Na", "Yucatec Maya", "Yurakaré", "Totoli", "Gurindji")
mapping_vector <- setNames(language_names, glottocodes)
doreco_data_preprocessed$language <- mapping_vector[doreco_data_preprocessed$lang]

# Keep track of which languages have at least one <<wip>>
lang_with_wip <- unique(doreco_data_preprocessed$lang)

# Write to file
write.csv(doreco_data_preprocessed, file = here("processed_data", "03_Preprocessed.csv"), row.names = FALSE)
  


# 5) Basic quantitative analysis and plots ####

# Number of wip's per language, percentage of wip's of overall pauses
counting_wips <- doreco_wd_csv_data %>%
  filter(wd %in% c("<p:>", "<<wip>>")) %>%
  group_by(lang) %>%
  summarise(
    number_of_wips = sum(wd == "<<wip>>"),
    total_pauses = sum(wd == "<p:>") + sum(wd == "<<wip>>"),
    percent_of_wips = round((100 * number_of_wips / total_pauses), 2)
  ) %>%
  select(lang, number_of_wips, percent_of_wips) %>%
  arrange((desc(number_of_wips)))

# Summarize info on disruptive vs. ultra-disruptive pauses
disruptive_types <- doreco_data_preprocessed %>%
  group_by(lang, language) %>%
  summarise(disruptive = sum(pause_type == "disruptive", na.rm = T),
            ultra_disruptive = sum(pause_type == "ultra-disruptive", na.rm = T)
            
  )

# Merge with info on disruptive vs. ultra-disruptive pauses
counting_wips_plus <- left_join(counting_wips, disruptive_types, by = "lang") %>%
  select(language, number_of_wips, percent_of_wips, disruptive, ultra_disruptive) %>%
  filter(!is.na(language)) %>%
  mutate(language = ifelse(language == "English (Southern England)", "English", language))
  
# Sort the languages by the number of "<<wip>>" entries in descending order
ordered_lang <- counting_wips_plus %>%
  pull(language)

# Visualize wip counts per language
counting_wips_plus_pivot <- counting_wips_plus %>%
  pivot_longer(cols = c(disruptive, ultra_disruptive), names_to = "pause_type", values_to = "count")
wip_count_plot <- ggplot(counting_wips_plus_pivot,
                         aes(y = factor(language, levels = rev(ordered_lang)), x = count, fill = pause_type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("disruptive" = "grey", "ultra_disruptive" = "black")) +
  labs(y = "Language", x = "Count", title = "Number of <<wip>> per language") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=24), legend.position = "none")
ggsave(here("images", "wip-count-per-lg.png"), plot = wip_count_plot, width = 12, height = 12, units = "in", dpi = 300)

# Combined density plot for pause durations
combined_density_plot <- ggplot() +
  # Density line for cleaned_pause_data (regular speech pauses)
  geom_density(data = cleaned_pause_data, aes(x = pause_duration, color = "general"), 
               alpha = 0.7, linewidth = 1.5) +
  # Density line for doreco_data_preprocessed (wip's)
  geom_density(data = doreco_data_preprocessed, aes(x = pause_duration, color = "wip"), 
               alpha = 0.7, linewidth = 1.5) +
  scale_color_manual(values = c("general" = "black", "wip" = "lightgrey")) +
  xlim(0, 2500) +
  theme_minimal() +
  theme(text = element_text(size = 24), legend.position = "none") +
  labs(title = "Density plot for word-internal and regular pauses",
       x = "Duration (msec)", y = "Density", color = "Dataset")
ggsave(here("images", "density-plot.png"), plot = combined_density_plot, width = 12, height = 12, units = "in", dpi = 300)

# Proportion of <<wip>> in various duration groups
duration_groups_wip <- doreco_data_preprocessed %>%
  summarise(below_500 = sum(pause_duration < 500),
            between_500_and_1000 = sum(pause_duration >= 500 & pause_duration <= 1000),
            above_1000 = sum(pause_duration > 1000)) %>%
  mutate(total = between_500_and_1000 + below_500 + above_1000) %>%
  bind_rows(
    mutate(
      .,
      across(c(below_500, between_500_and_1000, above_1000), 
             ~ (. / total) * 100, .names = "{.col}"), 
      total = 100,
      row_type = "percentages"
    )
  ) %>%
  mutate(row_type = replace_na(row_type, "counts"))

# Duration of regular silent pauses
languages_with_wip <- c("anal1239", "arap1274", "bain1259", "beja1238", "bora1263", "cabe1245", "cash1254", "dolg1241", "even1259", "goro1270", "guri1247", "hoch1243", "jeha1242", "jeju1234", "kaka1265", "kama1351", "kark1256", "ligh1234", "lowe1385", "movi1243", "ngal1292", "nisv1234", 
                        "nort2875", "orko1234", "pnar1238", "port1286", "resi1247", "ruul1235", "sadu1234", "sanz1248", "savo1255", "sout2856", "sout3282", 
                        "svan1243", "taba1259", "teop1238", "toto1304", "trin1278", "tsim1256", "urum1249", "warl1254", "yong1270", "yura1255")
cleaned_pause_data <- doreco_ph_csv_data %>%
  mutate(pause_duration = round(1000*(end-start))) %>%
  group_by(lang,file,speaker) %>%
  filter(start != 0 & end != max(end)) %>%
  filter(wd == "<p:>") %>%
  ungroup() %>%
  filter(lang %in% languages_with_wip)

duration_groups_pause <- cleaned_pause_data %>%
  summarise(below_500 = sum(pause_duration < 500),
            between_500_and_1000 = sum(pause_duration >= 500 & pause_duration <= 1000),
            above_1000 = sum(pause_duration > 1000)) %>%
  mutate(total = between_500_and_1000 + below_500 + above_1000) %>%
  bind_rows(
    mutate(
      .,
      across(c(below_500, between_500_and_1000, above_1000), 
             ~ (. / total) * 100, .names = "{.col}"), 
      total = 100,
      row_type = "percentages"
    )
  ) %>%
  mutate(row_type = replace_na(row_type, "counts"))

# parts of speech: descriptive stats
pos_stats <- doreco_data_preprocessed %>%
  mutate(ps = ifelse(ps == "pro", "other", ps)) %>%
  count(ps) %>%
  mutate(percentages = round(100 * (n / sum(n)), 2)) %>%
  mutate(ps = factor(ps, levels = c("verb", "noun", "other", NA))) %>%
  arrange(ps)

# Proportions of verbs in the 10 lgs with the most wips
top_10_langs <- doreco_data_preprocessed %>%
  count(lang, sort = TRUE) %>%
  slice_max(n, n = 10) 

top_10_pivot <- doreco_data_preprocessed %>%
  filter(!is.na(ps)) %>%
  mutate(ps = ifelse(ps == "pro", "other", ps)) %>%
  filter(lang %in% top_10_langs$lang) %>%
  group_by(lang, ps) %>%
  summarize(count = n()) %>%
  ungroup()
top_10_pivot$ps <- factor(top_10_pivot$ps, levels = c("other", "noun", "verb"))

pos_plot_top_10 <- ggplot(top_10_pivot,
                         aes(y = factor(lang, levels = rev(top_10_langs$lang)), x = count, fill = ps)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("verb" = "black", "noun" = "darkgrey", "other" = "lightgrey")) +
  labs(y = "Language", x = "Count", title = "Parts of speech in <<wip>>") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text=element_text(size=24), legend.position = "none")
ggsave(here("images", "pos-top-10.png"), plot = pos_plot_top_10, width = 12, height = 12, units = "in", dpi = 300)


# 6) Disfluencies ####

# Define segments in the vicinity of (word-internal) pauses
vicinity <- doreco_wd_csv_data %>%
  filter(core_extended == "core") %>%
  filter(lang %in% lang_with_wip) %>%
  group_by(lang, file, speaker) %>%
  mutate(
  vicinity_pause = lag(ph, 1) == "<p:>" | lead(ph, 1) == "<p:>" | lag(ph, 2) == "<p:>" | lead(ph, 2) == "<p:>",
  vicinity_wip = lag(ph, 1) == "<<wip>>" | lead(ph, 1) == "<<wip>>" | lag(ph, 2) == "<<wip>>" | lead(ph, 2) == "<<wip>>"
  ) %>%
  ungroup()

# Count occurrences of disfluencies in the vicinity of (word-internal) pauses
counts_disfluencies <- vicinity %>%
  filter(ph %in% c("<<fp>>", "<<fs>>")) %>%
  summarise(
    count_p = sum(vicinity_pause, na.rm = TRUE),
    count_wip = sum(vicinity_wip, na.rm = TRUE)
  )

# Compute likelihoods
total_p_vicinity = sum(vicinity$vicinity_pause, na.rm = TRUE)
total_wip_vicinity = sum(vicinity$vicinity_wip, na.rm = TRUE)
likelihood_p = counts_disfluencies$count_p / total_p_vicinity
likelihood_wip = counts_disfluencies$count_wip / total_wip_vicinity

# Compute log-likelihood
log_likelihood = log(likelihood_p / likelihood_wip)

# Output results
list(
  counts = counts_disfluencies,
  likelihood_p = likelihood_p,
  likelihood_wip = likelihood_wip,
  log_likelihood = log_likelihood
)

# 7) Synthesis ####

# Read synthesis score table created with 02_Morphological_Synthesis.R
synthesis_table <- read.csv(here("processed_data", "DoReCo_1_3_core_synthesis.csv"))

# Merging and normalizing
wip_counts_and_synthesis <- left_join(synthesis_table, counting_wips, by = "lang")  

# Plot: Proportion of wips relative to synthesis score
synthesis_plot <- ggplot(wip_counts_and_synthesis, aes(y = synthesis, x = percent_of_wips)) +
  geom_point(color = "black", size = 3, show.legend = F) +
  geom_smooth(method = "lm", color = "black", linewidth = 2, se = TRUE) +
  theme_minimal() +
  theme(text = element_text(size = 24),
        legend.position = "none") +
  scale_x_log10(breaks = c(0, 0.1, 1),
                labels = c("0%", "0.1%", "1%"))+
  labs(
    title = "Morphological synthesis and word-internal pauses",
    y = "Synthesis score",
    x = "Proportion of word-internal pauses among all pauses"
  )
ggsave(here("images", "synthesis.png"), plot = synthesis_plot, width = 12, height = 12, units = "in", dpi = 300)

# Pearson's Correlation Coefficient
pearson <- cor(log(wip_counts_and_synthesis$percent_of_wips), wip_counts_and_synthesis$synthesis)



# 8) Morph type ####
morph_type_data <- doreco_data_preprocessed %>%
  mutate(mb_left_of_wip = word(first_part_mb_norm, -1),
         mb_right_of_wip = word(second_part_mb_norm, 1),
         mt_left_of_wip = case_when(
            str_detect(mb_left_of_wip, "\\*{4}") ~ NA,
            (str_starts(mb_left_of_wip, "[-~]") & str_ends(mb_left_of_wip, "[-~]")) ~ "infix",
            str_ends(mb_left_of_wip, "[-~]") ~ "prefix",
            str_ends(mb_left_of_wip, "=") ~ "proclitic",
            str_starts(mb_left_of_wip, "[-~]") ~ "suffix",
            str_starts(mb_left_of_wip, "=") ~ "enclitic",
            TRUE ~ "root"
                                  ),
         mt_right_of_wip = case_when(
           str_detect(mb_right_of_wip, "\\*{4}") ~ NA,
           (str_starts(mb_right_of_wip, "[-~]") & str_ends(mb_right_of_wip, "[-~]")) ~ "infix",
           str_ends(mb_right_of_wip, "[-~]") ~ "prefix",
           str_ends(mb_right_of_wip, "=") ~ "proclitic",
           str_starts(mb_right_of_wip, "[-~]") ~ "suffix",
           str_starts(mb_right_of_wip, "=") ~ "enclitic",
           TRUE ~ "root"
                                ),
         mt_context = case_when(
           is.na(mt_left_of_wip) ~ NA,
           is.na(mt_right_of_wip) ~ NA,
           TRUE ~ paste(mt_left_of_wip, mt_right_of_wip, sep = "/")
                              )
        )

# Disruptive pauses
combinations <- c("proclitic/proclitic", "proclitic/prefix", "prefix/prefix", "prefix/root", "root/root", "root/suffix", "suffix/suffix", "suffix/enclitic", "enclitic/enclitic")

morph_type_count_disr <- morph_type_data %>%
  filter(!is.na(mt_context)) %>%
  filter(pause_type == "disruptive" & mt_context %in% combinations) %>%
  count(mt_context) %>%
  mutate(mt_context = factor(mt_context, levels = combinations))

barplot_morphtype_disruptive <- ggplot(data = morph_type_count_disr, aes(y = n, x = mt_context)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 24),
        legend.position = "none") +
  labs(
    title = "Morph combinations split by disruptive pauses",
    y = "Count",
    x = "Morph type combination"
  )

ggsave(here("images", "morph-type-disr.png"), plot = barplot_morphtype_disruptive, width = 12, height = 12, units = "in", dpi = 300)

# Ultra-disruptive pauses
morph_types = c("proclitic", "prefix", "root", "suffix", "enclitic")

morph_type_count_ultr <- morph_type_data %>%
  filter(!is.na(mt_context)) %>%
  filter(pause_type == "ultra-disruptive") %>%
  count(mt_right_of_wip) %>%
  mutate(mt_right_of_wip = factor(mt_right_of_wip, levels = morph_types))

barplot_morphtype_ultra_disruptive <- ggplot(data = morph_type_count_ultr, aes(y = n, x = mt_right_of_wip)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 24),
        legend.position = "none") +
  labs(
    title = "Morph types split by ultra-disruptive pauses",
    y = "Count",
    x = "Morph type"
  )

ggsave(here("images", "morph-type-ultr.png"), plot = barplot_morphtype_ultra_disruptive, width = 12, height = 12, units = "in", dpi = 300)



# 9) Final Lengthening ####

# X-SAMPA vowel base characters
vowel_list <- c("a", "A", "E", "e", "I", "i", "O", "o", "U", "u", 
                "Y", "y", "@", "1", "2", "3", "6", "7", "8", "9", 
                "V", "&", "{", "}", "Q", "M")

# Add classification for vowels
final_lengthening_data <- doreco_ph_csv_data %>%
  # Discard columns not needed for analysis
  select(-ref,-tx,-core_extended,-ft,-mb,-mb_ID,-gl,-ps,-"doreco-mb-algn",-"mc-zero",-refind,-isnref) %>%
  # Basic preprocessing
  filter(lang %in% lang_with_wip) %>%
  group_by(file, speaker) %>%
  mutate(
    # Duration in milliseconds
    duration = round((end - start) * 1000),    
    # Determine if the segment is a vowel, a (word-internal) pause, or something else
    segment_type = ifelse(ph %in% vowel_list, "vowel", ifelse(ph %in% c("<p:>", "<<wip>>"), "pause", "not_vowel")),
    next_ph = lead(ph),
    # IPUs
    pause_group = cumsum(segment_type == "pause")) %>%
  # Position relative to pauses
  group_by(file, speaker, pause_group) %>%
  mutate(sum_vowels = sum(segment_type == "vowel", na.rm = TRUE)) %>%
  filter(sum_vowels > 0) %>%
  mutate(
    counting_vowels = ifelse(segment_type == "vowel", row_number(), NA_integer_),
    max_count = if_else(sum_vowels > 0, max(counting_vowels, na.rm = TRUE), NA_real_),
    pause_type = last(next_ph),
    position = case_when(
      counting_vowels == max_count ~ ifelse(pause_type == "<p:>", "before_pause", 
                                            ifelse(pause_type == "<<wip>>", "before_wip", NA_character_)),
      segment_type == "vowel" ~ "not_final",
      TRUE ~ NA_character_),
    speech_rate = ifelse(sum(duration) - duration == 0, NA, round(1000*((n() - 1) / (sum(duration) - duration)), 3))
    ) %>%
  # Right context (as a proxy for syllable structure)
  group_by(file, speaker, wd_ID) %>%
  mutate(
    context = ifelse(row_number() == n(), "_#", 
              ifelse(lead(segment_type, n = 1) == "vowel", "_V",
              ifelse(lead(segment_type, n = 1) == "not_vowel" & row_number() + 1 == n(), "_C",
              ifelse(lead(segment_type, n = 1) == "not_vowel" & lead(segment_type, n = 2) == "not_vowel", "_CC",
              ifelse(lead(segment_type, n = 1) == "not_vowel", "_C", NA)))))
  ) %>%
  # Remove non-vowels and vowels without a valid position
  ungroup() %>%
  filter(segment_type == "vowel" & !is.na(position)) %>%
  # Normalize speaker codes 
  mutate(speaker = paste(lang, speaker, sep = "_")) %>%
  # Remove vowels shorter than 40 ms
  filter(duration >= 40)

# Plotting vowel durations as a function of position
final_lengthening_plot <- final_lengthening_data
final_lengthening_plot$position <- factor(final_lengthening_plot$position, levels = c("not_final", "before_pause", "before_wip"))
boxplot_lengthening <- ggplot(final_lengthening_plot, aes(x = position, y = duration, fill = position)) +
  geom_boxplot() +
  scale_fill_manual(values = c("not_final" = "black", "before_pause" = "darkgrey", "before_wip" = "lightgrey")) +
  theme_minimal() +
  theme(text = element_text(size = 24),
        legend.position = "none") +
  labs(
    title = "Vowel duration by position",
    x = "Position",
    y = "Duration (milliseconds)"
  ) +
  coord_cartesian(ylim = c(0, 250)) +
  geom_signif(
  comparisons = list(c("not_final", "before_pause"), c("before_pause", "before_wip")),
  map_signif_level = TRUE,
  textsize = 6,
  y_position = c(175, 175),
  tip_length = 0.02
)

ggsave(here("images", "finallengthening.png"), plot = boxplot_lengthening, width = 12, height = 12, units = "in", dpi = 300)


# Median duration values
final_lengthening_plot %>%
  group_by(position) %>%
  summarise(median_duration = median(duration, na.rm = TRUE))

# Linear mixed model to test the effect of position on vowel duration
lmm <- lmer(log(duration) ~ position + ph + speech_rate + context + (1 | speaker) + (1 | lang), data = final_lengthening_data)

# Model summary
summary(lmm)

# Inspect residuals
residuals_lmm <- residuals(lmm)
qqnorm(residuals_lmm)
qqline(residuals_lmm, col = "red")



# 10) Optional: Save workspace
#save.image(here("wordinternalpauses.RData"))


