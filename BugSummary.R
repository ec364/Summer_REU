# Summary Script
# Heili Lowman
# April 7, 2025

# Calculate total counts of insects.

# Load packages and data.
library(tidyverse)
library(here)

df <- read_csv("shiny/hubbard_brook_aquatic_insects/data_raw/sticky_trap_counts.csv")

aq_only <- df %>%
  select(dipteran_large, dipteran_small,
         caddisfly_large, caddisfly_small,
         stonefly_large, 
         mayfly_large)

aq_pivot <- aq_only %>%
  pivot_longer(cols = dipteran_large:mayfly_large, names_to = "ID")

aq_summary <- aq_pivot %>%
  group_by(ID) %>%
  summarize(total_count = sum(value, na.rm = TRUE)) %>%
  ungroup()

# Dipterans account for 97% of aquatic insects recorded!
# 0.1% mayflies
# 0.9% caddisflies
# 1.7% stoneflies

# Total counted as of this dataset - 234,069
# Total counted as of 4/7/25 - 382,868

# End of script.
