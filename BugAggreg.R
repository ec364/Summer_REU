####Load in Data Sets and Packages
sticky <- sticky_trap_counts
chemical <- HBEFdata_All_2024_05_17
view(chemical)

view(sticky)
####converting date data to date form 
sticky$date <- as.Date(sticky$date)
sticky$year <- year(sticky$date)
sticky$month <- month(sticky$date, label = TRUE)

####converting nas into 0

sticky[is.na(sticky)] <- 0

####Shaping data for caddisflies
count_caddisfly <-  sticky %>%
  filter(sample_id != "ST200808") |>
  group_by(year, month) %>%
  summarise(total_caddis = sum(caddisfly_small+caddisfly_large, na.rm = TRUE)) %>%
  ungroup()

####Making Scatter for caddisfly
ggplot(count_caddisfly, aes(x = as.integer(year), y = total_caddis)) +
  geom_point() +  # Use geom_point() if you prefer points
  facet_wrap(~ month, scales = "fixed") +
  labs(title = "Total Number of Caddisflies by Year and Month",
       x = "Year",
       y = "Total Number of Caddisflies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Shaping data for stoneflies 
count_stone <- sticky %>%
  filter(sample_id != "ST200808") |>
  group_by(year, month) %>%
  summarise(total_stoneflies = sum(stonefly_large, na.rm = TRUE)) %>%
  ungroup()

####making graph for stoneflies
ggplot(count_stone, aes(x = as.integer(year), y = total_stoneflies)) +
  geom_point() +  # Use geom_point() if you prefer points
  facet_wrap(~ month, scales = "fixed") +
  labs(title = "Total Number of Stoneflies by Year and Month",
       x = "Year",
       y = "Total Number of Stoneflies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Shaping data for mayflies 
count_may <- sticky %>%
  filter(sample_id != "ST200808") |>
  group_by(year, month) %>%
  summarise(total_mayflies = sum(mayfly_large, na.rm = TRUE)) %>%
  ungroup()

####making graph for mayflies
ggplot(count_may, aes(x = as.integer(year), y = total_mayflies)) +
  geom_point() +  # Use geom_point() if you prefer points
  facet_wrap(~ month, scales = "fixed") +
  labs(title = "Total Number of Mayflies by Year and Month",
       x = "Year",
       y = "Total Number of Mayflies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


