#### Load in packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(viridis)
library(bslib)
library(lubridate)

####Load in Data Sets and Packages
sticky <- read.csv("shiny/shiny_reu_24/data_raw/sticky_trap_counts.csv")
sticky <- sticky[-1393,] # removes record # 1393
chemical <- read.csv("shiny/shiny_reu_24/data_raw/HBEFdata_All_2024-05-17.csv")
view(chemical)

view(sticky)

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

#### making graph for mayflies
ggplot(count_may, aes(x = as.integer(year), y = total_mayflies)) +
  geom_point() +  # Use geom_point() if you prefer points
  facet_wrap(~ month, scales = "fixed") +
  labs(title = "Total Number of Mayflies by Year and Month",
       x = "Year",
       y = "Total Number of Mayflies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### make scatterplot: x = time, y = bug count, color = watershed ####
sticky %>% 
  mutate(Date = as.Date(date, "%Y-%m-%d")) %>% 
  ggplot(aes(x = Date, y = dipteran_large, color = as.factor(watershed))) +
  geom_point() + #turn date into date 
  labs(title = "Scatter plot of Bugs over Time",
       x = "Date",
       y = "Number of Bugs",
       color = "Watershed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
scale_x_date(date_breaks = "months" , date_labels = "%b-%y") 

#check to see if theres a way to change the number of labels. (ex 20 breaks on x) ####

####shiny app code
filtered_data1 <- sticky %>% 
  mutate(Date = as.Date(date, "%Y-%m-%d")) %>% 
  pivot_longer(cols = dipteran_large:other_small, 
               names_to = "taxa", # names the new names column
               values_to = "count") %>% 
  group_by(Date) %>% 
  summarise(total_bug = sum(count, na.rm = TRUE)) %>%
  ungroup()
filtered_data1

####generating emergence plot####

tail(sticky)
peak_other <- sticky %>% 
  mutate(Date = as.Date(date, "%Y-%m-%d")) %>% 
  group_by(date) %>% 
  summarize(max = max(other_small, na.rm = TRUE)) %>% 
  ungroup()
peak_other
