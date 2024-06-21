#### Load in packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(viridis)
library(bslib)
library(lubridate)
library(tibbletime)

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

####plotting peak emergence####
sticky3 <- sticky %>%
  mutate(
    Year = year(date),
    DayOfYear = yday(date)
  )

max_bugs_per_year <- sticky3 %>%
  group_by(Year, watershed) %>%
  summarise(
    MaxBugs = max(dipteran_small, na.rm = TRUE),
    DayOfYear = DayOfYear[which.max(dipteran_small)]
  )

ggplot(max_bugs_per_year, aes(x = Year, y = DayOfYear, size = MaxBugs, color = watershed)) +
  geom_point(alpha = 0.7)  +
   labs(
    title = "Maximum Number of Bugs Each Year by Watershed",
    size = "Max Bugs",
    color = "Watershed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

str(chemical)

chemical %>% 
  mdy(date)

str(chemical)

  filter(site == "W1" | site == "W2" | site == "W3" | site == "W4" | site == "W4" | site == "W5"
         | site == "W6"| site == "W9" | site == "HBK") %>% 
  filter(between(date, as.Date("12/31/17"), as.Date("12/31/23")))

chemicalSheds


chemicalSheds %>% 
  ggplot(
    aes(x = Date, y = temp, color = site)
  ) +
  geom_point()

# Convert to Date format
chemicalSheds$dates_converted <- mdy(chemicalSheds$date)
chemicalSheds$dates_converted
# Format dates to mm/dd/yyyy
chemicalSheds$formatted_dates <- format(chemicalSheds$dates_converted, "%m/%d/%Y")

chemicalSheds <- data.frame(
  original_dates = chemicalSheds$dates,
  converted_dates = chemicalSheds$formatted_dates,
  pH = chemicalSheds$pH,
  temp = chemicalSheds$temp,
  site = chemicalSheds$site
)

unique(chemicalSheds$site)
str(chemicalSheds)

chemicalShedsDate <- chemicalShedsDate %>% 
  year(chemicalShedsDate$original_dates)

  filter(original_dates > "2017-12-31") 

str(chemicalShedsDate) 
unique(chemicalShedsDate$original_dates)

str(chemicalShedsDate)
chemicalSheds$year <- year(chemicalSheds$converted_dates)
print(chemicalShedsDate$year)
colnames(sticky)
sticky %>% ###total bug count
  summarise(totalBug = sum(dipteran_large, na.rm = TRUE) + sum(dipteran_small, na.rm = TRUE)
            + sum(terrestrial_large, na.rm = TRUE)
            + sum(caddisfly_large, na.rm = TRUE)
            + sum(mayfly_large, na.rm = TRUE)
            + sum(stonefly_large, na.rm = TRUE)
            + sum(other_large, na.rm = TRUE)
            + sum(dipteran_small, na.rm = TRUE)
            + sum(terrestrial_small, na.rm = TRUE)
            + sum(caddisfly_small, na.rm = TRUE)
            + sum(other_small, na.rm = TRUE))
###summing by date 
sticky2 <- sticky %>% 
  mutate(dipteran = dipteran_large + dipteran_small,
         terrestrial = terrestrial_large + terrestrial_small,
         caddisfly = caddisfly_large + caddisfly_small,
         other = other_large + other_small)

stickysum <- sticky2 %>% ##count grouped by date and watershed 
  select(sample_id, side_or_trapnum, watershed, date, dipteran, terrestrial, caddisfly, other, mayfly_large, stonefly_large) %>% 
  group_by(date, watershed) %>% 
  summarize(
    dipteranSum = sum(dipteran, na.rm = TRUE),
    terrestrialSum = sum(terrestrial, na.rm = TRUE),
    caddisflySum = sum(caddisfly, na.rm = TRUE),
    otherSum = sum(other, na.rm = TRUE),
    mayflySum = sum(mayfly_large, na.rm = TRUE),
    stoneflySum = sum(stonefly_large, na.rm = TRUE),
    .groups = 'drop'
  )

stickysum %>% 
  mutate(
    Year = year(date),
    DayOfYear = yday(date)
  ) %>% 
  group_by(Year, watershed) %>% 
  summarize(
    maxdip = max(dipteranSum),
    .groups = 'drop'
  )
  
#testing to see if app works
sticky3 <- stickysum %>%
  mutate(
    Year = year(date), #gives columns year 
    DayOfYear = yday(date) #gives column day of year
  )

#user chooses watershed 6
filtered_data <- sticky3 %>%
  filter(
    watershed == "6" #looking in the watershed selected 
  )

##user chooses diptera

max_bugs_per_year <- filtered_data %>%
  group_by(Year, watershed) %>%
  summarise(
    BugCount = max(dipteranSum, na.rm = TRUE),
    DayOfYear = DayOfYear[which.max(dipteranSum)]
  ) %>%
  ungroup()
  
