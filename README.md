# Analysis-of-NOAA-s-1989-Storm-Events
This is an analysis of the National Oceanic and Atmospheric Administration  (NOAA) storm events for 1989
---
title: "Analysis of NOAA’s 1989 Storm Events "
author: "Vasco Ayere Avoka"
date: "2024-06-28"
output: 
  html_document:
    theme:
      bootswatch: flatly 
---
```{r setup, include= FALSE}
knitr::opts_chunk$set(echo = TRUE)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  janitor,
  inspectdf,
  DataExplorer,
  here,
  stringr,
  tidyr,
  ggplot2
)
```
```{r}
# Load data
NOAA_data <- read.csv(here("data/week 6.csv"))

# Limit the dataframe to the specified columns
limited_colums <- NOAA_data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

# Data arranged by state name
state_data <- limited_colums %>%
  arrange(STATE)

# State and county names changed to title case
title_case <- state_data %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))

# Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column
filtered_data <- title_case %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

# Pad the state and county FIPS with a “0” at the beginning
filtered_data <- filtered_data %>%
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, side = "left", pad = "0"))

# Unite the two columns to make one FIPS column with the new state-county FIPS code
filtered_data <- filtered_data %>%
  unite("FIPS", STATE_FIPS, CZ_FIPS, sep = "")

# Change all the column names to lower case
final_data <- filtered_data %>%
  rename_all(tolower)

# Task 8: Create a dataframe with state name, area, and region
data("state")
state_info <- data.frame(state = tolower(state.name), region = state.region, area = state.area)

# Task 9: Create a dataframe with the number of events per state
events_per_state <- final_data %>%
  group_by(state) %>%
  summarise(num_events = n()) %>%
  mutate(state = tolower(state))

# Merge in the state information dataframe
merged_data <- merge(x = events_per_state, y = state_info, by.x = "state", by.y = "state", all.x = TRUE)
merged_data <- na.omit(merged_data)

# Task 10: Create the plot
plot <-  ggplot(merged_data, aes(x = area, y = num_events, color = region)) +
  geom_point() +
  labs(x = "Land area (square miles)", y = "# of storm events in 2017", color = "Region") +
  ggtitle("Number of storm events by state, area and region created by Vasco Ayere Avoka") +
  theme_minimal()

# Save the plot to a file
ggsave("storm_events_plot.pdf", plot = plot, width = 10, height = 6, dpi = 300)
