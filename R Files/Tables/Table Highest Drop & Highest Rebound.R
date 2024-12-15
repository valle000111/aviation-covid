# Load required libraries
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)

# Ensure the `month` column in combined_dataset is in Date format
combined_dataset <- combined_dataset %>%
  mutate(month_date = as.Date(paste0(month, "-01")))

# Filter for the countries of interest and date range
filtered_data_monthly <- combined_dataset %>%
  filter(
    country %in% c("France", "Germany", "Spain", "Italy"),
    month_date >= as.Date("2020-01-01") & month_date <= as.Date("2024-12-31")
  )

# Find the highest drop and rebound for each country by month
highest_drop_rebound_monthly <- filtered_data_monthly %>%
  group_by(country) %>%
  summarise(
    # Highest drop (including 2020)
    month_with_highest_drop = month_date[which.min(combined_passengers_relative_to_2019)],
    highest_drop = min(combined_passengers_relative_to_2019, na.rm = TRUE),
    
    # Highest rebound (excluding 2020)
    month_with_highest_rebound = month_date[year(month_date) != 2020][which.max(combined_passengers_relative_to_2019[year(month_date) != 2020])],
    highest_rebound = max(combined_passengers_relative_to_2019[year(month_date) != 2020], na.rm = TRUE)
  ) %>%
  ungroup()

# Convert to a LaTeX-compatible table
highest_drop_rebound_monthly %>%
  mutate(
    month_with_highest_drop = format(as.Date(month_with_highest_drop), "%Y-%m"),
    month_with_highest_rebound = format(as.Date(month_with_highest_rebound), "%Y-%m")
  ) %>%
  kable(
    format = "latex",
    caption = "Highest Drop and Rebound in Monthly Passenger Change Relative to 2019 for France, Germany, Spain, and Italy (2020-2024)",
    col.names = c("Country", "Month with Highest Drop", "Change compared to 2019 (%)",
                  "Month with Highest Rebound", "Change compared to 2019 (%)"),
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")




# Calculation Passenger Numbers First Month Above 100%


# Ensure the `month` column in combined_dataset is in Date format
combined_dataset <- combined_dataset %>%
  mutate(month_date = as.Date(paste0(month, "-01")))

# Filter for the top 4 countries and find the first month (2021 onwards) where passenger levels were >= 100%
top_countries_first_above_100 <- combined_dataset %>%
  filter(
    country %in% c("France", "Germany", "Spain", "Italy"),
    year(month_date) >= 2021,  # Only consider data from 2021 onwards
    combined_passengers_relative_to_2019 >= 1
  ) %>%
  group_by(country) %>%
  slice_min(month_date) %>%  # Select the first occurrence for each country
  ungroup() %>%
  mutate(
    month_formatted = format(month_date, "%Y-%m")  # Format month_date as "YYYY-MM"
  ) %>%
  select(country, month_formatted, combined_passengers_relative_to_2019) %>%
  arrange(country)

# Convert to a LaTeX-compatible table
top_countries_first_above_100 %>%
  kable(
    format = "latex",
    caption = "First Month (2021 Onwards) When Passenger Levels Were At or Above 100% of 2019 Levels for France, Germany, Spain, and Italy (2021-2024)",
    col.names = c("Country", "Month", "Relative to 2019 (%)"),
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")





# EU Weighted Average Highest Drop and Rebound


# Calculate EU total passengers and relative change for 2020-2024
eu_data <- combined_dataset %>%
  mutate(month_date = as.Date(paste0(month, "-01"))) %>%  # Convert `YYYY-MM` to `YYYY-MM-DD`
  filter(month_date >= as.Date("2020-01-01") & month_date <= as.Date("2024-06-30")) %>%
  group_by(month_date) %>%
  summarise(
    eu_total_passengers = sum(combined_passenger_numbers, na.rm = TRUE),
    weighted_passenger_change = sum(combined_passenger_numbers * combined_passengers_relative_to_2019, na.rm = TRUE) / sum(combined_passenger_numbers, na.rm = TRUE)
  ) %>%
  ungroup()

# Find highest drop and rebound
eu_drop_rebound <- eu_data %>%
  summarise(
    country = "EU Average",
    month_with_highest_drop = month_date[which.min(weighted_passenger_change)],
    highest_drop = min(weighted_passenger_change, na.rm = TRUE),
    month_with_highest_rebound = month_date[which.max(weighted_passenger_change)],
    highest_rebound = max(weighted_passenger_change, na.rm = TRUE)
  ) %>%
  mutate(
    month_with_highest_drop = format(as.Date(month_with_highest_drop), "%Y-%m"),
    month_with_highest_rebound = format(as.Date(month_with_highest_rebound), "%Y-%m")
  )

# Display the result in LaTeX table format
eu_drop_rebound %>%
  kable(
    format = "latex",
    caption = "Highest Drop and Rebound in Monthly Passenger Change Relative to 2019 for EU Average (2020-2024)",
    col.names = c("Country", "Month with Highest Drop", "Change Compared to 2019 (%)",
                  "Month with Highest Rebound", "Change Compared to 2019 (%)"),
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")





# EU Weighted Average First Month Above Pre-Pandemic Levels


# Create eu_data
eu_data <- combined_dataset %>%
  mutate(month_date = as.Date(paste0(month, "-01"))) %>%  # Convert `YYYY-MM` to `YYYY-MM-DD`
  filter(month_date >= as.Date("2020-01-01") & month_date <= as.Date("2024-06-30")) %>%
  group_by(month_date) %>%
  summarise(
    eu_total_passengers = sum(combined_passenger_numbers, na.rm = TRUE),
    weighted_passenger_change = sum(combined_passenger_numbers * combined_passengers_relative_to_2019, na.rm = TRUE) / sum(combined_passenger_numbers, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure the `month_date` column is correctly formatted
eu_above_100 <- eu_data %>%
  mutate(month = as.Date(month_date, format = "%Y-%m-%d"))

# Filter data for the first occurrence when EU average passenger levels were at or above 100% of 2019 levels from 2021 onwards:
eu_above_100_first <- eu_above_100 %>%
  filter(year(month) >= 2021, weighted_passenger_change >= 1) %>% 
  slice_min(month) %>% 
  ungroup() %>%
  mutate(month_formatted = format(month, "%Y-%m")) %>% 
  select(month_formatted, weighted_passenger_change)  

# Convert to a LaTeX-compatible table:
eu_above_100_first %>%
  kable(
    format = "latex",
    caption = "First Month (2021 Onwards) When EU Average Passenger Levels Were At or Above 100% of 2019 Levels (Weighted Average)",
    col.names = c("Month", "Weighted EU Average Relative to 2019 (%)"),
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")
