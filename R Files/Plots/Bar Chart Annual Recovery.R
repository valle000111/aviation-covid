# Load necessary libraries
library(dplyr)
library(ggplot2)

# Step 1: Calculate total passengers for 2023 and 2019 for each country
recovery_data <- combined_dataset %>%
  mutate(year = as.numeric(format(as.Date(month_date), "%Y"))) %>%
  filter(year %in% c(2023, 2019)) %>%
  group_by(country, year) %>%
  summarise(
    total_passengers = sum(combined_passenger_numbers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = total_passengers, names_prefix = "year_") %>%
  mutate(
    recovery = (year_2023 / year_2019) * 100 
  ) %>%
  filter(country %in% c("Spain", "Germany", "France", "Italy")) 

# Step 2: Calculate weighted EU-27 average
eu27_data <- combined_dataset %>%
  mutate(year = as.numeric(format(as.Date(month_date), "%Y"))) %>% 
  filter(year %in% c(2023, 2019)) %>% 
  group_by(year) %>%
  summarise(
    total_passengers = sum(combined_passenger_numbers, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = total_passengers, names_prefix = "year_") %>%
  mutate(
    recovery = (year_2023 / year_2019) * 100, 
    country = "EU-27" 
  )

# Step 3: Combine country data with EU-27 data
recovery_combined <- bind_rows(recovery_data, eu27_data)

# Step 4: Set the order of countries for consistent plotting
recovery_combined$country <- factor(recovery_combined$country, levels = c("Spain", "Germany", "France", "Italy", "EU-27"))

# Step 5: Create the bar chart
ggplot(recovery_combined, aes(x = country, y = recovery)) +
  geom_bar(stat = "identity", fill = "#1F77B4", width = 0.7) + 
  labs(
    x = "Country",
    y = "Percentage Recovery (%)"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 120)) +
  theme_minimal() +
  theme(
    text = element_text(size = 0, family = "serif"), 
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 0),
    panel.grid.major = element_line(color = "#eaeaea"), 
    panel.grid.minor = element_blank()
  )
