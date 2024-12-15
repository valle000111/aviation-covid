# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the blue color
blueColor <- "#1F77B4"

# Step 1: Filter data for selected countries and years
selected_countries <- c("Spain", "Germany", "France", "Italy") 
passenger_data <- combined_dataset %>%
  mutate(year = as.numeric(format(as.Date(month_date), "%Y"))) %>% 
  filter(year %in% c(2020, 2021, 2022), country %in% selected_countries) %>%
  group_by(country, year) %>%
  summarise(
    total_passengers = sum(combined_passenger_numbers, na.rm = TRUE),
    baseline_passengers = sum(baseline_passengers_2019, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    drop_percent = ((total_passengers - baseline_passengers) / baseline_passengers) * 100
  )

# Step 2: Prepare data for bar chart
passenger_data_long <- passenger_data %>%
  select(country, year, drop_percent) %>%
  mutate(Metric = "Drop in Passengers") %>%
  rename(Drop = drop_percent)

# Step 3: Ensure the correct order of the countries
passenger_data_long$country <- factor(passenger_data_long$country, levels = c("Spain", "Germany", "France", "Italy"))

# Step 4: Create the bar chart
ggplot(passenger_data_long, aes(x = factor(year), y = Drop)) +
  geom_bar(stat = "identity", fill = blueColor, show.legend = FALSE) + #
  facet_wrap(~country, nrow = 2, scales = "free_x") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-100, 0)) +
  labs(x = "Year", y = "Percentage Drop (%)") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14), 
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "#eaeaea"), 
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.5, "lines")
  )
