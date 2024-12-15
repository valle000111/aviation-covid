# Selected countries Plot Growth Pre-Covid

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define the Selected Countries and the Relevant Years
top_countries <- c("Germany", "Spain", "Italy", "France")
pre_pandemic_years <- 2012:2019

# Step 1: Calculate the annual passenger numbers for each country and year
filtered_data_yearly <- combined_dataset %>%
  filter(
    country %in% top_countries, 
    as.numeric(substr(month, 1, 4)) %in% pre_pandemic_years
  ) %>%
  mutate(year = as.integer(substr(month, 1, 4))) %>%
  group_by(country, year) %>%
  summarize(
    total_combined_passengers = sum(combined_passenger_numbers, na.rm = TRUE) / 1e6,  
    .groups = "drop"
  ) %>%
  arrange(country, year)

# Define the color palette for the countries
color_palette <- c(
  "Spain" = "#D32F2F",
  "Germany" = "#000000",
  "Italy" = "#4CAF50",
  "France" = "#0055A4"  
)

# Ensure consistent factor ordering for countries
filtered_data_yearly$country <- factor(
  filtered_data_yearly$country, 
  levels = c("Spain", "Germany", "France", "Italy")
)

# Create the plot
yearly_passenger_plot <- ggplot(filtered_data_yearly, aes(x = year, y = total_combined_passengers, color = country)) +
  geom_line(linewidth = 1) +
  labs(
    x = "", 
    y = "Combined Passenger Numbers (in millions)",
    color = "Country"  
  ) +
  scale_x_continuous(breaks = seq(2012, 2019, 1)) +  
  scale_color_manual(values = color_palette) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),                 
    axis.text.y = element_text(size = 14),                 
    axis.title.y = element_text(size = 14),                
    legend.text = element_text(size = 14),                 
    legend.title = element_text(size = 14),                
    text = element_text(family = "serif")                  
  )

# Display the plot
print(yearly_passenger_plot)
