# COVID Plot France


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)

# Filter for France data
france_data <- combined_dataset %>%
  filter(country == "France")

# Filter for 2020-2024
france_data <- france_data %>%
  filter(format(month_date, "%Y") >= 2020)

# Calculate xmax for ITC shading
france_data <- france_data %>%
  arrange(month_date)  
france_data$xmax <- c(france_data$month_date[-1], max(france_data$month_date))  

# Define scaling factor for y-axes
max_cases_per_100k_france <- 15000
max_passengers_relative <- 100
scaling_factor_france <- max_passengers_relative / max_cases_per_100k_france

# Add shading categories for international travel controls
france_data <- france_data %>%
  mutate(
    travel_control_category = cut(
      international_travel_controls,
      breaks = c(-Inf, 1, 2, 3, 4, Inf),  
      labels = c("0", "1", "2", "3", "4"), 
      right = FALSE                        
    )
  )

# Ensure the data is sorted and prepare xmax for shading
france_data <- france_data %>%
  arrange(month_date) %>%
  mutate(
    xmax = c(month_date[-1], month_date[n()]) 
  )

# Plot the data
ggplot(france_data, aes(x = month_date)) +
  with_raster(
    geom_rect(
      data = france_data %>% filter(travel_control_category != "0" & !is.na(travel_control_category)),
      aes(
        xmin = month_date,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = travel_control_category
      ),
      color = NA, 
      alpha = 0.7,
      inherit.aes = FALSE
    )
  ) +
  # Lines for relative passengers and cases
  geom_line(aes(y = combined_passengers_relative_to_2019, color = "Relative Change in Passengers compared to 2019 (%)"), linewidth = 2) +
  geom_line(aes(y = new_cases_per_100k * scaling_factor_france, color = "COVID-19 Cases per 100k"), linewidth = 2) +
  scale_x_date(
    date_labels = "%Y",
    breaks = as.Date(paste0(2020:2024, "-01-01")),
    name = NULL  
  ) +
  # Y-axis customization
  scale_y_continuous(
    limits = c(-100, 100),
    breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),  
    sec.axis = sec_axis(~ . / scaling_factor_france, 
                        breaks = seq(0, 15000, by = 5000),
                        labels = scales::label_number(accuracy = 1))
  ) +
  # Line colors for relative passengers and cases
  scale_color_manual(
    values = c(
      "Relative Change in Passengers compared to 2019 (%)" = "#1F77B4",
      "COVID-19 Cases per 100k" = "#D62728"
    )
  ) +
  # Brown shades for ITC categories
  scale_fill_manual(
    name = NULL,
    values = c(
      "1" = "#E1C1AC",  
      "2" = "#C8A47D",  
      "3" = "#A6754A",  
      "4" = "#6D4228"   
    )
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_blank(),                   
    axis.title.y.right = element_blank(),                  
    axis.text.y = element_text(size = 22),                 
    axis.text.x = element_text(size = 22),                 
    legend.position = "none",                              
    plot.title = element_text(size = 20, hjust = 0.5),  
    text = element_text(family = "serif")
  )

