# Load necessary libraries
library(dplyr)
library(knitr)
library(kableExtra)


# Selected countries YoY Growth Rate Calculation 2013-2019

# Define the top 4 countries and the relevant years
top_countries <- c("Germany", "Spain", "Italy", "France")
pre_pandemic_years <- 2012:2019  

# Step 1: Calculate the annual passenger numbers for each country and year
annual_totals <- combined_dataset %>%
  filter(country %in% top_countries, as.numeric(substr(month, 1, 4)) %in% pre_pandemic_years) %>%
  mutate(year = as.integer(substr(month, 1, 4))) %>%
  group_by(country, year) %>%
  summarize(annual_passenger_numbers = sum(combined_passenger_numbers, na.rm = TRUE), .groups = "drop") %>%
  arrange(country, year)

# Step 2: Create a lagged dataset by shifting the years by one
annual_totals_lagged <- annual_totals %>%
  mutate(previous_year = year + 1) %>%
  select(country, previous_year, previous_passenger_numbers = annual_passenger_numbers)

# Step 3: Join the original data with the lagged data and calculate YoY growth rate
yoy_growth <- annual_totals %>%
  left_join(annual_totals_lagged, by = c("country", "year" = "previous_year")) %>%
  mutate(
    yoy_growth_rate = if_else(is.na(previous_passenger_numbers), NA_real_,
                              (annual_passenger_numbers - previous_passenger_numbers) / previous_passenger_numbers * 100)
  ) %>%
  filter(year >= 2012)

# Display the results
print(yoy_growth)

# Select the data for the final LaTeX table output
summary_table <- yoy_growth %>%
  select(country, year, annual_passenger_numbers, yoy_growth_rate) %>%
  mutate(
    annual_passenger_numbers_millions = annual_passenger_numbers / 1e6,  
    yoy_growth_rate = ifelse(is.na(yoy_growth_rate), "--", paste0(sprintf("%.2f", yoy_growth_rate), "%"))  
  ) %>%
  select(country, year, annual_passenger_numbers_millions, yoy_growth_rate)


summary_table %>%
  kable(
    format = "latex",
    caption = "Annual Passenger Numbers and Year-on-Year Growth Rates for Top 4 Countries (2013-2019)",
    col.names = c("Country", "Year", "Passenger Numbers (Millions)", "YoY Growth Rate"),
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")





# EU Wide Growth Rate and Combined Passenger Numbers


eu_data <- combined_dataset %>%
  mutate(year = as.integer(substr(month, 1, 4))) %>%  
  group_by(year) %>%  
  summarise(
    total_passengers = sum(combined_passenger_numbers, na.rm = TRUE), 
    .groups = "drop"  
  ) %>%
  arrange(year) %>%  
  mutate(
    lagged_passengers = dplyr::lag(total_passengers), 
    yoy_growth_rate = if_else(
      is.na(lagged_passengers), NA_real_,  
      ((total_passengers - lagged_passengers) / lagged_passengers) * 100  
    )
  )


# Create a LaTeX table
eu_data %>%
  mutate(
    total_passengers_millions = total_passengers / 1e6,  
    yoy_growth_rate_formatted = ifelse(is.na(yoy_growth_rate), "--", sprintf("%.2f", yoy_growth_rate))  
  ) %>%
  select(year, total_passengers_millions, yoy_growth_rate_formatted) %>% 
  rename(
    `Year` = year,
    `Passengers (Millions)` = total_passengers_millions,
    `YoY Growth Rate (%)` = yoy_growth_rate_formatted
  ) %>%
  kable(
    format = "latex",
    caption = "Annual Combined Passenger Numbers (Millions) for the EU (2012-2019) with Year-on-Year Growth Rate",
    booktabs = TRUE,
    col.names = c("Year", "Passengers (Millions)", "YoY Growth Rate (%)")
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")






# Define the Selected Countries
top_countries <- c("France", "Germany", "Italy", "Spain")

# Step 1: Filter YoY growth for 2013-2019 for selected countries
average_growth_selected <- yoy_growth %>%
  filter(country %in% top_countries, year >= 2013 & year <= 2019) %>%
  group_by(country) %>%
  summarise(
    avg_growth = mean(yoy_growth_rate, na.rm = TRUE),  # Average growth for 2013-2019
    total_passengers = sum(annual_passenger_numbers, na.rm = TRUE),  # Total passengers for 2013-2019
    .groups = "drop"
  )

# Step 2: Calculate EU-27 totals for 2013-2019
eu_growth <- eu_data %>%
  filter(year >= 2013 & year <= 2019) %>%
  summarise(
    avg_growth = mean(yoy_growth_rate, na.rm = TRUE),  # EU-wide average growth for 2013-2019
    total_passengers = sum(total_passengers, na.rm = TRUE),  # EU-wide total passengers for 2013-2019
    .groups = "drop"
  )

# Step 3: Calculate market share relative to EU-27 total passengers
final_table <- average_growth_selected %>%
  mutate(
    market_share = total_passengers / eu_growth$total_passengers * 100  # Proportion of EU-27 traffic
  ) %>%
  bind_rows(
    data.frame(
      country = "EU-27",
      avg_growth = eu_growth$avg_growth,
      total_passengers = eu_growth$total_passengers,
      market_share = 100.0
    )
  ) %>%
  select(country, avg_growth, market_share) %>%
  mutate(
    avg_growth = sprintf("%.2f", avg_growth),  
    market_share = sprintf("%.1f", market_share)  
  )

# Step 4: Generate LaTeX table
final_table %>%
  kable(
    format = "latex",
    caption = "Average Growth and Market Share in Flight Numbers for EU-27 and Selected Countries (2013-2019)",
    col.names = c("Country", "Growth (%)", "Share (%)"),
    booktabs = TRUE,
    align = "lrr"
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")
