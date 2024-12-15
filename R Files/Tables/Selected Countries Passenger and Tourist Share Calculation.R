# Passenger and Tourist Share Calculation 

# Define the Selected Countries
top_countries <- c("France", "Germany", "Spain", "Italy")

# Calculate total passengers for each top country, total EU passengers, and percentage of EU traffic
country_summary <- combined_dataset %>%
  filter(as.numeric(substr(month, 1, 4)) >= 2012 & as.numeric(substr(month, 1, 4)) <= 2023) %>%
  group_by(country) %>%
  summarise(total_passengers = sum(combined_passenger_numbers, na.rm = TRUE)) %>%
  mutate(top_4_flag = ifelse(country %in% top_countries, "Top 4", "Other EU"))

# Calculate total for the top 4 and entire EU
top_4_total <- country_summary %>% 
  filter(top_4_flag == "Top 4") %>% 
  summarise(total_top_4 = sum(total_passengers)) %>% 
  pull(total_top_4)

eu_total <- sum(country_summary$total_passengers, na.rm = TRUE)

# Calculate percentage of Top 4 countries
percentage_top_4 <- (top_4_total / eu_total) * 100

# Format the table values for LaTeX
country_summary_latex <- country_summary %>%
  filter(country %in% top_countries) %>%
  bind_rows(data.frame(country = "EU Total", total_passengers = eu_total, top_4_flag = "", percentage = NA)) %>%
  mutate(percentage = ifelse(country != "EU Total", round((total_passengers / eu_total) * 100, 2), round(100)))


# Format for LaTeX table
country_summary_latex %>%
  kable(
    format = "latex",
    caption = "Total Passengers and EU Percentage for Top 4 Countries (2012-2023)",
    col.names = c("Country", "Total Passengers", "Top 4 Flag", "Percentage of EU (%)"),
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")





# Calculate Tourism Share

# Define selected countries, including EU-27
selected_countries <- c("Spain", "Germany", "France", "Italy", "European Union - 27 countries (from 2020)")

# Step 1: Calculate the total tourists for EU-27
eu_totals <- tourism_data %>%
  filter(country == "European Union - 27 countries (from 2020)") %>%
  summarize(eu_total_tourists = sum(tourists, na.rm = TRUE))

# Step 2: Calculate the total tourists for selected countries
country_totals <- tourism_data %>%
  filter(country %in% selected_countries) %>%
  group_by(country) %>%
  summarize(total_tourists = sum(tourists, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_share = (total_tourists / eu_totals$eu_total_tourists) * 100
  )

# Step 3: Create a LaTeX table
country_totals %>%
  mutate(
    total_tourists_millions = total_tourists / 1e6,
    total_share = sprintf("%.2f", total_share)
  ) %>%
  select(country, total_tourists_millions, total_share) %>%
  rename(
    `Country` = country,
    `Total Tourists (Millions)` = total_tourists_millions,
    `Total Share of EU (\\%)` = total_share
  ) %>%
  kable(
    format = "latex",
    caption = "Total Share of Tourists in Selected Countries Relative to EU-27 (2012-2023)",
    booktabs = TRUE
  ) %>%
  kable_styling(latex_options = c("hold_position"), position = "center")
