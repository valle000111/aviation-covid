# Regression Analysis


# Prepare the dataset
combined_dataset <- combined_dataset %>%
  mutate(month_date = as.Date(paste0(month, "-01")))

# Create a dataset with ITC dummies for 2020-2022
regression_dataset <- combined_dataset %>%
  filter(month_date >= as.Date("2020-01-01") & month_date <= as.Date("2022-12-31")) %>%
  mutate(
    ITC_0 = ifelse(round(international_travel_controls) == 0, 1, 0),
    ITC_1 = ifelse(round(international_travel_controls) == 1, 1, 0),
    ITC_2 = ifelse(round(international_travel_controls) == 2, 1, 0),
    ITC_3 = ifelse(round(international_travel_controls) == 3, 1, 0),
    ITC_4 = ifelse(round(international_travel_controls) == 4, 1, 0)
  )

# Selected Countries regressions
run_regressions <- function(country_name) {
  country_data <- regression_dataset %>% filter(country == country_name)
  
  # Define regression formulas, without ITC_4 for Italy and France
  cases_only <- "combined_passengers_relative_to_2019 ~ new_cases_per_100k" # Model 1
  itc_only <- if (country_name %in% c("France", "Italy")) { # Model 2
    "combined_passengers_relative_to_2019 ~ ITC_1 + ITC_2 + ITC_3"
  } else {
    "combined_passengers_relative_to_2019 ~ ITC_1 + ITC_2 + ITC_3 + ITC_4"
  }
  both_combined <- paste(cases_only, "+", sub("combined_passengers_relative_to_2019 ~ ", "", itc_only)) # Model 3
  
  # Run regressions
  cat("\nResults for", country_name, ":\n")
  cat("\nModel 1: New Cases Only\n")
  print(summary(lm(cases_only, data = country_data)))
  cat("\nModel 2: ITC Dummies Only\n")
  print(summary(lm(itc_only, data = country_data)))
  cat("\nModel 3: Both Cases and ITC\n")
  print(summary(lm(both_combined, data = country_data)))
}

# Run for selected countries
top_countries <- c("Germany", "Spain", "France", "Italy")
for (country in top_countries) {
  run_regressions(country)
}

# EU-wide panel regression (fixed effects - within model)
# Model 1
eu_model_cases <- plm(
  combined_passengers_relative_to_2019 ~ new_cases_per_100k,
  data = regression_dataset,
  index = c("country", "month_date"),
  model = "within"
)

# Model 2
eu_model_itc <- plm(
  combined_passengers_relative_to_2019 ~ ITC_1 + ITC_2 + ITC_3 + ITC_4,
  data = regression_dataset,
  index = c("country", "month_date"),
  model = "within"
)

# Model 3
eu_model_both <- plm(
  combined_passengers_relative_to_2019 ~ new_cases_per_100k + ITC_1 + ITC_2 + ITC_3 + ITC_4,
  data = regression_dataset,
  index = c("country", "month_date"),
  model = "within"
)

cat("\nEU-wide Panel Regression:\n")
cat("\nModel 1: New Cases Only\n")
summary(eu_model_cases)
cat("\nModel 2: ITC Dummies Only\n")
summary(eu_model_itc)
cat("\nModel 3: Both Cases and ITC\n")
summary(eu_model_both)
