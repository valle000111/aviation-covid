# Bachelorarbeit Code

# Define the packages
packages <- c("dplyr", "ggplot2", "lubridate", "plm", "kableExtra", 
              "extrafont", "xtable", "knitr", "prettyunits", "reshape2", 
              "stargazer", "tidyr", "tidyselect", "scales", "readr", "ggfx")

# Install all packages
install.packages(packages)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))


# International Travel Controls data
ITC <- read.csv("https://ourworldindata.org/grapher/international-travel-covid.csv?v=1&csvType=full&useColumnShortNames=true")

# Rename ITC column
ITC <- ITC %>%
  rename(international_travel_controls = c8ev_international_travel_controls)



# Covid Data
COVID <- read.csv("https://catalog.ourworldindata.org/garden/covid/latest/compact/compact.csv")



# Merge COVID data and ITC data
merged_data <- merge(
  COVID, ITC,
  by.x = c("country", "date"),
  by.y = c("Entity", "Day"),
  all.x = TRUE
)

# Rename 'Czechia' to 'Czech Republic' in merged_data
merged_data <- merged_data %>%
  mutate(country = recode(country, "Czechia" = "Czech Republic"))


# EU countries
country_names <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
                   "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
                   "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
                   "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", 
                   "Spain", "Sweden")



# Filter for EU countries
eu_covid_data <- merged_data %>%
  filter(country %in% country_names)


# Convert date column to Date format
eu_covid_data <- eu_covid_data %>%
  mutate(date = as.Date(date))

# Define the variables of interest
variables_of_interest <- c("date", "country", "new_cases_smoothed_per_million", "international_travel_controls")

# Filter the dataset to include only the variables of interest
filtered_eu_covid_data <- eu_covid_data %>%
  select(all_of(variables_of_interest))


# Calculate the monthly average of international_travel_controls and new cases per 100k for each country
monthly_covid_data <- filtered_eu_covid_data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(country, month) %>%
  summarise(
    new_cases_per_100k = sum(new_cases_smoothed_per_million, na.rm = TRUE) / 10,
    international_travel_controls = mean(international_travel_controls, na.rm = TRUE)       
  ) %>%
  ungroup()



# Import Flight Dataset
# Change to relative path if you are not working with RStudio
flight_arrivals <- read.csv("Data Sources/flight_arrivals.csv")
flight_departures <- read.csv("Data Sources/flight_departures.csv")


# Filter for relevant columns & prepare data
flight_arrivals <- flight_arrivals %>%
  select(country = Geopolitical.entity..reporting., month = TIME_PERIOD, arrivals = OBS_VALUE)

flight_departures <- flight_departures %>%
  select(country = Geopolitical.entity..reporting., month = TIME_PERIOD, departures = OBS_VALUE)


flight_arrivals <- flight_arrivals %>%
  mutate(country = ifelse(country == "Czechia", "Czech Republic", country))

flight_departures <- flight_departures %>%
  mutate(country = ifelse(country == "Czechia", "Czech Republic", country))



# Merge the datasets
combined_dataset <- monthly_covid_data %>%
  full_join(flight_arrivals, by = c("country", "month")) %>%
  full_join(flight_departures, by = c("country", "month"))


# Create Combined_passenger_numbers
combined_dataset <- combined_dataset %>%
  mutate(combined_passenger_numbers = arrivals + departures)




# Calculate relative change in passengers compared to 2019
baseline_2019 <- combined_dataset %>%
  filter(substr(month, 1, 4) == "2019") %>%  
  select(country, month, combined_passenger_numbers) %>%  
  rename(
    baseline_month = month,
    baseline_passengers_2019 = combined_passenger_numbers
  ) %>%
  mutate(baseline_month = substr(baseline_month, 6, 7))  

# Join the baseline data with the original dataset
combined_dataset <- combined_dataset %>%
  mutate(current_month = substr(month, 6, 7)) %>%  
  left_join(baseline_2019, by = c("country", "current_month" = "baseline_month")) %>%
  select(-current_month)

# Calculate relative change compared to the 2019 baseline
combined_dataset <- combined_dataset %>%
  mutate(
    combined_passengers_relative_to_2019 = ifelse(
      !is.na(baseline_passengers_2019) & baseline_passengers_2019 > 0,
      (combined_passenger_numbers / baseline_passengers_2019 - 1) * 100,
      NA
    )
  )




# Tourism Data Import
# Change to relative path if you are not working with RStudio
tourism_data <- read.csv("Data Sources/tourism.csv", header = TRUE, sep = ",")


# Select only the required columns: geo (country), TIME_PERIOD (year), and OBS_VALUE (tourism numbers)
tourism_data <- tourism_data[, c("geo", "TIME_PERIOD", "OBS_VALUE")]


# Rename columns
colnames(tourism_data) <- c("country", "year", "tourists")



# Save the workspace (Backup)
save.image(file = paste0("workspace_", Sys.Date(), ".RData"))
