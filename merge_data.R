# Start 

# Install libraries
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("countrycode")

# Load Libraries
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(countrycode)

# Set wtd
setwd("~/Desktop/working-sessions/cleaning_data")

# Specify the path to your CSV file
file_path <- "~/Desktop/working-sessions/cleaning_data/csv"

# Read the CSV file into a data frame
cocaine_data <- read_csv("csv/cocaine_data_long.csv")
print(cocaine_data)

# Convert the 'country' column to a character vector
cocaine_data$country <- as.character(cocaine_data$country)

# Use countrycode
cocaine_data$code <- countrycode(
  sourcevar = cocaine_data$country,
  origin = "country.name", destination = "iso3c"
)

# Display the unique country names
cocaine_data_countrynames <- unique(cocaine_data$country)
print(cocaine_data_countrynames)

# Rename United States
cocaine_data$country[cocaine_data$country == "United States of America"] <- "United States"
cocaine_data_countrynames <- unique(cocaine_data$country)
print(cocaine_data_countrynames)

# Reorder columns
cocaine_data <- cocaine_data[, c("country", "code", "year", "seizures", "seizures_binary")]
colnames(cocaine_data)

# Read the CSV file into a data frame
government_data <- read_csv("csv/government_data.csv") 
print(government_data)

# Display the unique country names
government_data_countrynames <- unique(government_data$country)
print(government_data_countrynames)

# Merge cocaine_data and government_data
coca_government <- inner_join(cocaine_data, government_data, by = c("code", "year"))
print(coca_government)

# Remove unwanted columns
coca_government <- subset(coca_government, select = -country.x)

# Rename country.x
colnames(coca_government)[colnames(coca_government) == "country.y"] <- "country"
str(coca_government)
head(coca_government)
tail(coca_government)

# Display the unique country names from the merged data
unique_countries <- unique(coca_government$country)
print(unique_countries)
summary(coca_government$seizures)

# Look for duplicates
any_duplicates <- any(duplicated(coca_government))

# Print the result
if (any_duplicates) {
  print("There are duplicates in the data frame.")
} else {
  print("No duplicates found in the data frame.")
}

# Find duplicate rows
duplicate_rows <- coca_government[duplicated(coca_government), ]

# Print the duplicate rows
print(duplicate_rows)

# Remove exact duplicates
coca_government <- distinct(coca_government)

# Check the structure after removing duplicates
str(coca_government)

# Display the unique country names from the merged data
unique_countries <- unique(coca_government$country)
print(unique_countries)

# Reorder columns
coca_government <- coca_government[, c("region", "region_iso3c", "country", "code", "year", "seizures", "seizures_binary",
                                       "log_adjusted_gdp", "adjusted_gdp", "gdp_cap_dollar", "deflator_us", "gne_dollar",
                                       "gne_gdp", "adjusted_nicap_dollar", "income_level", "income_level_iso3c", "lending_type",
                                       "lending_type_iso3c", "exports_dollar", "exports_gdp", "imports_dollar", "imports_gdp")]
colnames(coca_government)
head(coca_government)
tail(coca_government)

# Read the CSV file into a data frame
statelist <- read_csv("csv/statelist.csv")
print(statelist)

# Display the unique country names from the merged data
unique_countries <- unique(statelist$country)
print(unique_countries)

# Rename US
statelist$country[statelist$country == "United States of America"] <- "United States"

# Display the unique country names from the merged data
unique_countries <- unique(statelist$country)
print(unique_countries)

# Merge based on the "code" column
coca_gov_state <- merge(coca_government, statelist, by = "code", all.x = TRUE)

# Check the structure of the merged data
str(coca_gov_state)
head(coca_gov_state)
tail(coca_gov_state)

# Remove unwanted columns
coca_gov_state <- subset(coca_gov_state, select = -c(country.y, id))

# Rename country.x
colnames(coca_gov_state)[colnames(coca_gov_state) == "country.x"] <- "country"
str(coca_gov_state)
summary(coca_gov_state$seizures)

# Convert NAs to 0 in specified columns
cols_to_replace_na <- c("UN1961", "UN1971", "UN1988")
coca_gov_state[cols_to_replace_na][is.na(coca_gov_state[cols_to_replace_na])] <- 0

# Check
summary(coca_gov_state$UN1961)
summary(coca_gov_state$UN1971)
summary(coca_gov_state$UN1988)

# Reorder columns
coca_gov_state <- coca_gov_state[, c("region", "region_iso3c", "country", "code", "year", "seizures", "seizures_binary",
                                     "UN1961", "UN1971", "UN1988", "log_adjusted_gdp", "adjusted_gdp", "gdp_cap_dollar", 
                                     "deflator_us", "gne_dollar", "gne_gdp", "adjusted_nicap_dollar", "income_level", 
                                     "income_level_iso3c", "lending_type", "lending_type_iso3c", "exports_dollar", "exports_gdp", 
                                     "imports_dollar", "imports_gdp")]
str(coca_gov_state)

# Read the CSV file into a data frame
wgi_data <- read_csv("csv/wgi_data.csv")
print(wgi_data)

# Display the unique country names from the merged data
unique_countries <- unique(wgi_data$country)
print(unique_countries)

# Merge based on "code", and "year"
coca_gov_wgi <- merge(wgi_data, coca_gov_state, by = c("code", "year"))

# Check the structure of the merged data
str(coca_gov_wgi)
head(coca_gov_wgi)
tail(coca_gov_wgi)

# Remove unwanted columns
coca_gov_wgi <- subset(coca_gov_wgi, select = -country.y)

# Rename country.x
colnames(coca_gov_wgi)[colnames(coca_gov_wgi) == "country.x"] <- "country"
str(coca_gov_wgi)

# Reorder columns
coca_gov_wgi <- coca_gov_wgi[, c("region", "region_iso3c", "country", "code", "year", "UN1961", "UN1971", "UN1988", "seizures", 
                                 "seizures_binary", "log_adjusted_gdp", "adjusted_gdp", "gdp_cap_dollar", "deflator_us", 
                                 "gne_dollar", "gne_gdp", "adjusted_nicap_dollar", "income_level", "income_level_iso3c", 
                                 "lending_type", "lending_type_iso3c", "exports_dollar", "exports_gdp", "imports_dollar", 
                                 "imports_gdp", "CC.EST", "GE.EST", "RQ.EST", "RL.EST", "VA.EST", "PV.EST")]
head(coca_gov_wgi)

# Read the CSV file into a data frame
milex_data <- read_csv("csv/milex_data.csv")
print(milex_data)

# Use countrycode
milex_data$code <- countrycode(sourcevar = milex_data$country, origin = "country.name", destination = "iso3c")
print(milex_data)

# Merge
data <- merge(coca_gov_wgi, milex_data, by = c("code", "year"))
head(data) # check
tail(data) # check 

# Remove unwanted columns
data <- subset(data, select = -country.y)

# Rename country.x
colnames(data)[colnames(data) == "country.x"] <- "country"
str(data)

# Reorder columns
data <- data[, c("region", "region_iso3c", "country", "code", "year", "UN1961", "UN1971", "UN1988", "seizures", "seizures_binary",
                 "log_adjusted_gdp","adjusted_gdp", "gdp_cap_dollar", "deflator_us", "gne_dollar", "gne_gdp", "milex_dollar", 
                 "milex_gdp", "adjusted_nicap_dollar", "income_level", "income_level_iso3c", "lending_type", "lending_type_iso3c",  
                 "exports_dollar", "exports_gdp", "imports_dollar", "imports_gdp", "milex_gdp", "milex_dollar", "CC.EST", 
                 "GE.EST", "RQ.EST", "RL.EST", "VA.EST", "PV.EST")]
head(data) # check
tail(data) # check 

# Rename country.x
colnames(data)[colnames(data) == "milex.gdp.1"] <- "milex.gdp"
colnames(data)[colnames(data) == "milex.dollar.1"] <- "milex.dollar"
str(data)

# Check for missing values in the dataset using 
sum(is.na(data))

# Filter rows where the year is 1996 or later
data <- data[data$year >= 1996, , drop = FALSE]

# Check for missing values in the dataset using 
sum(is.na(data))

# Check years involved in this study
unique(data$year)
summary(data)

# Assuming 'cleaned_data' is your original dataset
cleaned_data <- data[complete.cases(data), ]
summary(cleaned_data)

# Clean environment
rm(cocaine_data, government_data, coca_government, statelist, coca_gov_state, wgi_data, coca_gov_wgi, milex_data, duplicate_rows, 
   wb_countries, us_deflator_data, merged_data, merged_wb_data, unique_countries_per_region)

# interval 
cleaned_data$year_interval <- cut(cleaned_data$year, breaks = c(1995, 1999, 2009, 2019),
                                  labels = c("1996-1999", "2000-2009", "2010-2019"), include.lowest = TRUE, right = TRUE)

# Create "any" variable
cleaned_data <- cleaned_data %>%
  mutate(any_UN = ifelse(UN1961 == 1 | UN1971 == 1 | UN1988 == 1, 1, 0))

# Add the new column trade_ratio
cleaned_data$trade_ratio <- (cleaned_data$exports_gdp + cleaned_data$imports_gdp) / cleaned_data$adjusted_gdp

# Reorder columns
cleaned_data <- cleaned_data[, c("region", "region_iso3c", "country", "code", "year", "year_interval", "any_UN", "UN1961", 
                                 "UN1971", "UN1988", "seizures", "seizures_binary", "log_adjusted_gdp", "adjusted_gdp", 
                                 "gdp_cap_dollar", "deflator_us", "gne_dollar", "gne_gdp", "adjusted_nicap_dollar", 
                                 "income_level", "income_level_iso3c", "lending_type", "lending_type_iso3c", "exports_dollar", 
                                 "exports_gdp", "imports_dollar", "imports_gdp", "trade_ratio", "milex_gdp", "milex_dollar",
                                 "CC.EST", "GE.EST", "RQ.EST", "RL.EST", "VA.EST", "PV.EST")]
colnames(cleaned_data) # check order 

# Save as a .csv
write.csv(cleaned_data, file = "~/Desktop/working-sessions/cleaning_data/cleaned_data.csv", row.names = FALSE)

# Save as a .rds
saveRDS(cleaned_data, "~/Desktop/working-sessions/cleaned_data.csv.rds")

# Count the number of unique countries with complete rows
num_unique_countries <- length(unique(cleaned_data$country[complete.cases(cleaned_data)]))
num_unique_countries # 145 unique countries

# List the unique countries 
unique_countries <- unique(cleaned_data$country[complete.cases(cleaned_data)])

# Display the result
print(unique_countries)

# Count complete cases by income level
complete_cases_by_income <- table(cleaned_data$income_level[complete.cases(cleaned_data)])

# Display the result
print(complete_cases_by_income)

# Count complete cases by income level
complete_cases_by_lending <- table(
  cleaned_data$lending_type[complete.cases(cleaned_data)])

# Display the result
print(complete_cases_by_lending)

# Count countries per region
unique_countries_per_region <- lapply(split(cleaned_data$country, cleaned_data$region), 
  function(x) list(unique_countries = unique(x), count = length(unique(x))))

# Display the result
print(unique_countries_per_region)

# End 
