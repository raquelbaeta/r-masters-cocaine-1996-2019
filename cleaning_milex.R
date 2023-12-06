# Start 

# Install libraries
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("dplyr")

# Load Libraries
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)

# Set wtd
setwd("~/Desktop/working-sessions/cleaning_data")

# file path
file_path <- "~/Desktop/working-sessions/cleaning_data/milex_sipri.xlsx"

# Read the "Current US$" sheet, skipping the first 5 rows and selecting columns from the 6th row
milex_dollar <- read_excel(file_path, sheet = "Current US$", skip = 5, range = "A6:AE199")
print(milex_dollar) # check

# Pivot
milex_dollar_longer <- milex_dollar %>%
  pivot_longer(cols = "1990":"2019", names_to = "year", values_to = "milex_dollar")
print(milex_dollar_longer) # check

# List of values to exclude
excluded_values <- c("Africa", "North Africa", "Americas", "Central America and the Caribbean", "South America", 
                     "Asia & Oceania", "Oceania", "South Asia", "East Asia", "South East Asia", "Central Asia", "Europe", 
                     "Central Europe", "Eastern Europe", "Western Europe", "European Union", "Middle East")

# Filter out rows with values in the 'Country' column that are in the exclusion list
filtered_milex <- milex_dollar_longer %>%
  filter(!country %in% excluded_values)
print(filtered_milex) # check

# Read the "Share of GDP" sheet, skipping the first 5 rows and selecting columns from the 6th row
milex_gdp <- read_excel(file_path, sheet = "Share of GDP", skip = 5, range = "A6:AE199")
print(milex_gdp) # check

# Pivot
milex_gdp_longer <- milex_gdp %>%
  pivot_longer(cols = "1990":"2019", names_to = "year", values_to = "milex_gdp")
print(milex_gdp_longer) # check

# List of values to exclude
excluded_values <- c("Africa", "North Africa", "Americas", "Central America and the Caribbean", "South America", 
                     "Asia & Oceania", "Oceania", "South Asia", "East Asia", "South East Asia", "Central Asia", "Europe", 
                     "Central Europe", "Eastern Europe", "Western Europe", "European Union", "Middle East")

# Filter out rows with values in the 'country' column that are in the exclusion list
filtered_milex_gdp <- milex_gdp_longer %>%
  filter(!country %in% excluded_values)
print(filtered_milex_gdp) # check

# Merge filtered_milex_gdp and filtered_milex
merged_milex <- left_join(filtered_milex_gdp, filtered_milex, by = c("country", "year"))
print(merged_milex) # check

# Merge
milex_numeric <- merged_milex %>%
  mutate_at(vars("milex_gdp":"milex_dollar"), as.numeric)
sapply(milex_numeric, class)

# Save as a .csv
write.csv(milex_numeric, file = "~/Desktop/working-sessions/cleaning_data/milex_data.csv", row.names = FALSE)

# Save as a .rds
saveRDS(milex_numeric, "~/Desktop/working-sessions/milex_data.csv.rds")

# End
