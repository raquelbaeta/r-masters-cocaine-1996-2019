# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Data Source: United Nations (UN)
# Variables: Annual cocaine seizures

# Install Required Packages
install.packages("readxl") 
install.packages("tidyr")
install.packages("dplyr")
install.packages("tidyverse") 
install.packages("data.table") 

# Load Libraries
library(readxl) # work with Excel files
library(tidyr) # data manipulation
library(dplyr) # data manipulation
library(data.table) # data manipulation
library(tidyverse) # data manipulation

# Set Working Directory
setwd("/Users/raquelbaeta/Desktop/working_sessions/datasets")

# Step 1: Read the Excel file "unodc_annual_seizure_1990_2019.xlsx" into a data frame named seizures.
seizures <- read_excel("unodc_annual_seizure_1990_2019.xlsx")

# Step 2: Remove columns corresponding to years not included in the study and rename columns for easier manipulation.
seizures[6:11] <- NULL
seizures <- seizures %>% 
  rename("region" = "Region", 
         "subregion" = "Sub Region", 
         "country" = "Country", 
         "group" = "Drug Group",
         "drug" = "Drug")

# Step 3: The fill() function fills missing values in categorical columns.
seizures <- fill(seizures, "region", "subregion", "country", "group", "drug")

# Step 4: Pivot the data from wide to long format, creating columns for "year" and "seizure".
seizures <- pivot_longer(seizures, 
                         cols = "1996":"2019", 
                         names_to = "year", 
                         values_to = "seizure")

# Step 5: Filter the data to include only rows where the "group" is "Cocaine-type" and the countries are from the specified list.
seizures <- filter(seizures, 
                   group == "Cocaine-type", 
                   country %in% 
                     c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg", 
                       "Netherlands", "Norway", "Portugal", "Spain", "Sweden", "Switzerland"))

# Step 6: Convert Year to Numeric
seizures <- mutate(seizures, year = as.numeric(year))
head(seizures, 10) # view the first 10 rows of the cleaned seizures data frame.

# Step 7: Export the cleaned data frame seizures to a CSV file named "seizures.csv".
fwrite(seizures, "seizures.csv")

# End
