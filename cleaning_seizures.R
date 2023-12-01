# Start

# Install necessary libraries if not already installed
install.packages("readxl")
install.packages("tidyverse")

# Load Libraries
library(readxl)
library(tidyverse)

# Set wtd
setwd("~/Desktop/working-sessions/cleaning_data")

# Read the data into R Studio
cocaine_data <- read_excel("seizures_unodc.xlsx")

# Specify the columns to be filled
columns_to_fill <- c("region", 
                     "subregion", 
                     "country", 
                     "drug_group")

# Fill missing values using a top-down approach
filled_data <- cocaine_data %>%
  fill(!!columns_to_fill, 
       .direction = "down")

# Show the filled data
print(filled_data)

# Filter rows where drug_group is "Cocaine-type"
cocaine_data_filtered <- filled_data %>%
  filter(drug_group == "Cocaine-type")

# Show the filtered data
print(cocaine_data_filtered)

# Pivoting the table
cocaine_data_long <- cocaine_data_filtered %>%
  pivot_longer(cols = "1990":"2019", 
               names_to = "year", 
               values_to = "seizures")

# Viewing the result
print(cocaine_data_long)
summary(cocaine_data_long) # check seizure observations

# Remove data
rm(cocaine_data, filled_data, cocaine_data_filtered)

# Assuming your data frame is named cocaine_data_long
cocaine_data_long <- cocaine_data_long %>%
  mutate(merged_drug = ifelse(!is.na(drug_type), drug_type, drug))

# Now you can remove the original drug-type and drug columns if needed
cocaine_data_long <- cocaine_data_long %>%
  select(-drug_group, -drug)

# data frame is named cocaine_data
cocaine_data_long <- mutate(cocaine_data_long, 
                            seizures = ifelse(is.na(seizures), 0, seizures))

# Save as a .csv
write.csv(
  cocaine_data_long, 
  file = "~/Desktop/working-sessions/cleaning_data/cocaine_data_long.csv", 
  row.names = FALSE)

# Save as a .rds
saveRDS(cocaine_data_long, "~/Desktop/working-sessions/cleaning_data.rds")

# End