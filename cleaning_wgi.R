# Start 

# Install necessary libraries if not already installed
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

# Read the data 
wgi <- read_excel("wgi_world_bank.xlsx", sheet = "Data")

# Assuming df is your dataframe
wgi_filtered <- wgi[grepl("Estimate", wgi$series_name), ]
sapply(wgi_filtered, class)

# Assuming wgi_filtered is your dataframe
wgi_numeric <- wgi_filtered %>%
  mutate_at(vars("1996":"2019"), as.numeric)
sapply(wgi_numeric, class)

# Filter the columns
wgi_transformed <- wgi_numeric %>%
  select(series_code, country, code, "1996":"2019")

# Reshape the data frame
wgi_transformed <- wgi_transformed %>%
  pivot_longer(cols = "1996":"2019", names_to = "year", values_to = "value")

# Filter the rows
wgi_transformed <- wgi_transformed %>%
  filter(series_code %in% c("CC.EST", "GE.EST", "RQ.EST", "RL.EST", "VA.EST", "PV.EST"))

# Rename the value column
wgi_transformed <- wgi_transformed %>%
  rename(estimate = value)
wgi_transformed

# Pivot wider
wgi_transformed_wide <- wgi_transformed %>%
  pivot_wider(names_from = series_code, values_from = estimate)
wgi_transformed_wide

# Save as a .csv
write.csv(
  wgi_transformed_wide, 
  file = "~/Desktop/working-sessions/cleaning_data/wgi_data.csv", 
  row.names = FALSE)

# Save as a .rds
saveRDS(wgi_transformed_wide, "~/Desktop/working-sessions/wgi_data.csv.rds")

# End