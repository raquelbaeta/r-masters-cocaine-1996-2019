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
cow_statelist <- read_excel("COW_statelist.xlsx")
treaty <- read_excel("un_treaty.xlsx")

# Join the data sets
statelist <- merge(cow_statelist, treaty, by = c("country", "id"), all.x = TRUE)

# Replace NA with 0 in the specified columns
columns_to_replace <- c("UN1961", "UN1971", "UN1988")
statelist[columns_to_replace][is.na(statelist[columns_to_replace])] <- 0

# Convert non-zero values to 1
statelist[columns_to_replace] <- +(statelist[columns_to_replace] != 0)
print(statelist)

# Remove duplicate rows
statelist <- statelist[!duplicated(statelist), ]
print(statelist)

# Save as a .csv
write.csv(
  statelist, 
  file = "~/Desktop/working-sessions/cleaning_data/statelist.csv", 
  row.names = FALSE)

# Save as a .rds
saveRDS(statelist, "~/Desktop/working-sessions/statelist.rds")

# End