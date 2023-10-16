# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Data Source: World Bank (WB)
# Variables: Annual public order expenditure

# Install Required Packages
install.packages("readxl")
install.packages("tidyr") 
install.packages("dplyr")  
install.packages("data.table") 

# Load Libraries
library(readxl) # work with Excel files
library(tidyr) # data reshaping
library(dplyr) # data manipulation
library(data.table) # data handling

# This is the set working directory for this analysis.
setwd("/Users/raquelbaeta/Desktop/working_sessions/raw_datasets")

# Step 1: Load the data set from the Eurostat.
public_order <- read_excel(path = "~/Desktop/working_sessions/raw_datasets/public_order_spending_eurostat.xlsx", 
                           sheet = 3,
                           range = "A11:AV29")
head(public_order) # view

# Step 2: Remove the first row
public_order <- public_order[-1, ]

# Rename the "TIME" column to "country".
colnames(public_order)[1] <- "country"

# Step 3: Convert the columns that are characters to numeric.
public_order$"1996" <- as.numeric(public_order$"1996")
public_order$"1997" <- as.numeric(public_order$"1997")

# Step 4: Define a function to remove columns based on names
remove_columns <- function(data, column_names) {
  column_indices <- which(names(data) %in% column_names)
  data <- data[, -column_indices, drop = FALSE]
  return(data)
}

# List of column names to be removed
columns_to_remove <- paste0("...", seq(3, 47, by = 2))

# Apply the function to remove columns
public_order <- remove_columns(public_order, columns_to_remove)

# Step 5: Transform the data set using tidyr's pivot_longer function.
public_order <- pivot_longer(public_order, 
                             cols = c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                                      "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019"),
                             names_to = "year", 
                             values_to = "annual_public_order")
head(public_order) # check the pivot

# Export the cleaned and reshaped data frame to a CSV file named "public_order.csv".
fwrite(public_order, "public_order.csv")

# End
