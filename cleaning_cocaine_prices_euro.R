# Start

# title: 'Cocaine Prices in Europe in Euros (2020) from 1996 to 2019'
# author: 'Raquel Baeta'
# date: '2023-06-30'

# Data Source: United Nations Office on Drugs and Crime (UNODC).
# Variables: 
# 1. Annual wholesale averages
# 2. Annual retail averages
# 3. Weighted averages
# 4. Euro-inflation-adjusted weighted average for wholesale 
# 5. Euro-inflation-adjusted weighted average for retail

###
# Requirements
###

# install Required Packages
install.packages(c("readxl", "tidyr", "dplyr", "tidyverse", "data.table"))

# load libraries
library(readxl) # work with Excel files
library(tidyr) # data manipulation
library(dplyr) # data manipulation
library(data.table) # data manipulation
library(tidyverse) # data manipulation

###
# Retail prices of cocaine
###

# load the retail price data from the Excel file and specify sheet and range.
retail_prices <- read_excel("price_dollar_western_europe.xlsx", 
                            sheet = 2, 
                            range = "A5:AF26")

# rename the first column to "country"
colnames(retail_prices)[1] <- "country"

# use the %>% pipe operator from the "dplyr" to manipulate data
retail_prices <- retail_prices %>%
  
  # remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # filter out unwanted rows
  filter(!country %in%
           c("United Kingdom", 
             "Weighted* average, US$", 
             "Unweighted average, in US$",
             "Weighted* average, Euro",
             "Euro-inflation-adjusted weighted* average, in 2020 Euro"))

# reshape the data from wide to long format using the pivot_longer() to transform 
# years 1996 to 2019 into a single "year" column 
retail_prices <- pivot_longer(retail_prices,
                              cols = c("1996":"2019"),
                              names_to = "year",
                              values_to = "retail")

###
# Wholesale prices of cocaine
###

# load the wholesale price data from the Excel file.
wholesale_prices <- read_excel("price_dollar_western_europe.xlsx", 
                               sheet = 2, 
                               range = "A34:AF55")

# rename the first column to "country"
colnames(wholesale_prices)[1] <- "country" 

# use the %>% pipe operator from the "dplyr" to manipulate data
wholesale_prices <- wholesale_prices %>%
  
  # remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # filter out unwanted rows
  filter(!country %in%
           c("United Kingdom", 
             "Weighted* average, US$", 
             "Unweighted average, in US$", "Weighted* average, Euro per gram",
             "Euro-inflation-adjusted weighted* average, in 2020 Euro"))

# reshape the data from wide to long format using the pivot_longer() to transform 
# years 1996 to 2019 into a single "year" column 
wholesale_prices <- pivot_longer(wholesale_prices,
                                 cols = c("1996":"2019"),
                                 names_to = "year",
                                 values_to = "wholesale")

# merge retail and wholesale using the full_join()
prices <- full_join(retail_prices, 
                    wholesale_prices, 
                    by = c("country", "year"))

# clean environment
rm(retail_prices, wholesale_prices)

###
# Retail weighted average, euro per gram
###

# load and filter for the 'Weighted* average, Euro' category
retail_wght_average <- read_excel("price_dollar_western_europe.xlsx",
                                  sheet = 2,
                                  range = "A5:AF26")

# rename the first column to "country"
colnames(retail_wght_average)[1] <- "country"

# use the %>% pipe operator from the "dplyr" to manipulate data
retail_wght_average <- retail_wght_average %>%
  
  # remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # filter out unwanted rows
  filter(country == "Weighted* average, Euro")

# reshape the data from wide to long format using the pivot_longer() to transform 
# years 1996 to 2019 into a single "year" column 
retail_wght_average <- pivot_longer(retail_wght_average,
                                    cols = c("1996":"2019"),
                                    names_to = "year",
                                    values_to = "retail_wgted_average")

# remove the "country" column
retail_wght_average[1] <- NULL

###
# Wholesale weighted average, Euro per gram
### 

# load and filter for the 'weighted average, euro per gram'
wholesale_wght_average <- read_excel("price_dollar_western_europe.xlsx",
                                     sheet = 2,
                                     range = "A34:AF55")

# rename the first column to "country"
colnames(wholesale_wght_average)[1] <- "country"

# use the %>% pipe operator from the "dplyr" to manipulate data
wholesale_wght_average <- wholesale_wght_average %>%
  
  # remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # filter out unwanted rows
  filter(country == "Weighted* average, Euro per gram")

# reshape the data from wide to long format using the pivot_longer() to transform 
# years 1996 to 2019 into a single "year" column 
wholesale_wght_average <- pivot_longer(wholesale_wght_average,
                                       cols = c("1996":"2019"),
                                       names_to = "year",
                                       values_to = "wholesale_wgted_average")

# remove the "country" column
wholesale_wght_average[1] <- NULL

# merge() average price and wholesale data based on the "year" 
# "all = TRUE" to ensure that all years are included
wght_average <- merge(retail_wght_average, 
                      wholesale_wght_average, 
                      by = "year",
                      all = TRUE)

# clean environment
rm(retail_wght_average, wholesale_wght_average) 

###
# Retail euro-inflation-adjusted weighted average
###

# load and filter for the euro-inflation-adjusted weighted
retail_inflation_average <- read_excel("price_dollar_western_europe.xlsx",
                                       sheet = 2,
                                       range = "A5:AF26")

# Rename the first column to "country" 
colnames(retail_inflation_average)[1] <- "country"

# use the %>% pipe operator from the "dplyr" to manipulate data
retail_inflation_average <- retail_inflation_average %>%
  
  # remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # filter out unwanted rows
  filter(country == "Euro-inflation-adjusted weighted* average, in 2020 Euro")

# reshape the data from wide to long format using the pivot_longer() to transform 
# years 1996 to 2019 into a single "year" column 
retail_inflation_average <- pivot_longer(retail_inflation_average,
                                         cols = c("1996":"2019"),
                                         names_to = "year",
                                         values_to = "retail_inflation_average")

# remove the "country" column
retail_inflation_average[1] <- NULL

###
# Wholesale euro-inflation-adjusted weighted average 
###

# load the average inflation-adjusted wholesale price data
# filter "euro-inflation-adjusted' weighted average
wholesale_inflation_average <- read_excel("price_dollar_western_europe.xlsx",
                                          sheet = 2,
                                          range = "A34:AF55")

# rename the first column to "country"
colnames(wholesale_inflation_average)[1] <- "country"

# use the %>% pipe operator from the "dplyr" to manipulate data
wholesale_inflation_average <- wholesale_inflation_average %>%
  
  # remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # filter out unwanted rows
  filter(country == "Euro-inflation-adjusted weighted* average, in 2020 Euro")

# reshape the data from wide to long format using the pivot_longer() to transform 
# years 1996 to 2019 into a single "year" column 
wholesale_inflation_average <- pivot_longer(
  wholesale_inflation_average,
  cols = c("1996":"2019"),
  names_to = "year",
  values_to = "wholesale_inflation_average")

# remove the "country" column
wholesale_inflation_average[1] <- NULL

# merge() retail and wholesale price-adjusted based on "year" 
# "all = TRUE" to ensure that all years are included 
inflation_average <- merge(wholesale_inflation_average, 
                           retail_inflation_average,
                           by = "year", 
                           all = TRUE)

# clean environment
rm(wholesale_inflation_average, retail_inflation_average)

# merge() based on "year" column
# "all = TRUE" argument to ensure that all years are included 
prices_avg <- merge(inflation_average, 
                    wght_average, 
                    by = "year", 
                    all = TRUE)

# clean environment
rm(wght_average, inflation_average) 

# fwrite() CSV files
fwrite(prices, "prices.csv")
fwrite(prices_avg, "prices_avg.csv")

# End