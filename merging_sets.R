# Start 

# Thesis title: The Role of International Commitments in Combating the Illicit 
# Distribution of Cocaine.
# Author: Raquel Baeta

# [1] United Nations Treaty data, [2] Cocaine Annual Seizures data , [3] World 
# Bank Government Expenditure data, [4] World Governance Indicators. [5] Annual 
# Prices for cocaine, [6] Annual Public Order Expenditure data.

# Install Required Packages and set working directory
install.packages("readxl") 
install.packages("tidyr") 
install.packages("dplyr") 
install.packages("tidyverse") 
install.packages("countrycode") 
install.packages("data.table") 

# Load Libraries
library(readxl) # work with Excel files
library(tidyr) # data manipulation
library(dplyr) # data manipulation
library(tidyverse) # data manipulation
library(countrycode) # country code conversions
library(data.table) # data manipulation

# Set Working Directory
setwd("/Users/raquelbaeta/Desktop/working_sessions/raw_datasets")

# [1] United Nations Treaty Agreement data

# Step 1.1: Read the Excel file "un_treaty.xlsx" into a data frame named 
# untreaties.
untreaties <- read_excel("un_treaty.xlsx")
head(untreaties) # preview the first few rows

# Step 1.2: Filter out rows corresponding to countries "United States of 
# America" and "United Kingdom" as these countries were dropped from the
# analysis.
untreaties <- untreaties %>%
  filter(country != "United States of America", country != "United Kingdom")

# Step 1.3: Convert the treaty values in columns un1971, un1961, and un1988 to 
# numeric values ("0" for no commitment or "1" for commitment) using the ifelse 
# function.
untreaties$un1971 <- ifelse(untreaties$un1971 > 0, 1, 0)
untreaties$un1961 <- ifelse(untreaties$un1961 > 0, 1, 0)
untreaties$un1988 <- ifelse(untreaties$un1988 > 0, 1, 0)

# Step 1.4: Replace any missing values in the treaty columns with 0.
untreaties$un1971[is.na(untreaties$un1971)] <- 0
untreaties$un1961[is.na(untreaties$un1961)] <- 0
untreaties$un1988[is.na(untreaties$un1988)] <- 0

# Step 1.5: Add new columns code and region to the data frame. The code column 
# contains ISO 3-letter country codes, and the region column contains continent 
# names.
untreaties$code <- countrycode(untreaties$country, 
                               origin = "country.name", 
                               destination = "iso3c")

untreaties$region <- countrycode(untreaties$code, 
                                 origin = "iso3c", 
                                 destination = "continent")

# Step 1.6: Export the cleaned data frame untreaties to a CSV file named 
# "untreaties.csv".
fwrite(untreaties, "untreaties.csv")

# [2] United Nations Office on Drugs an Crime, Annual cocaine seizures.

# Step 2.1: Read the Excel file "unodc_annual_seizure_1990_2019.xlsx" into a 
# data frame  named seizures.
seizures <- read_excel("unodc_annual_seizure_1990_2019.xlsx")
head(seizures) # preview the first few rows

# Step 2.2: Remove columns corresponding to years not included in the study and
# rename columns for easier manipulation.
seizures[6:11] <- NULL
seizures <- seizures %>% 
  rename("region" = "Region", "subregion" = "Sub Region", "country" = "Country", 
         "group" = "Drug Group", "drug" = "Drug")

# Step 2.3: The fill() function fills missing values in categorical columns.
seizures <- fill(seizures, "region", "subregion", "country", "group", "drug")

# Step 2.4: Pivot the data from wide to long format, creating columns for "year" 
# and "seizure".
seizures <- pivot_longer(seizures, 
                         cols = "1996":"2019", 
                         names_to = "year",
                         values_to = "seizure")

# Step 2.5: Filter the data to include only rows where the "group" is "Cocaine-
# type" and the countries are from the specified list.
seizures <- filter(seizures, 
                   group == "Cocaine-type", 
                   country %in% c("Austria", "Belgium", "Denmark", "Finland", 
                                  "France", "Germany", "Greece", "Ireland",
                                  "Italy", "Luxembourg", "Netherlands", "Norway", 
                                  "Portugal", "Spain", "Sweden", "Switzerland"))

# Step 2.6: Convert Year to Numeric
seizures <- mutate(seizures, year = as.numeric(year))
head(seizures, 10) # view the first 10 rows of the cleaned seizures data frame.

# Step 2.7: Export the cleaned data frame seizures to a CSV file named
# "seizures.csv".
fwrite(seizures, "seizures.csv")

# [3] World Bank Government expenditure data

# Step 3.1: Load the World Bank dataset which is stored in an Excel file named 
# "wb_indicators.xlsx" using the read_excel function from the "readxl" package.
wb <- read_excel("wb_indicators.xlsx")
head(wb)

# Step 3.2: The raw dataset has columns with long, complex names. Make these
# column names more informative for ease of manipulation. By renaming the 
# columns, the stage is set for better clarity and organisation in the
# subsequent steps.
wb <- wb %>% rename(
  "country" = "Country Name",
  "code" = "Country Code",
  "year" = "Time",
  "yearcode" = "Time Code",
  "population" = "Population, total [SP.POP.TOTL]",
  "gni_cap" = "GNI per capita, Atlas method (current US$) [NY.GNP.PCAP.CD]",
  "gni_ppp" = "GNI, PPP (current international $) [NY.GNP.MKTP.PP.CD]",
  "gnicap_ppp" = "GNI per capita, PPP (current international $) [NY.GNP.PCAP.PP.CD]",
  "gdp_dollar" = "GDP (current US$) [NY.GDP.MKTP.CD]",
  "gdp_growth" = "GDP growth (annual %) [NY.GDP.MKTP.KD.ZG]",
  "exports_gdp" = "Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]",
  "imports_gdp" = "Imports of goods and services (% of GDP) [NE.IMP.GNFS.ZS]",
  "military_gdp" = "Military expenditure (% of GDP) [MS.MIL.XPND.GD.ZS]",
  "gdpcap_dollar" = "GDP per capita (current US$) [NY.GDP.PCAP.CD]",
  "exports_growth" = "Exports of goods and services (annual % growth) [NE.EXP.GNFS.KD.ZG]",
  "exports_dollar" = "Exports of goods and services (current US$) [NE.EXP.GNFS.CD]",
  "imports_growth" = "Imports of goods and services (annual % growth) [NE.IMP.GNFS.KD.ZG]",
  "imports_dollar" =  "Imports of goods and services (current US$) [NE.IMP.GNFS.CD]",
  "military_dollar" = "Military expenditure (current USD) [MS.MIL.XPND.CD]",
  "military_govexp" = "Military expenditure (% of general government expenditure) [MS.MIL.XPND.ZS]")

# Step 3.3: Remove unnecessary columns and exclude specific countries that 
# aren't relevant to the analysis
wb[4:5] <- NULL  # Remove "yearcode" and "population".
wb <- wb %>% filter(country != "United States", country != "United Kingdom")

# Step 3.4: Data Transformation, Handling Missing Values, and Export

# 3.4.1: GDP and Economic Indicators. The data set includes various economic
# indicators related to GDP, such as "GDP (current US$)", "GDP growth (annual 
# %)," and more. To focus on these indicators, create a subset of the data:
economic_indicators <- wb %>%
  select(country, year, gdp_dollar, gdp_growth, gdpcap_dollar, gni_cap, gni_ppp,
         gnicap_ppp)

# 3.4.2: Extract the data related to exports and imports
exports_imports <- wb %>%
  select(country, year, exports_gdp, imports_gdp, exports_dollar,
         imports_dollar)

# 3.4.3 Subset to analyse military-related variables
military_data <- wb %>%
  select(country, year, military_gdp, military_govexp, military_dollar)

# Step 3.5: To identify missing values, use the summary function
summary(economic_indicators)
summary(exports_imports)
summary(military_data)

# For numeric variables, one approach is to replace missing values with the mean
# of the respective variable.

# 3.5.2: Economic Indicators
economic_indicators <- economic_indicators %>%
  mutate_all(~ ifelse(is.na(.),
                      mean(., na.rm = TRUE), .))

# 3.5.2: Export and Import Indicators
exports_imports <- exports_imports %>%
  mutate_all(~ ifelse(is.na(.),
                      mean(., na.rm = TRUE), .))

# 3.5.3: Military Indicators
military_data <- military_data %>%
  mutate_all(~ ifelse(is.na(.),
                      mean(., na.rm = TRUE), .))

# Step 3.6: Merging variables
merged_data <- left_join(economic_indicators,
                         exports_imports,
                         by = c("country", "year")) %>%
  left_join(., military_data,
            by = c("country", "year"))

# Step 3.7: Exporting Data to CSV
write.csv(merged_data, "merged_data.csv", row.names = FALSE)

# Clean environment
rm(economic_indicators, exports_imports, military_data) 

# [4] World Governance Indicators

# Step 4.1: Read the data
wgi <- read_excel("wgi_indicators.xlsx")
head(wgi) # preview the first few rows

# Remove rows with any NAs from the dataset
wgi <- na.omit(wgi)

# Step 4.2: Rename columns for clarity and later manipulation
colnames(wgi) <- c("country", "code", "year", "timecode", "cc_est", "ge_est", 
                   "ge_no_src", "ge_per_rnk", "ge_per_rnk_lower", 
                   "ge_per_rnk_upper", "ge_std_err", "pv_est", "pv_no_src", 
                   "pv_per_rnk", "pv_per_rnk_lower", "pv_per_rnk_upper", 
                   "pv_std_err", "rq_est", "rq_no_src", "rq_per_rnk", 
                   "rq_per_rnk_lower", "rq_per_rnk_upper", "rq_std_err", "rl_est", 
                   "rl_no_src", "rl_per_rnk", "rl_per_rnk_lower", 
                   "rl_per_rnk_upper", "rl_std_err", "va_est", "va_no_src", 
                   "va_per_rnk", "va_per_rnk_lower", "va_per_rnk_upper", 
                   "va_std_err", "cc_std_err", "cc_per_rnk_upper", 
                   "cc_per_rnk_lower", "cc_per_rnk", "cc_no_src")

# Remove the "timecode" column
wgi[4] <- NULL 

# Step 4.3: Remove rows for "United States" and "United Kingdom"
wgi <- wgi %>%
  filter(country != "United States" & country != "United Kingdom")

# Step 4.4: Export the cleaned data set to a CSV file named "wgi_indicators.csv."
fwrite(wgi, "wgi_indicators.csv")

# Step 4.5: Specify the columns for which you want to calculate averages
columns_wgi_avg <- c("cc_est", "ge_est", "pv_est", "rq_est", "rl_est", "va_est")

# Calculate annual averages for the specified columns
wgi_avg <- wgi %>% 
  
  # Group the data by year
  group_by(year) %>% 
  
  # Calculate the annual averages 
  summarise(across(all_of(columns_wgi_avg), mean))

# Export the dataset containing the calculated annual averages to a CSV file 
# named "annual_wgi.csv."
fwrite(wgi_avg, "wgi_avg.csv")

# [5] Annual Prices for cocaine in Europe

# Step 5.1.1: Load the retail price data from the Excel file "price_dollar_
# western_europe.xlsx" and specify the relevant sheet and range.
retail_prices <- read_excel("price_dollar_western_europe.xlsx",
                            sheet = 2,
                            range = "A5:AF26")

# Rename the first column to "country" for better clarity.
colnames(retail_prices)[1] <- "country"

# Step 5.1.2: Use the %>% pipe operator from the "dplyr" package to perform a 
# series of data manipulation steps.
retail_prices <- retail_prices %>%
  
  # Remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # Filter out unwanted rows
  filter(!country %in%
           c("United Kingdom", "Weighted* average, US$", 
             "Unweighted average, in US$", "Weighted* average, Euro",
             "Euro-inflation-adjusted weighted* average, in 2020 Euro"))

# Step 5.1.3: Reshape the data from wide to long format using the pivot_longer() 
# function from the "tidyr" package. This function helps transform the yearly 
# columns (1996 to 2019) into a single "year" column and their corresponding 
# values into a "retail" column.
retail_prices <- pivot_longer(retail_prices,
                              cols = c("1996":"2019"),
                              names_to = "year",
                              values_to = "retail")

# Step 5.2.1: Load the wholesale price data from the Excel file.
wholesale_prices <- read_excel("price_dollar_western_europe.xlsx",
                               sheet = 2,
                               range = "A34:AF55")

# Rename the first column to "country" for better clarity.
colnames(wholesale_prices)[1] <- "country"

# Step 5.2.2: Use the %>% pipe operator from the "dplyr" package to perform a 
# series of data manipulation steps.
wholesale_prices <- wholesale_prices %>%
  
  # Remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # Filter out unwanted rows
  filter(!country %in%
           c("United Kingdom", "Weighted* average, US$", 
             "Euro-inflation-adjusted weighted* average, in 2020 Euro"
             "Unweighted average, in US$", "Weighted* average, Euro per gram"))

# Step 5.2.3: Reshape the data from wide to long using the pivot_longer()
# function from the "tidyr" package. This function helps transform yearly
# columns (1996 to 2019) into a single "year" column and their corresponding 
# values into a "wholesale" column.
wholesale_prices <- pivot_longer(wholesale_prices,
                                 cols = c("1996":"2019"),
                                 names_to = "year",
                                 values_to = "wholesale")

# Step 5.3: Merge retail and wholesale price data. Use the full_join() function 
# from the "dplyr" package to merge the retail and wholesale price data. Specify 
# the columns "country" and "year" as the joining keys. The result is a data set
# named "cocaine_prices" which contains both retail and wholesale prices for each 
# country and year.
prices <- full_join(retail_prices,
                    wholesale_prices,
                    by = c("country", "year"))

# Clean environment
rm(retail_prices, wholesale_prices)

# Step 5.4: Load and filter for the "Weighted* average, Euro" category.
retail_wght_average <- read_excel("price_dollar_western_europe.xlsx",
                                  sheet = 2,
                                  range = "A5:AF26")

# Rename the first column to "country" for better clarity.
colnames(retail_wght_average)[1] <- "country"

# 4.2: Use the %>% pipe operator from the "dplyr" package to perform a series of 
# data manipulation steps.
retail_wght_average <- retail_wght_average %>%
  
  # Remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # Filter out unwanted rows
  filter(country == "Weighted* average, Euro")

# 4.3: Reshape the data from wide to long using the pivot_longer() function from
# the "tidyr" package. This function helps transform yearly columns (1996 to 
# 2019) into a single "year" column and their corresponding values into a 
# "wholesale" column.
retail_wght_average <- pivot_longer(retail_wght_average,
                                    cols = c("1996":"2019"),
                                    names_to = "year",
                                    values_to = "retail_wgted_average")

# Remove the "country" column
retail_wght_average[1] <- NULL

# Step 5.5: Load and filter for the "Weighted* average, Euro per gram" category.
wholesale_wght_average <- read_excel("price_dollar_western_europe.xlsx",
                                     sheet = 2,
                                     range = "A34:AF55")

# Rename the first column to "country" for better clarity.
colnames(wholesale_wght_average)[1] <- "country"

# Step 5.5.2: Use the %>% pipe operator from the "dplyr" package to perform a 
# series of data manipulation steps.
wholesale_wght_average <- wholesale_wght_average %>%
  
  # Remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # Filter out unwanted rows
  filter(country == "Weighted* average, Euro per gram")

# Step 5.5.3: Reshape the data from wide to long using the pivot_longer() 
# function from the "tidyr" package. This function helps transform yearly 
# columns (1996 to 2019) into a single "year" column and their corresponding 
# values into a "wholesale" column.
wholesale_wght_average <- pivot_longer(wholesale_wght_average,
                                       cols = c("1996":"2019"),
                                       names_to = "year",
                                       values_to = "wholesale_wgted_average")

# Remove the "country" column
wholesale_wght_average[1] <- NULL

# Step 5.6: Use the merge() function to combine the different average price and
# wholesale data sets based on the "year" column. "all = TRUE" argument to
# ensure that all years are included in the final data set.
wght_average <- merge(retail_wght_average,
                      wholesale_wght_average,
                      by = "year",
                      all = TRUE)

# Clean environment
rm(retail_wght_average, wholesale_wght_average)

# Step 5.7: Load and filter for the "Euro-inflation-adjusted weighted* average,
# in 2020 Euro" category.
retail_inflation_average <- read_excel("price_dollar_western_europe.xlsx",
                                       sheet = 2,
                                       range = "A5:AF26")

# Rename the first column to "country" for better clarity.
colnames(retail_inflation_average)[1] <- "country"

# Step 5.7.2: Use the %>% pipe operator from the "dplyr" package to perform a
# series of data manipulation steps.
retail_inflation_average <- retail_inflation_average %>%
  
  # Remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # Filter out unwanted rows
  filter(country == "Euro-inflation-adjusted weighted* average, in 2020 Euro")

# Step 5.7.3: Reshape the data from wide to long using the pivot_longer() 
# function from the "tidyr" package. This function helps transform yearly 
# columns (1996 to 2019) into a single "year" column and their corresponding 
# values into a "wholesale" column.
retail_inflation_average <- pivot_longer(retail_inflation_average,
                                         cols = c("1996":"2019"),
                                         names_to = "year",
                                         values_to = "retail_inflation_average")

# Remove the "country" column
retail_inflation_average[1] <- NULL

# Step 5.8: Load the average inflation-adjusted wholesale price data and filter 
# for the "Euro-inflation-adjusted weighted* average, in 
# 2020 Euro" category.
wholesale_inflation_average <- read_excel("price_dollar_western_europe.xlsx",
                                          sheet = 2,
                                          range = "A34:AF55")

# Rename the first column to "country" for better clarity.
colnames(wholesale_inflation_average)[1] <- "country"

# Step 5.8.2: Use the %>% pipe operator from the "dplyr" package to perform a 
# series of data manipulation steps.
wholesale_inflation_average <- wholesale_inflation_average %>%
  
  # Remove unwanted years
  select(-c("1990", "1991", "1992", "1993", "1994", "1995", "2020")) %>%
  
  # Filter out unwanted rows
  filter(country == "Euro-inflation-adjusted weighted* average, in 2020 Euro")

# Step 5.8.3: Reshape the data from wide to long using the pivot_longer() 
# function from the "tidyr" package. This function helps transform yearly columns 
# (1996 to 2019) into a single "year" column and their corresponding values into
# a "wholesale" column.
wholesale_inflation_average <- pivot_longer(wholesale_inflation_average,
                                            cols = c("1996":"2019"),
                                            names_to = "year",
                                            values_to = "wholesale_inflation_average")

# Remove the "country" column
wholesale_inflation_average[1] <- NULL

# Step 5.9: Use the merge() function to combine the different average retail and 
# wholesale price-adjusted data sets based on the "year" column. "all = TRUE" 
# argument to ensure that all years are included in the final data set.
inflation_average <- merge(wholesale_inflation_average,
                           retail_inflation_average,
                           by = "year",
                           all = TRUE)

# Clean environment
rm(wholesale_inflation_average, retail_inflation_average)

# Step 5.10: Use the merge() function to combine the different data sets based 
# on the "year" column. "all = TRUE" argument to ensure that all years are 
# included in the final data set.
prices_avg <- merge(inflation_average,
                    wght_average,
                    by = "year",
                    all = TRUE)

# Clean environment
rm(wght_average, inflation_average)

# Step 5.11: Using the fwrite() function from the "data.table" package create 
# two CSV files named  "prices.csv" and "prices_avg.csv".
fwrite(prices, "prices.csv")
fwrite(prices_avg, "prices_avg.csv")

# [6] Annual Public Order Expenditure data.

# Step 6.1: Load the data set from the Eurostat.
public_order <- read_excel(path = "~/Desktop/working_sessions/raw_datasets/public_order_spending_eurostat.xlsx", 
                           sheet = 3,
                           range = "A11:AV29")
head(public_order)

# Step 6.2: Remove the first row and rename the "TIME" column to "country".
public_order <- public_order[-1, ]
colnames(public_order)[1] <- "country" # and rename the first column.

# Step 6.3: Convert the columns that are characters to numeric.
public_order$"1996" <- as.numeric(public_order$"1996")
public_order$"1997" <- as.numeric(public_order$"1997")

# Step 6.4: Define a function to remove columns based on names
remove_columns <- function(data, column_names) {
  column_indices <- which(names(data) %in% column_names)
  data <- data[, -column_indices, drop = FALSE]
  return(data)
}

# List of column names to be removed
columns_to_remove <- paste0("...", seq(3, 47, by = 2))

# Apply the function to remove columns
public_order <- remove_columns(public_order, columns_to_remove)

# Step 6.5: Transform the data set using tidyr's pivot_longer function.
public_order <- pivot_longer(public_order, 
                             cols = c("1996", "1997", "1998", "1999", "2000", 
                                      "2001", "2002", "2003", "2004", "2005", 
                                      "2006", "2007", "2008", "2009", "2010", 
                                      "2011", "2012", "2013", "2014", "2015", 
                                      "2016", "2017", "2018", "2019"),
                             names_to = "year", 
                             values_to = "public_order")
head(public_order) # check the pivot

# Export the cleaned and reshaped data frame to a CSV file named "public_order.csv".
fwrite(public_order, "public_order.csv")

# [7] Merging data sets

# Step 7.1: Merge "cocaine," "seizures," and "public_order" by "country" and "year"
prices_kg <- full_join(prices, 
                       public_order, 
                       by = c("country", "year"))

# Convert "year" in "prices_kg" to numeric.
prices_kg$year <- as.numeric(prices_kg$year)

# Step 7.2: Merge "prices_kg" and "seizures"
prices_kg <- full_join(prices_kg, 
                       seizures, 
                       by = c("country", "year"))

# Clean the environment 
rm(prices, public_order, seizures)

# Step 7.3: Add country codes using countrycode function
prices_kg$code <- countrycode(prices_kg$country, 
                              origin = "country.name", 
                              destination = "iso3c")

# Step 7.4: Merge "prices_kg" and "untreaties" by "country", "region", "code"
cocaine <- full_join(prices_kg, 
                     untreaties, 
                     by = c("country", "region", "code"))

# Clean environment
rm(prices_kg, untreaties)

# Step 7.5: Join "wb" and "wgi" as "country", "code", and "year".
wb_wgi <- full_join(wb, 
                    wgi, 
                    by = c("country", "code", "year"))

# Clean environment
rm(wb, wgi)

# Step 7.6: Merge "wb_wgi" and "cocaine" by "country", "code", and "year".
data <- full_join(wb_wgi, 
                  cocaine, 
                  by = c("country", "code", "year"))

# Step 7.7: Convert the columns that are characters to numeric.
data_reordered$gni_cap <- as.numeric(data_reordered$gni_cap)
data_reordered$gni_ppp <- as.numeric(data_reordered$gni_ppp)
data_reordered$gnicap_ppp <- as.numeric(data_reordered$gnicap_ppp)

# Step 7.8.: Convert seizure values from kilograms to grams
data$seizure <- data$seizure * 1000

# Step 7.9: Perform Logarithmic transformation
data$seizure_log <- log(data$seizure + 1)
data$gdp_dollar_log <- log(data$gdp_dollar + 1)
data$gdpcap_dollar_log <- log(data$gdpcap_dollar + 1)

# Step 7.10.1: Reorder columns for consistent order
desired_column_order <- c("region", "subregion", "code", "country", "year", 
                          "un1961", "un1971", "un1988", "drug", "wholesale",
                          "retail", "seizure", "seizure_log", "gni_cap", 
                          "gni_ppp", "gnicap_ppp", "gdp_growth", "gdp_dollar", 
                          "gdp_dollar_log", "gdpcap_dollar", "gdpcap_dollar_log",
                          "exports_gdp", "exports_growth", "exports_dollar", 
                          "imports_gdp", "imports_growth", "imports_dollar", 
                          "military_gdp", "military_govexp", "military_dollar", 
                          "public_order", "cc_est", "cc_no_src", "cc_per_rnk",
                          "cc_per_rnk_upper", "cc_per_rnk_lower", "cc_std_err",
                          "ge_est", "ge_no_src", "ge_per_rnk", 
                          "ge_per_rnk_lower", "ge_per_rnk_upper", "ge_std_err",
                          "pv_est", "pv_no_src", "pv_per_rnk",
                          "pv_per_rnk_lower", "pv_per_rnk_upper", "pv_std_err", 
                          "rq_est", "rq_no_src", "rq_per_rnk", 
                          "rq_per_rnk_lower", "rq_per_rnk_upper", "rq_std_err",
                          "rl_est", "rl_no_src", "rl_per_rnk", 
                          "rl_per_rnk_lower", "rl_per_rnk_upper", "rl_std_err", 
                          "va_est", "va_no_src", "va_per_rnk", 
                          "va_per_rnk_lower", "va_per_rnk_upper", "va_std_err")

# Step 7.10.2:Reorder the columns based on the desired order
data_reordered <- data %>%
  select(all_of(desired_column_order))

colnames(data_reordered) # check column names 

# Step 7.11: Export the final merged data set to a CSV file named "data.csv"
fwrite(data_reordered, "data.csv")

# Clean environment
rm(wb_wgi, cocaine, data)

# Step 7.12.1: Merge the annual averages. Convert "year" in "prices_kg" to 
# numeric.
prices_avg$year <- as.numeric(prices_avg$year)

# Step 7.12.2: Merge using full_join()
prices_wgi_avg <- full_join(prices_avg, 
                            wgi_avg, 
                            by = "year")

# Step 13: Export the final merged data set to a CSV file named "data.csv"
fwrite(prices_wgi_avg, "prices_wgi_avg.csv")

# Clean up: Remove original data frames
rm(prices_avg, wgi_avg)

# [5] Aggregate the data into five year intervals.
complete_data <- data[complete.cases(data), ]

# Step 5.2: Add a new column for the five-year interval
aggregated_fiveyear_data <- complete_data %>%
  mutate(interval = floor((year - 1996) / 5) * 5 + 1996)

# Step 5.3: List of variables to summarise
summary_vars <- c("wholesale","retail", "seizure", "seizure_log", "gni_cap", 
                  "gni_ppp", "gnicap_ppp", "gdp_growth", "gdp_dollar", 
                  "gdp_dollar_log", "gdpcap_dollar", "gdpcap_dollar_log", 
                  "exports_gdp", "exports_growth", "exports_dollar", 
                  "imports_gdp", "imports_growth", "imports_dollar", 
                  "military_gdp", "military_govexp", "military_dollar",
                  "public_order", "cc_est", "cc_no_src", "cc_per_rnk", 
                  "cc_per_rnk_upper", "cc_per_rnk_lower", "cc_std_err",
                  "ge_est", "ge_no_src", "ge_per_rnk", "ge_per_rnk_lower", 
                  "ge_per_rnk_upper", "ge_std_err", "pv_est", "pv_no_src",
                  "pv_per_rnk", "pv_per_rnk_lower", "pv_per_rnk_upper", 
                  "pv_std_err", "rq_est", "rq_no_src", "rq_per_rnk",  
                  "rq_per_rnk_lower", "rq_per_rnk_upper", "rq_std_err", 
                  "rl_est", "rl_no_src", "rl_per_rnk", "rl_per_rnk_lower", 
                  "rl_per_rnk_upper", "rl_std_err", "va_est", "va_no_src", 
                  "va_per_rnk", "va_per_rnk_lower", "va_per_rnk_upper", 
                  "va_std_err")

# Step 5.4: Aggregate data by five-year intervals and calculate summary 
# statistics for each variable
aggregated_fiveyear_data <- aggregated_fiveyear_data %>%
  group_by(interval) %>%
  summarize(across(all_of(summary_vars), 
                   list(mean = ~ mean(., na.rm = TRUE), 
                        sum = ~ sum(., na.rm = TRUE)), .names = "{.col}_{.fn}"),
            .groups = "drop")

# Step 5.5: Export the data frames to a CSV file named "aggregated_fiveyear_data".
fwrite(aggregated_fiveyear_data, "aggregated_fiveyear_data.csv")

# End 