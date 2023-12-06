# Start 

# Install libraries
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("wbstats")
install.packages("WDI")

# Load Libraries
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(wbstats)
library(WDI)

# Set wtd
setwd("~/Desktop/working-sessions/cleaning_data")

# Download data from R packages on CRAN
worldbank_data <- wb_data(indicator = c("NY.GDP.PCAP.CD", "NE.DAB.TOTL.CD", "NE.DAB.TOTL.ZS", 
                                        "NY.ADJ.NNTY.PC.CD", "NE.IMP.GNFS.ZS", "NE.IMP.GNFS.CD", 
                                        "NE.EXP.GNFS.ZS", "NE.EXP.GNFS.CD"),
                          start = 1996, end = 2019)
names(worldbank_data)

# Download regions and extra information 
wb_countries <- wb_countries()
names(wb_countries)

# Merge 
merged_wb_data <- merge(worldbank_data, 
                        y = wb_countries[c("iso3c", "country", "region", "region_iso3c", "lending_type", "lending_type_iso3c",
                                           "income_level", "income_level_iso3c")], 
                        by = "iso3c", all.x = TRUE)
merged_wb_data <- subset(merged_wb_data, region != "Aggregates") # removes NAs

# View colnames 
colnames(merged_wb_data)

# Rename columns
merged_wb_data <- merged_wb_data %>%
  rename(
    region = "region",
    region_iso3c = "region_iso3c",
    country = "country.x",
    code = "iso3c",
    year = "date",
    gdp_cap_dollar = "NY.GDP.PCAP.CD",
    gne_dollar = "NE.DAB.TOTL.CD",
    gne_gdp = "NE.DAB.TOTL.ZS",
    adjusted_nicap_dollar = "NY.ADJ.NNTY.PC.CD",
    income_level = "income_level",
    income_level_iso3c = "income_level_iso3c",
    lending_type = "lending_type",
    lending_type_iso3c = "lending_type_iso3c",        
    exports_dollar = "NE.EXP.GNFS.CD",
    exports_gdp = "NE.EXP.GNFS.ZS",
    imports_dollar = "NE.IMP.GNFS.CD",
    imports_gdp = "NE.IMP.GNFS.ZS",
  ) %>%
  select(region, region_iso3c, country, code, year, 
         gdp_cap_dollar, gne_dollar, gne_gdp, adjusted_nicap_dollar,
         income_level, income_level_iso3c, lending_type, lending_type_iso3c,
         exports_dollar, exports_gdp, imports_dollar, imports_gdp)

# Now, worldbank_data has the columns renamed 
colnames(merged_wb_data)

# Identify countries
unique_countries <- unique(merged_wb_data$country)
print(unique_countries) 

## Adjusting GDP for deflation 
deflator_indicator_code <- "NY.GDP.DEFL.ZS.AD" # Set the indicator code
country_code <- "USA"                          # Set the country code

# Fetch US GDP deflator data
us_deflator_data <- wbstats::wb(indicator = deflator_indicator_code, country = country_code)
colnames(us_deflator_data)
print(head(us_deflator_data)) # structure

# Remove unwanted columns
us_deflator_data <- subset(us_deflator_data, select = -c(iso3c, indicatorID, indicator, iso2c, country))

# Rename 
colnames(us_deflator_data)[colnames(us_deflator_data) == "date"] <- "year"
colnames(us_deflator_data)[colnames(us_deflator_data) == "value"] <- "deflator_us"
str(us_deflator_data)

# Merge the US GDP deflator data
merged_data <- merge(merged_wb_data, us_deflator_data, by.x = "year", by.y = "year")
str(merged_data) # structure

merged_data <- merged_data[, c("region", "region_iso3c", "country", "code", "year", "gdp_cap_dollar", "deflator_us",
                               "gne_dollar", "gne_gdp", "adjusted_nicap_dollar", "income_level", "income_level_iso3c",
                               "lending_type", "lending_type_iso3c", "exports_dollar", "exports_gdp", "imports_dollar", 
                               "imports_gdp")]
str(merged_data) # structure

# Apply the deflator adjustment
merged_data$adjusted_gdp <- merged_data$gdp_cap_dollar / (0.01 * merged_data$deflator_us)
print(head(merged_data$adjusted_gdp)) # check 

# log transform GDP
merged_data$log_adjusted_gdp <- log(merged_data$adjusted_gdp)

# Reorder columns
merged_data <- merged_data[, c("region", "region_iso3c", "country", "code", "year", "log_adjusted_gdp", "adjusted_gdp", 
                               "deflator_us", "gdp_cap_dollar", "gne_dollar", "gne_gdp", "adjusted_nicap_dollar", 
                               "income_level", "income_level_iso3c", "lending_type", "lending_type_iso3c", "exports_dollar", 
                               "exports_gdp", "imports_dollar", "imports_gdp")]
str(merged_data)  # check

# Save as a .csv
write.csv(merged_data, file = "~/Desktop/working-sessions/cleaning_data/government_data.csv", row.names = FALSE)

# Save as a .rds
saveRDS(merged_data, "~/Desktop/working-sessions/government_data.csv.rds")

# End
