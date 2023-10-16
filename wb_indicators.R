# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Data Source: World Bank (WB)
# Variables: Gross Domestic Product (GDP), Exports of Goods and Services, Imports of Goods and Services Gross National Income (GNI) and 
# Military expenditure

# Step 3.1: Load the World Bank dataset which is stored in an Excel file named "wb_indicators.xlsx" using the read_excel function from 
# the "readxl" package 
wb <- read_excel("wb_indicators.xlsx")
head(wb) # preview the first few rows

# Step 3.2: The raw dataset has columns with long, complex names. Make these column names more informative for ease of manipulation. By 
# renaming the columns, the stage is set for better clarity and organisation in the subsequent steps.
wb <- wb %>% rename("country" = "Country Name", 
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

# Step 3.3: Remove unnecessary columns
wb[4:5] <- NULL  # Remove "yearcode" and "population".

# Remove rows with any NAs from the dataset
wb <- na.omit(wb)

# Exclude specific countries that aren't relevant to the analysis
wb <- wb %>% filter(country != "United States", country != "United Kingdom")

# Step 3.4: Data Transformation, Handling Missing Values, and Export. GDP and Economic Indicators: The data set includes various 
# economic indicators related to GDP, such as "GDP (current US$)," "GDP growth (annual %)," and more. To focus on these indicators, 
# create a subset of the data:

# [3.4.a] Subset the economic-related indicators 
economic_indicators <- wb %>%
  select(country, code, year, gdp_dollar, gdp_growth, gdpcap_dollar, gni_cap, 
         gni_ppp, gnicap_ppp)

# [3.4.b] Extract the data related to exports and imports
exports_imports <- wb %>%
  select(country, code, year, exports_gdp, exports_dollar, exports_growth, 
         imports_gdp, imports_dollar, imports_growth)

# [3.4.c] Subset to analyse military-related variables
military_data <- wb %>%
  select(country, code, year, military_gdp, military_govexp, military_dollar)

# Step 3.5: To identify missing values, use the summary function
summary(economic_indicators)
summary(exports_imports)
summary(military_data)

# Step 3.7: Exporting Data to CSV
write.csv(wb, "wb_indicators.csv", row.names = FALSE)
write.csv(economic_indicators, "economic_indicators.csv", row.names = FALSE)
write.csv(exports_imports, "exports_imports.csv", row.names = FALSE)
write.csv(military_data, "military_data.csv", row.names = FALSE)

# End
