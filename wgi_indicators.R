# Start 

# Thesis title: The Role of International Commitments in Combating the Illicit 
# Distribution of Cocaine.
# Author: Raquel Baeta

# Data Source: World Governance Indicators (WGI)
# Variables: "Voice and Accountability", "Political Stability and Absence of 
# Violence/Terrorism",  "Government Effectiveness", "Regulatory Quality", "Rule 
# of Law", and "Control of Corruption".

# Step 4.1: Read the data
wgi <- read_excel("wgi_indicators.xlsx")
head(wgi) # preview the first few rows

# Remove rows with any NAs from the data set
wgi <- na.omit(wgi)

# Step 4.2: Rename columns for clarity and later manipulation
colnames(wgi) <- c("country", "code", "year", "timecode", "cc_est", "ge_est", 
                   "ge_no_src", "ge_per_rnk", "ge_per_rnk_lower", 
                   "ge_per_rnk_upper",  "ge_std_err", "pv_est", "pv_no_src", 
                   "pv_per_rnk", "pv_per_rnk_lower", "pv_per_rnk_upper", 
                   "pv_std_err", "rq_est", "rq_no_src", "rq_per_rnk", 
                   "rq_per_rnk_lower", "rq_per_rnk_upper", "rq_std_err", 
                   "rl_est", "rl_no_src", "rl_per_rnk", "rl_per_rnk_lower", 
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

# End 