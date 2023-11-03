# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(stargazer)
library(grDevices)

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load data
complete_data <- read_csv("scripts/complete_data.csv")

# Convert seizure_log to grams
complete_data$seizure <- complete_data$seizure / 1000 # convert to grams
complete_data$seizure_log <- complete_data$seizure_log / 1000 # convert to grams
complete_data$wholesale <- complete_data$wholesale / 1000 # convert to grams

# Aggregate the data by country and year group, calculating the mean of 
# wholesale, retail, and seizure_log
state_data_agg <- complete_data %>%
  mutate(year_group = cut(year, breaks = seq(min(year), max(year), by = 4.5), include.lowest = TRUE)) %>%
  group_by(code, year_group, un1971) %>%
  summarise(across(c(wholesale, retail, seizure_log), mean, na.rm = TRUE))

# Print the aggregated data
print(state_data_agg)

# Calculate the overall average of wholesale, retail, and seizure_log for 
# each year group
overall_avg <- complete_data %>%
  mutate(year_group = cut(year, breaks = seq(min(year), max(year), by = 4.5), include.lowest = TRUE)) %>%
  group_by(year_group) %>%
  summarise(avg_wholesale = mean(wholesale, na.rm = TRUE),
            avg_retail = mean(retail, na.rm = TRUE),
            avg_seizure_log = mean(seizure_log, na.rm = TRUE),
            avg_market_prevalence = mean(c(wholesale, retail, seizure_log), na.rm = TRUE)
  )

# Print the aggregated data
print(overall_avg)

# Calculate the average of wholesale, retail, and seizure_log for each country 
# and year group
market_avg <- complete_data %>%
  mutate(year_group = cut(year, breaks = seq(min(year), max(year), by = 4.5), include.lowest = TRUE)) %>%
  group_by(code, year_group, un1971) %>%
  summarise(avg_wholesale = mean(wholesale, na.rm = TRUE), 
            avg_retail = mean(retail, na.rm = TRUE),
            avg_seizure_log = mean(seizure_log, na.rm = TRUE),
            avg_market_prevalence = mean(c(wholesale, retail, seizure_log), na.rm = TRUE)
  )

# Print the aggregated data
print(market_avg)

# Define the start and end colors
start_color <- c("#A2A475", "#C6CDF7")  # Light Green
end_color <- c("#E6A0C4", "#7294D4")  # Light Orange

# Create a color palette function
my_palette <- colorRampPalette(c(start_color, end_color))

# Generate a palette with 14 colors
my_colors <- my_palette(14)

# Randomize the colors
my_colors <- sample(my_colors)

# Print the randomized colors
print(my_colors)

# Omit all empty rows from the data
market_avg <- na.omit(market_avg)

# Create a bar plot of average market prevalence over time for each country
state_market <- ggplot(
  market_avg, aes(x = factor(year_group), 
                  y = avg_market_prevalence, 
                  fill = code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", 
       y = "Average Market Prevalence (in grams)", 
       fill = "Country Code",
       title = "A Bar Graph: A Holistic View of the Average Market Prevalence in Each European State",
       subtitle = "The averages of Wholesale prices (in euros), Retail prices (in euros), Cocaine Seizures (in grams) from 1996 to 2018") +
  theme_minimal() +
  scale_fill_manual(values = my_colors) +
  scale_x_discrete(labels=c("1996-2000", "2000-2005", "2005-2010", "2010-2014", "2014-2018"))

# Check the plot
print(state_market)

# Save the plot
ggsave("state_market.png", plot = state_market)

# Statistical Test 
subset_data <- market_avg %>%
  select(code, un1971, avg_wholesale, avg_retail, avg_seizure_log, avg_market_prevalence)

indicators <- c("avg_wholesale", "avg_retail", "avg_seizure_log", "avg_market_prevalence")

for (indicator in indicators) {
  cat("T-test for", indicator, "\n")
  
  t_test_results <- t.test(subset_data[[indicator]] ~ subset_data$un1971)
  
  cat("Group 0 (Not signed):\n")
  cat("  Mean:", mean(subset_data[subset_data$un1971 == 0, indicator]), "\n")
  cat("Group 1 (Signed):\n")
  cat("  Mean:", mean(subset_data[subset_data$un1971 == 1, indicator]), "\n")
  print(t_test_results)
  cat("\n")
}

# End
