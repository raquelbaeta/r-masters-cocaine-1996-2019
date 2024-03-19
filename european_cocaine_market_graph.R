# Start

# title: 'Plotting european cocaine prices i'
# author: Raquel Baeta
# date: 2023-07-16

# Load necessary libraries
library(c(readr, ggplot2, dplyr, stargazer, grDevices))

# working directory
setwd("~/Desktop/working-sessions")

# load data
data <- read_csv("scripts/complete_data.csv")

# convert to euros
data$wholesale <- data$wholesale * 0.95 
data$retail <- data$retail * 0.95 

# convert to kilograms
complete_data$retail <- complete_data$retail * 1000 
complete_data$seizure_log <- complete_data$seizure_log * 1000

# aggregate the data by country and year group, calculating the mean of 
# wholesale, retail, and seizure_log
state_data_agg <- data %>%
  mutate(year_group = cut(year, 
                          breaks = seq(min(year), max(year), by = 4.5), 
                          include.lowest = TRUE)) %>%
  group_by(code, year_group, un1971) %>%
  summarise(across(c(wholesale, retail, seizure_log),
                   mean, na.rm = TRUE))

# print the aggregated data
print(state_data_agg)

###
# Range of seizures, retail and wholesale
###

# overview
summary(state_data_agg[c("seizure_log", "retail", "wholesale")])

# filter highest 'wholesale' value for each year group
wholesale_max <- state_data_agg %>%
  group_by(year_group) %>%
  filter(wholesale == max(wholesale, na.rm = TRUE))

print(wholesale_max) # results

# filter lowest 'wholesale' value for each year group
wholesale_min <- state_data_agg %>%
  group_by(year_group) %>%
  filter(wholesale == min(wholesale, na.rm = TRUE))

print(wholesale_min) # results

# filter highest 'retail' value for each year group
retail_max <- state_data_agg %>%
  group_by(year_group) %>%
  filter(retail == max(retail, na.rm = TRUE))

print(retail_max) # results

# filter lowest 'retail' value for each year group
retail_min <- state_data_agg %>%
  group_by(year_group) %>%
  filter(retail == min(retail, na.rm = TRUE))

print(retail_min) # filter

# filter highest 'seizure_log' value for each year group
seizure_max <- state_data_agg %>%
  group_by(year_group) %>%
  filter(seizure_log == max(seizure_log, na.rm = TRUE))

print(seizure_max) # results

# filter highest 'seizure_log' value for each year group
seizure_min <- state_data_agg %>%
  group_by(year_group) %>%
  filter(seizure_log == min(seizure_log, na.rm = TRUE))

print(seizure_min) # results

###
# Mean of seizures, wholesale and retail prices 
###

# calculate the overall means
overall_avg <- complete_data %>%
  mutate(
    year_group = cut(year, 
                     breaks = seq(min(year), max(year), by = 4.5), 
                     include.lowest = TRUE)) %>%
  group_by(year_group) %>%
  summarise(
    avg_wholesale = mean(wholesale, na.rm = TRUE),
    avg_retail = mean(retail, na.rm = TRUE),
    avg_seizure_log = mean(seizure_log, na.rm = TRUE),
    avg_market_prevalence = mean(c(wholesale, retail, seizure_log), na.rm = TRUE)
  )

print(overall_avg) # results

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
overall_avg <- na.omit(overall_avg)

# Create a bar plot of average market prevalence over time for each country
state_market <- ggplot(
  overall_avg, aes(x = factor(year_group), 
                   y = avg_market_prevalence, 
                   fill = code)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(x = "", 
       y = "Average Market Prevalence (kilograms)", 
       fill = "Country Code",
       title = "A Bar Graph: An Overview View of the Average Market Prevalence in Each European State",
       subtitle = "The averages of Wholesale prices (€), Retail prices (€), Cocaine Seizures (kilograms) from 1996 to 2018") +
  theme_minimal() +
  scale_fill_manual(values = my_colors) +
  scale_x_discrete(labels = c("1996-2000", "2000-2005", "2005-2010", "2010-2014", "2014-2018"))

# print
print(state_market)

# save plot
ggsave("state_market.png", plot = state_market)

# statistical Test 
subset_data <- market_avg %>%
  select(code, year_group, un1971, avg_wholesale, avg_retail, avg_seizure_log, avg_market_prevalence)

# convert to numeric
subset_data$year_group <- as.numeric(subset_data$year_group)

# check for non-numeric values
sapply(subset_data, function(x) any(!is.numeric(x)))

# specify Indicators
indicators <- c("avg_wholesale", "avg_retail", "avg_seizure_log", "avg_market_prevalence")

for (indicator in indicators) {
  cat("T-test for", indicator, "\n")
  
  # t-test
  t_test_results <- t.test(subset_data[[indicator]] ~ subset_data$un1971)
  
  cat("Group 0 (Not signed):\n")
  cat("  Mean:", mean(subset_data[subset_data$un1971 == 0, indicator]), "\n")
  cat("Group 1 (Signed):\n")
  cat("  Mean:", mean(subset_data[subset_data$un1971 == 1, indicator]), "\n")
  print(t_test_results)
  cat("\n")
}

# End