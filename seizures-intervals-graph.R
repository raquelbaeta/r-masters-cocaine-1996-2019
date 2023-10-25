# Start 

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stargazer)

# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Aggregate the data into five year intervals.
complete_data <- data[complete.cases(data), ]


# Export the final merged data set to a CSV file named "data.csv"
fwrite(complete_data, "complete_data.csv")

# Create a new column in the data frame that represents the four-year interval for each year
complete_data$year_group <- cut(complete_data$year, breaks = seq(min(complete_data), max(complete_data$year), by = 5))

# Aggregate your data by the year_group column
data_agg <- complete_data %>%
  group_by(year_group) %>%
  summarise_all(mean, na.rm = TRUE)
head(data_agg) # check

# Create bar graph
data_agg$seizure <- data_agg$seizure * 1000
data_agg$seizure <- data_agg$seizure_log * 1000

# With seizures (not logged)
agg_plot <- ggplot(data_agg, aes(x=year_group, y=seizure, fill=year_group)) + 
  geom_bar(stat="identity", position="dodge") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), # change hjust to 0.5 
        legend.title = element_text("Year Intervals"), 
        plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "white")) + 
  labs(x="Intervals", 
       y="Average Cocaine Seizures (in grams)", # change the unit to kg 
       title="European Cocaine Seizures in Five-Year Intervals from 1996 to 2016", 
       fill="Range") + 
  scale_fill_manual(values=c("orange", "darkorange", "steelblue", "navy"), 
                    labels=c("1996-2001", "2001-2006", "2006-2011", "2011-2016")) + 
  scale_y_continuous(labels = scales::comma) + 
  scale_x_discrete(labels=c("1996-2001", "2001-2006", "2006-2011", "2011-2016"))
                                                                        
agg_plot # view plot

# Then, use ggsave() to the plot to a file
ggsave("agg_seizures.png", plot = agg_plot)

# Assuming your data frame is named 'data_agg'
# Select the variables you want to include in the table
selected_vars <- c("year", "un1961", "un1971", "un1988", "wholesale", "retail", "seizure", "seizure_log")

# Check for NA values in the selected variables
if(any(is.na(data_agg[,selected_vars]))) {
  print("The selected variables contain NA values.")
  } else {
  # Create the stargazer table
  stargazer(data_agg[,selected_vars], type = "text")
  }

# End
