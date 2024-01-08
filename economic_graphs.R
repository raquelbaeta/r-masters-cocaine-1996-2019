# Start 

# Install
install.packages(c("readr", "tidyverse", "ggplot2", "scales", "ggrepel", "dplyr",))

# Load
library(readr) # reading CSV
library(tidyverse) # data manipulation and plotting
library(dplyr) # data manipulation and summarising data
library(ggplot2) # plotting
library(ggrepel) # text labels
library(scales) # Breaks

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#5386E4")

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load data
print(data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv"))

#
# Data Wranggling 
#

# Group data in to intervals
data$year_interval_3yr <- cut(
  data$year, 
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = FALSE)

# Group data
print(grouped_data_3yr <- data %>%
  group_by(region, year_interval_3yr, any_UN, seizures_binary) %>%
  summarise(
    mean_seizures = mean(seizures),
    mean_log_adjusted_gdp = mean(log_adjusted_gdp),
    mean_milex_gdp = mean(milex_gdp),
    mean_exports_gdp = mean(exports_gdp),
    mean_imports_gdp = mean(imports_gdp),
    mean_trade_ratio = mean(trade_ratio))
  )

# Identify rows with missing values
missing_rows <- anti_join(grouped_data_3yr, na.omit(grouped_data_3yr))

# Display the rows that were removed
print(missing_rows)

# Fill missing values in "year_interval_3yr" with the text "2017-2109"
print(grouped_data_3yr$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr$year_interval_3yr), "8",
  grouped_data_3yr$year_interval_3yr))

#
# GDP by Commitment
#

# Convert it using as.factor()
grouped_data_3yr$year_interval_3yr <- as.factor(grouped_data_3yr$year_interval_3yr)

# Plot
log_adjusted_gdp_commitment_plot <- ggplot(
  grouped_data_3yr,
  aes(x = year_interval_3yr,
      y = mean_log_adjusted_gdp,
      color = as.factor(any_UN))) +
  
  # Plotting mean points for each category
  stat_summary(fun = mean, geom = "point", size = 3) +
  
  # Connecting mean points with lines
  geom_line(aes(group = any_UN), stat = "summary", fun = mean, size = 1, linetype = "solid") +
  
  # Adding error bars based on mean and standard deviation
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  
  # Adding plot labels and titles
  labs(title = "Commitment Clash: Drug Policies vs. Economic Growth (1996-2019)",
       subtitle = "Comparing log-adjusted GDP between nations committed and non-committed to United Nations 1961, 1971, and 1988 Conventions",
       x = "",
       y = "Log-Adjusted GDP (Mean ± SD) in USD",
       caption = "Source: World Bank (WB) Data Bank and United Nations Office on Drugs and Crime (UNODC)",
       color = "") +
  
  # Manually setting color values and labels for better clarity
  scale_color_manual(values = c("0" = "#F08030", "1" = "#5386E4"),
                     labels = c("Committed to No Conventions", "Committed to One or More Conventions")) +
  
  # Manually setting fill colors to match the lines
  scale_fill_manual(values = c("#F08030", "#5386E4")) +
  
  # Adjusting x-axis labels for better readability
  scale_x_discrete(
    breaks = unique(grouped_data_3yr$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")) +
  
  # Adding "B" next to each number on the y-axis
  scale_y_continuous(labels = function(x) paste0(x, "B")) +
  
  # Setting the theme for a clean look
  theme_minimal() +
  
  # Adding dashed grid lines for better reference
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        # move legend to top
        legend.position = "top",
        # Center-align x-axis labels
        axis.text.x = element_text(hjust = 0.5),
        # Increasing title font size
        plot.title = element_text(size = 16))

# Print the plot
print(log_adjusted_gdp_commitment_plot)

# Save
ggsave("log_adjusted_gdp_commitment_plot.pdf", 
       plot = log_adjusted_gdp_commitment_plot, width = 14, height = 8)

ggsave("log_adjusted_gdp_commitment_plot.png", 
       plot = log_adjusted_gdp_commitment_plot, width = 14, height = 8)

#
# Regional Trends
#

# Basic plot
ggplot(
  grouped_data_3yr, 
  aes(x = region, y = mean_log_adjusted_gdp, 
      fill = ifelse(
        mean_log_adjusted_gdp > 
          lag(mean_log_adjusted_gdp, 
              default = first(mean_log_adjusted_gdp)), "Increase", "No Increase"))) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Log Adjusted GDP Bar Chart with Year Intervals",
       x = "Region",
       y = "Mean Log Adjusted GDP",
       fill = "Increase in GDP") +
  theme_minimal()

# Merge UN variable
merged_data <- merge(grouped_data_3yr, data, by = c("region", "year_interval_3yr"))

#
# GDP Regional Plot 
#

# Calculate the overall mean for each region
region_means <- merged_data %>%
  group_by(region, any_UN.y) %>%
  summarize(mean_log_adjusted_gdp = mean(mean_log_adjusted_gdp))

# Calculate the percentage increase
region_means <- region_means %>%
  group_by(any_UN.y) %>%
  mutate(
    percentage_increase = (
      mean_log_adjusted_gdp - lag(
        mean_log_adjusted_gdp,
        default = first(
          mean_log_adjusted_gdp))) / abs(lag(
            mean_log_adjusted_gdp, 
            default = first(mean_log_adjusted_gdp))) * 100)

# Check if both Increase and Decrease exist in the dataset
if (all(c("Increase", "Decrease") %in% 
        unique(factor(ifelse(
          region_means$percentage_increase > 0, "Increase", "Decrease"))))) {
  
  # Plotting the mean log-adjusted GDP with percentage increase or decrease
  regional_gdp <- ggplot(
    region_means, 
    aes(x = region, y = mean_log_adjusted_gdp, 
        fill = factor(ifelse(percentage_increase > 0, "Increase", "Decrease"), 
                      levels = c("Decrease", "Increase")))) +
    
    # Creating bar plot
    geom_bar(stat = "identity", color = "white") + 
    
    # Adding labels for percentage increase or decrease
    geom_text(aes(label = ifelse(percentage_increase != 0, paste0(round(abs(percentage_increase), 2), "%"), "")),
              vjust = -0.5, size = 3, color = "black", fontface = "bold") +
    
    # Customizing fill colors for decrease and increase
    scale_fill_manual(
      values = c("Decrease" = "#B3446C", "Increase" = "#38A3A5")) +
    
    # Adding plot titles and labels
    labs(
      title = "Global GDP Patterns Diverge: Commitment to UN Drug Conventions and Regional Economic Shifts (1996-2019)",
      subtitle = "Comparing economic growth or decline across regions with varying levels of commitment to United Nations 1961, 1971, and 1988 Conventions",
      x = "",
      y = "Mean Log-Adjusted GDP in USD",
      fill = "",
      caption = "Source: World Bank (WB) Data Bank and the United Nations (UN)") +
    
    # Customizing y-axis labels
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 11, by = 1),
      labels = function(x) paste0(x, "B")) +
    
    # Applying minimal theme
    theme_minimal() +
    
    # Customizing theme elements
    theme(
      panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
      panel.border = element_blank(),
      plot.title = element_text(size = 16),
      axis.text = element_text(color = "black", hjust = 1),
      axis.text.x = element_text(angle = 50, hjust = 1),  # Center alignment
      
      # Customizing strip background for facet_wrap
      strip.background = element_rect(
        fill = "#ffffff", color = "lightgrey", 
        size = 1, linetype = "solid"), 
      
      # Customizing text in strip
      strip.text = element_text(face = "bold"),
      
      # Positioning legend at the top
      legend.position = "top"
    ) +
    
    # Faceting by any_UN.y with free y-axis scales and 2 columns
    facet_wrap(~any_UN.y, scales = "free_y", ncol = 2)
  } else {
  print("Not enough data for both Increase and Decrease.")
  }

# Print the plot
print(regional_gdp)

# Save
ggsave("regional_trends_mean_gdp.pdf", 
       plot = regional_gdp, width = 14, height = 8)
ggsave("regional_trends_mean_gdp.png", 
       plot = regional_gdp, width = 14, height = 8)

#
# Trade Ratio 
# 

# Group data
print(grouped_data_3yr_country <- data %>%
        group_by(region, code, country, year_interval_3yr) %>%
        summarise(
          mean_seizures = mean(seizures),
          mean_log_adjusted_gdp = mean(log_adjusted_gdp),
          mean_milex_gdp = mean(milex_gdp),
          mean_exports_gdp = mean(exports_gdp),
          mean_imports_gdp = mean(imports_gdp),
          mean_trade_ratio = mean(trade_ratio))
)

# Identify rows with missing values
missing_rows <- anti_join(
  grouped_data_3yr_country, na.omit(grouped_data_3yr_country))

# Display the rows that were removed
print(missing_rows)

# Fill missing values in "year_interval_3yr" with the text "2017-2109"
print(grouped_data_3yr_country$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr_country$year_interval_3yr), "8",
  grouped_data_3yr_country$year_interval_3yr))

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Plot
trade_ratio_gdp <- ggplot(
  grouped_data_3yr_country, 
  aes(x = mean_trade_ratio, y = mean_log_adjusted_gdp, 
      color = region, size = mean_trade_ratio, fill = code)) +
  
  # Add points with transparency
  geom_point(alpha = 0.4) +  
  
  # Customize color scale manually using specified colors
  scale_color_manual(values = colors, name = "") +  
  
  # Customize size scale with continuous values
  scale_size_continuous(
    range = c(2, 18),
    name = "Trade Ratio",
    guide = guide_legend(title = "Trade Ratio"), 
    breaks = seq(2, 18, by = 2)
  ) +
  
  # Customize y-axis labels
  scale_y_continuous(
    breaks = seq(0, 12, by = 1),
    labels = function(x) paste0(x, "B")
  ) +
  
  # Customize x-axis labels with percentage
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  
  # Add plot titles and labels
  labs(
    title = "Trade Ratios and Economic Landscapes: A Regional Perspective (1996-2019)",
    subtitle = "Exploring trade ratios ( as a percentage of GDP) and mean log-adjusted GDP across regions, revealing how varying levels of integration in the global trade network impact national economic performance.",
    x = "Mean Trade Ratio (as % of GDP)",
    y = "Mean Log-Adjusted GDP in USD",
    caption = "Source: World Bank (WB) Data Bank"
  ) +
  
  # Apply a minimal theme for a clean look
  theme_minimal() +
  
  # Customize theme elements
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")
  ) +
  
  # Remove legends for fill and size
  guides(fill = FALSE, size = guide_legend(title = "Trade Ratio"))


# Print
print(trade_ratio_gdp)

# Save the plot as a PDF/PNG
ggsave("trade_ratio_gdp_region_plot.pdf", 
       plot = trade_ratio_gdp, width = 15.2, height = 10)

ggsave("trade_ratio_gdp_region_plot.png", 
       plot = trade_ratio_gdp, width = 15.2, height = 10)

#
# Exports and Imports 
#

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Basic
ggplot(
  data = grouped_data_3yr, 
  aes(x = year_interval_3yr, y = mean_exports_gdp, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Exports as % of Log-Adjusted GDP Over Year Intervals",
       x = "Year",
       y = "Mean Exports (as a % GDP)") +
  theme_minimal()

# Plotting exports

# Order
ordered_regions_exports <- grouped_data_3yr %>%
  group_by(region, year_interval_3yr, any_UN) %>%
  summarize(mean_exports_gdp = mean(mean_exports_gdp)) %>%
  group_by(region) %>%
  summarize(mean_exports_gdp = mean(mean_exports_gdp)) %>%
  arrange(mean_exports_gdp) %>%
  pull(region)

# Set custom color palettes
export_palette <- c("#F08030", "#007BFF", "#C7B8E6", "#B3446C","#FFC107", 
                    "#DC3545", "#38A3A5")

# Grouping data and calculating mean exports
exports_data <- grouped_data_3yr %>% 
  group_by(region, year_interval_3yr, any_UN) %>% 
  summarize(mean_exports_gdp = mean(mean_exports_gdp))

# Create a bar plot with text annotations
exports_plot <- ggplot(
  exports_data,
  aes(x = factor(year_interval_3yr), y = mean_exports_gdp,
      fill = factor(region, levels = ordered_regions_exports))
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(
      label = sprintf("%.1f%%", mean_exports_gdp),
      y = mean_exports_gdp + ifelse(mean_exports_gdp < 10, 1, -1)
    ),
    position = position_stack(vjust = 0.75),
    color = "white", fontface = "bold",
    size = 3
  ) +
  # Customizing fill colors
  scale_fill_manual(values = export_palette) + 
  labs(
    title = "Shifting Export Paths: How Regional Commitments to Drug Conventions Influence Trade Activity (1996-2019)",
    subtitle = "Comparing mean exports of goods and services (as a % of GDP), categorised by commitment to United Nations 1961, 1971 AND 1988 Conventions",
    x = "",
    y = "Mean Exports of Goods and Services (as a % of GDP)",
    fill = "", 
    caption = "Source: World Bank (WB) Data Bank"
  ) +
  # Applying a minimal theme
  theme_minimal() +  
  # Move legend to the top
  theme(legend.position = "top",
        plot.title = element_text(size = 16),
    # Center-align x-axis labels
    axis.text.x = element_text(hjust = 0.5), 
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    # Adding light grey border to facet strip
    strip.background = element_rect(color = "lightgray", fill = "white", size = 1)  
  ) +
  # Set desired breaks for x-axis and corresponding labels
  scale_x_discrete(
    breaks = unique(grouped_data_3yr$year_interval_3yr),  
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")  
  ) +
  # Add percentage formatting to y-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  # Facet by commitment to UN Conventions
  facet_wrap(~any_UN, scales = "free_y", ncol = 2) 

# Print
print(exports_plot)

# Save the plot as a PDF/PNG
ggsave("avg_exports_by_region_plot.pdf", 
       plot = exports_plot, width = 14, height = 10)
ggsave("avg_exports_by_region_plot.png", 
       plot = exports_plot, width = 14, height = 10)

#
# Imports
#

# Plotting imports

# Order
ordered_regions_imports <- grouped_data_3yr %>%
  group_by(region, year_interval_3yr, any_UN) %>%
  summarize(mean_imports_gdp = mean(mean_imports_gdp)) %>%
  group_by(region) %>%
  summarize(mean_imports_gdp = mean(mean_imports_gdp)) %>%
  arrange(mean_imports_gdp) %>%
  pull(region)

# Set custom color palettes
import_palette <- c("#007BFF", "#F08030", "#C7B8E6", "#B3446C","#FFC107", 
                    "#DC3545", "#38A3A5")

# Grouping data and calculating mean imports
imports_data <- grouped_data_3yr %>% 
  group_by(region, year_interval_3yr, any_UN) %>% 
  summarize(mean_imports_gdp = mean(mean_imports_gdp))

# Create a bar plot with text annotations
imports_plot <- ggplot(
  imports_data,
  aes(x = factor(year_interval_3yr), y = mean_imports_gdp,
      fill = factor(region, levels = ordered_regions_imports))
) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(
      label = sprintf("%.1f%%", mean_imports_gdp),
      y = mean_imports_gdp + ifelse(mean_imports_gdp < 10, 1, -1)
    ),
    position = position_stack(vjust = 0.75),
    color = "white", fontface = "bold",
    size = 3
  ) +
  # Customizing fill colors
  scale_fill_manual(values = import_palette) + 
  labs(
    title = "Shifting Import Paths: How Regional Commitments to Drug Conventions Influence Trade Activity (1996-2019)",
    subtitle = "Comparing mean imports of goods and services (as a % of GDP), categorised by commitment to United Nations 1961, 1971 AND 1988 Conventions",
    x = "",
    y = "Mean Imports of Goods and Services (as a % of GDP)",
    fill = "", 
    caption = "Source: World Bank (WB) Data Bank"
  ) +
  # Applying a minimal theme
  theme_minimal() +  
  # Move legend to the top
  theme(legend.position = "top",
        plot.title = element_text(size = 16),
        # Center-align x-axis labels
        axis.text.x = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        # Adding light grey border to facet strip
        strip.background = element_rect(color = "lightgray", fill = "white", size = 1)  
  ) +
  # Set desired breaks for x-axis and corresponding labels
  scale_x_discrete(
    breaks = unique(grouped_data_3yr$year_interval_3yr),  
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")  
  ) +
  # Add percentage formatting to y-axis labels
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  # Facet by commitment to UN Conventions
  facet_wrap(~any_UN, scales = "free_y", ncol = 2) 

# Print
print(imports_plot)

# Save the plot as a PDF/PNG
ggsave("avg_imports_by_region_plot.pdf", 
       plot = imports_plot, width = 14, height = 10)
ggsave("avg_imports_by_region_plot.png", 
       plot = imports_plot, width = 14, height = 10)

#
# Military Expenditure 
#

#
# Milex as % of GDP by Seizures
#

# Convert it using as.factor()
grouped_data_3yr$year_interval_3yr <- as.factor(grouped_data_3yr$year_interval_3yr)

# Plotting log-adjusted GDP over time, color-coded by commitment to UN Conventions
miltex_gdp_seizures_plot <- ggplot(
  grouped_data_3yr,
  aes(x = year_interval_3yr,
      y = mean_milex_gdp,
      color = as.factor(seizures_binary))) +
  
  # Plotting mean points for each category
  stat_summary(fun = mean, geom = "point", size = 3) +
  
  # Connecting mean points with lines
  geom_line(aes(group = seizures_binary), stat = "summary", fun = mean, size = 1, linetype = "solid") +
  
  # Adding error bars based on mean and standard deviation
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  
  # Adding plot labels and titles
  labs(title = "Seized & Spent: Cocaine Seizures & Military Spending as % of GDP (1996-2019)",
       subtitle = "Analysing the economic and military trajectories of nations based on commitment to cocaine seizures",
       x = "",
       y = "Military Expenditure as % of GDP (Mean ± SD)",
       caption = "Source: World Bank (WB) Data Bank and United Nations Office on Drugs and Crime (UNODC)",
       color = "") +
  
  # Manually setting color values and labels for better clarity
  scale_color_manual(values = c("0" = "#B3446C", "1" = "#38A3A5"),
                     labels = c("Not Committed to Seizing Cocaine", 
                                "Committed to Seizing Cocaine")) +
  
  # Manually setting fill colors to match the lines
  scale_fill_manual(values = c("#B3446C", "#38A3A5")) +
  
  # Adjusting x-axis labels for better readability
  scale_x_discrete(
    breaks = unique(grouped_data_3yr$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")) +
  
  # Adding "b" next to each number on the y-axis
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  # Setting the theme for a clean look
  theme_minimal() +
  
  # Adding dashed grid lines for better reference
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        # move legend to top
        legend.position = "top",
        # Rotating x-axis labels for better readability
        axis.text.x = element_text(hjust = 0.5),  # Center alignment
        # Increasing title font size
        plot.title = element_text(size = 16))

# Print the plot
print(miltex_gdp_seizures_plot)

# Save
ggsave("miltex_gdp_seizures_plot.pdf", 
       plot = miltex_gdp_seizures_plot, width = 14, height = 8)
ggsave("miltex_gdp_seizures_plot.png", 
       plot = miltex_gdp_seizures_plot, width = 14, height = 8)

#
# Military Expenditure and GDP 
#

# Group data
print(grouped_data_3yr_country <- data %>%
        group_by(region, code, country, year_interval_3yr) %>%
        summarise(
          mean_seizures = mean(seizures),
          mean_log_adjusted_gdp = mean(log_adjusted_gdp),
          mean_milex_gdp = mean(milex_gdp),
          mean_exports_gdp = mean(exports_gdp),
          mean_imports_gdp = mean(imports_gdp),
          mean_trade_ratio = mean(trade_ratio))
)

# Identify rows with missing values
missing_rows <- anti_join(
  grouped_data_3yr_country, na.omit(grouped_data_3yr_country))

# Display the rows that were removed
print(missing_rows)

# Fill missing values in "year_interval_3yr" with the text "2017-2109"
grouped_data_3yr_country$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr_country$year_interval_3yr), "8",
  grouped_data_3yr_country$year_interval_3yr)

# Plot
miltex_gdp_scatterplot <- ggplot(
  grouped_data_3yr_country, 
  aes(x = mean_milex_gdp, y = mean_log_adjusted_gdp, 
      color = region, size = mean_milex_gdp, fill = code)) +
  
  # Add points with transparency
  geom_point(alpha = 0.375) +  
  
  # Customize color scale manually using specified colors
  scale_color_manual(values = colors, name = "") +  
  
  # Customize size scale with continuous values
  scale_size_continuous(
    range = c(2, 18),
    name = "Milex",
    guide = guide_legend(title = "Milex"), 
    breaks = seq(2, 18, by = 2)
  ) +
  
  # Customize y-axis labels
  scale_y_continuous(
    breaks = seq(0, 12, by = 1),
    labels = function(x) paste0(x, "B")
  ) +
  
  # Customize x-axis labels with percentage
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  
  # Add plot titles and labels
  labs(
    title = "Military Expenditure and Economic Landscapes: A Regional Perspective (1996-2019)",
    subtitle = "Exploring military expenditure (as a % of GDP) and log-adjusted GDP across regions, revealing how varying trade openness interacts with defense priorities",
    x = "Military Expenditure (as % of GDP)",
    y = "Mean Log-Adjusted GDP in USD",
    caption = "Source: World Bank (WB) Data Bank"
  ) +
  
  # Apply a minimal theme for a clean look
  theme_minimal() +
  
  # Customize theme elements
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")
  ) +
  
  # Remove legends for fill and size
  guides(fill = FALSE, size = guide_legend(title = "Milex"))

# Print
print(miltex_gdp_scatterplot)

# Save the plot as a PDF/PNG
ggsave("miltex_gdp_scatterplot_region_plot.pdf", 
       plot = miltex_gdp_scatterplot, width = 12, height = 8)
ggsave("miltex_gdp_scatterplot_plot.png", 
       plot = miltex_gdp_scatterplot, width = 12, height = 8)

#
# Military Expenditure Year trends
#

# Group data
print(grouped_data_3yr <- data %>%
        group_by(region, year_interval_3yr, any_UN) %>%
        summarise(
          mean_seizures = mean(seizures),
          mean_log_adjusted_gdp = mean(log_adjusted_gdp),
          mean_milex_gdp = mean(milex_gdp),
          mean_exports_gdp = mean(exports_gdp),
          mean_imports_gdp = mean(imports_gdp),
          mean_trade_ratio = mean(trade_ratio))
)

# Identify rows with missing values
missing_rows <- anti_join(grouped_data_3yr, na.omit(grouped_data_3yr))

# Display the rows that were removed
print(missing_rows)

# Fill missing values in "year_interval_3yr" with the text "2017-2109"
print(grouped_data_3yr$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr$year_interval_3yr), "8",
  grouped_data_3yr$year_interval_3yr))

# Make sure year_interval_3yr is a numeric vector
grouped_data_3yr$year_interval_3yr <- as.numeric(grouped_data_3yr$year_interval_3yr)

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")


# Define custom labels for year intervals
custom_labels <- c("1996-1998", "1999-2001", "2002-2004", "2005-2007", 
                   "2008-2010", "2011-2013", "2014-2016", "2017-2019")

# Plot military expenditure by region with an academic touch
milex_region <- ggplot(
  grouped_data_3yr,
  aes(
    x = factor(year_interval_3yr, levels = unique(year_interval_3yr)),
    y = mean_milex_gdp, color = region, fill = region, group = region
  )
) +
  
  # Use a continuous line for a smoother look
  geom_path(size = 0.8) +
  
  # Add a transparent ribbon to represent uncertainty or variability
  geom_ribbon(aes(ymin = mean_milex_gdp - sd(mean_milex_gdp),
                  ymax = mean_milex_gdp + sd(mean_milex_gdp), fill = region),
              alpha = 0.12) +
  
  # Assigning custom colors for each region
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  
  # Add informative labels and a subtitle
  labs(
    title = "Mapping Global Security Landscapes: Examining Regional Variations in Military Expenditures as a Share of GDP (1996-2019)",
    subtitle = "Mean and variability, faceting by commitment to United Nations 1961, 1971 and 1988 Conventions",  # Subtitle adjusted to reflect faceting
    x = "",
    y = "Military Expenditure as a % of GDP (Mean ± SD)",
    caption = "Source: World Bank (WB) Data Bank"
  ) +
  
  # Use a theme suitable for academic presentations
  theme_minimal() +
  theme(
    # Align x-axis label
    axis.text.x = element_text(hjust = 0.5),
    # Move legend to the top
    legend.position = "top",
    # Remove legend title
    legend.title = element_blank(),
    # Add grid lines
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    # Adjust caption position
    plot.caption = element_text(hjust = 0),
    # Adding light grey border to facet strip
    strip.background = element_rect(color = "lightgray", fill = "white", size = 1) 
  ) +
  # Set custom labels for x-axis
  scale_x_discrete(labels = custom_labels) +
  # Facet by commitment to UN Conventions
  facet_wrap(~any_UN, scales = "free_y", ncol = 2)

print(milex_region)

# Save the plot as a PDF/PNG
ggsave("avg_miltex_gdp_region_plot.pdf", 
       plot = milex_region, width = 12, height = 8)
ggsave("avg_miltex_gdp_region_plot.png", 
       plot = milex_region, width = 12, height = 8)

# End