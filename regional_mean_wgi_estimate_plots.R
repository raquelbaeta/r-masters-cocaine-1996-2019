# Start 

# Install
install.packages(c("readr", "tidyverse", "dplyr", "ggplot2", "scales"))

library(readr)
library(tidyverse)
library(dplyr) # data manipulation and summarising data
library(ggplot2)
library(scales)

# Set the working directory
setwd("~/Desktop/working-sessions")

# Merging data sets

# Load data
print(data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv"))

#
# Data Wranggling 
#

# Group data in to intervals
data$year_interval_3yr <- cut(data$year, 
                              breaks = seq(1996, 2019, by = 3), 
                              include.lowest = TRUE, 
                              labels = FALSE)

# Group data
print(grouped_data_3yr_country <- data %>%
        group_by(region, code, country, year_interval_3yr, any_UN) %>%
        summarise(mean_seizures = mean(seizures),
                  mean_log_adjusted_gdp = mean(log_adjusted_gdp),
                  mean_milex_gdp = mean(milex_gdp),
                  mean_trade_ratio = mean(trade_ratio),
                  mean_CC_EST = mean(CC.EST),
                  mean_GE_EST = mean(GE.EST),
                  mean_RQ_EST = mean(RQ.EST),
                  mean_RL_EST = mean(RL.EST),
                  mean_VA_EST = mean(VA.EST),
                  mean_PV_EST = mean(PV.EST)))

# Identify rows with missing values
missing_rows <- anti_join(grouped_data_3yr_country, na.omit(grouped_data_3yr_country))

# Display the rows that were removed
print(missing_rows)

# Fill missing values in "year_interval_3yr" with the text "2017-2109"
grouped_data_3yr_country$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr_country$year_interval_3yr), "8", grouped_data_3yr_country$year_interval_3yr)

head(grouped_data_3yr_country, n = 10)

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

#
# [1] East Asia & the Pacific
#

# Subset data for East Asia & Pacific and keep country information
filtered_east_asia_pacific_data <- grouped_data_3yr_country %>%
  filter(region == "East Asia & Pacific")

head(filtered_east_asia_pacific_data, n = 10)

# Group
filtered_east_asia_pacific_averages <- filtered_east_asia_pacific_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST)
  )

head(filtered_east_asia_pacific_averages)

# Plot
east_asia_pacific_wgi_esitmates_plot <- ggplot(filtered_east_asia_pacific_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in East Asia & Pacific: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C", 
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption",  
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  scale_x_discrete(breaks = unique(filtered_east_asia_pacific_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# print
print(east_asia_pacific_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("east_asia_pacific_wgi_esitmates_plot.pdf", plot = east_asia_pacific_wgi_esitmates_plot, width = 12, height = 8)
ggsave("east_asia_pacific_wgi_esitmates_plot.png", plot = east_asia_pacific_wgi_esitmates_plot, width = 12, height = 8)

#
# [2] Europe & Central Asia
#

# Subset data for Europe & Central Asia State and keep country information
filtered_europe_central_asia_data <- grouped_data_3yr_country %>%
  filter(region == "Europe & Central Asia")

head(filtered_europe_central_asia_data, n = 10)

# Filter
filtered_europe_central_asia_averages <- filtered_europe_central_asia_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST))

head(filtered_europe_central_asia_averages)

# Plot
europe_central_asia_wgi_esitmates_plot <- ggplot(filtered_europe_central_asia_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in Europe & Central Asia: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C",
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption", 
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  
  scale_x_discrete(breaks = unique(filtered_europe_central_asia_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# Print
print(europe_central_asia_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("europe_central_asia_wgi_esitmates_plot.pdf", plot = europe_central_asia_wgi_esitmates_plot, width = 12, height = 8)
ggsave("europe_central_asia_wgi_esitmates_plot.png", plot = europe_central_asia_wgi_esitmates_plot, width = 12, height = 8)

#
# [3] Latin America & Caribbean
#

# Subset data for Latin America & Caribbean and keep country information
filtered_latin_america_caribbean_data <- grouped_data_3yr_country %>%
  filter(region == "Latin America & Caribbean")

head(filtered_latin_america_caribbean_data, n = 10)

# Filter
filtered_latin_america_caribbean_averages <- filtered_latin_america_caribbean_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST)
  )

head(filtered_latin_america_caribbean_averages)

# Plot
latin_america_caribbean_wgi_esitmates_plot <- ggplot(filtered_latin_america_caribbean_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in Latin America & Caribbean: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C",
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption", 
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16), 
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  
  scale_x_discrete(breaks = unique(filtered_latin_america_caribbean_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# Print
print(latin_america_caribbean_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("latin_america_caribbean_wgi_esitmates_plot.pdf", plot = latin_america_caribbean_wgi_esitmates_plot, width = 12, height = 8)
ggsave("latin_america_caribbean_wgi_esitmates_plot.png", plot = latin_america_caribbean_wgi_esitmates_plot, width = 12, height = 8)

#
# [4] Middle East & North Africa States
#

# Subset data for Middle East & North African states and keep country information
filtered_middle_east_north_africa_data <- grouped_data_3yr_country %>%
  filter(region == "Middle East & North Africa")

head(filtered_middle_east_north_africa_data, n = 10)

# Filter
filtered_middle_east_north_africa_averages <- filtered_middle_east_north_africa_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST))

head(filtered_middle_east_north_africa_averages)

# Plot
middle_east_north_africa_wgi_esitmates_plot <- ggplot(filtered_middle_east_north_africa_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in Latin America & Caribbean: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C",
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption",  
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  
  scale_x_discrete(breaks = unique(filtered_middle_east_north_africa_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# Print
print(middle_east_north_africa_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("middle_east_north_africa_wgi_esitmates_plot_plot.pdf", plot = middle_east_north_africa_wgi_esitmates_plot, width = 12, height = 8)
ggsave("middle_east_north_africa_wgi_esitmates_plot_plot.png", plot = middle_east_north_africa_wgi_esitmates_plot, width = 12, height = 8)

#
# [5] North America
#

# Subset data for Middle East & North Africa States and keep country information
filtered_north_america_data <- grouped_data_3yr_country %>%
  filter(region == "North America")

head(filtered_north_america_data, n = 10)

# Filter
filtered_north_america_averages <- filtered_north_america_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST))

head(filtered_north_america_averages)

# Plot
north_america_wgi_esitmates_plot <- ggplot(filtered_north_america_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in North America: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C",
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption", 
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  
  scale_x_discrete(breaks = unique(filtered_north_america_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# Print
print(north_america_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("north_america_wgi_esitmates_plot_wgi_esitmates_plot.pdf", plot = north_america_wgi_esitmates_plot, width = 12, height = 8)
ggsave("north_america_wgi_esitmates_plot_wgi_esitmates_plot.png", plot = north_america_wgi_esitmates_plot, width = 12, height = 8)

#
# [6] South Asia
#

# Subset data for South Asia and keep country information
filtered_south_asia_data <- grouped_data_3yr_country %>%
  filter(region == "South Asia")

head(filtered_south_asia_data, n = 10)

# Filter
filtered_south_asia_averages <- filtered_south_asia_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST))

head(filtered_south_asia_averages)

# Plot
south_asia_wgi_esitmates_plot <- ggplot(filtered_south_asia_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in South Asia: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C",
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption", 
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +

  scale_x_discrete(breaks = unique(filtered_south_asia_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# Print
print(south_asia_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("south_asia_wgi_esitmates_plot_plot.pdf", plot = south_asia_wgi_esitmates_plot, width = 12, height = 8)
ggsave("south_asia_wgi_esitmates_plot_plot.png", plot = south_asia_wgi_esitmates_plot, width = 12, height = 8)

#
# [7] Sub-Saharan Africa  
#

# Subset data for Sub-Saharan Africa and keep country information
filtered_sub_saharan_africa_data <- grouped_data_3yr_country %>%
  filter(region == "Sub-Saharan Africa")

head(filtered_sub_saharan_africa_data, n = 10)

# Filter
filtered_sub_saharan_africa_averages <- filtered_sub_saharan_africa_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarize(mean_CC_EST = mean(mean_CC_EST),
            mean_GE_EST = mean(mean_GE_EST),
            mean_RQ_EST = mean(mean_RQ_EST),
            mean_RL_EST = mean(mean_RL_EST),
            mean_VA_EST = mean(mean_VA_EST),
            mean_PV_EST = mean(mean_PV_EST)
  )

head(filtered_sub_saharan_africa_averages)

# Plot
sub_saharan_africa_wgi_esitmates_plot <- ggplot(filtered_sub_saharan_africa_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), size = 2.5, alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = "Dynamics of Governance in Sub-Saharan Africa: Comparing Indicator Trends and Commitment to United Nations Conventions",
    subtitle = "Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not",
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: World Bank (WB) Data Bank") +
  
  scale_color_manual(name = "",
                     values = c("mean_CC_EST" = "#B3446C",
                                "mean_GE_EST" = "#007BFF",
                                "mean_RQ_EST" = "#38A3A5",
                                "mean_RL_EST" = "#C7B8E6",
                                "mean_VA_EST" = "#F08030",
                                "mean_PV_EST" = "#FFC107"),
                     labels = c("mean_CC_EST" = "Control of Corruption", 
                                "mean_GE_EST" = "Government Effectiveness",
                                "mean_RQ_EST" = "Regulatory Quality",
                                "mean_RL_EST" = "Rule of Law",
                                "mean_VA_EST" = "Voice and Accountability",
                                "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        plot.title = element_text(size = 16),
        legend.position = "top",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  
  scale_x_discrete(breaks = unique(filtered_sub_saharan_africa_averages$year_interval_3yr),
                   labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

# Print
print(sub_saharan_africa_wgi_esitmates_plot)

# Save the plot as a PDF/PNG
ggsave("sub_saharan_africa_wgi_esitmates_plot.pdf", plot = sub_saharan_africa_wgi_esitmates_plot, width = 12, height = 8)
ggsave("sub_saharan_africa_wgi_esitmates_plot.png", plot = sub_saharan_africa_wgi_esitmates_plot, width = 12, height = 8)

# End 
