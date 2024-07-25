# Start 

# title: "Regional Averages for World Goverancne Indicators"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Install
install.packages(c("readr", "tidyverse", "dplyr", "ggplot2", "scales"))
library(readr, tidyverse, dplyr, ggplot2, scales)

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")

# Data Wrangling 
data$year_interval_3yr <- cut(
  data$year, 
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = FALSE)

# Group data
grouped_data_3yr_country <- data %>%
  group_by(region, code, country, year_interval_3yr, any_UN) %>%
  summarise(
    mean_seizures = mean(seizures, na.rm = TRUE),
    mean_log_adjusted_gdp = mean(log_adjusted_gdp, na.rm = TRUE),
    mean_milex_gdp = mean(milex_gdp, na.rm = TRUE),
    mean_trade_ratio = mean(trade_ratio, na.rm = TRUE),
    mean_CC_EST = mean(CC.EST, na.rm = TRUE),
    mean_GE_EST = mean(GE.EST, na.rm = TRUE),
    mean_RQ_EST = mean(RQ.EST, na.rm = TRUE),
    mean_RL_EST = mean(RL.EST, na.rm = TRUE),
    mean_VA_EST = mean(VA.EST, na.rm = TRUE),
    mean_PV_EST = mean(PV.EST, na.rm = TRUE))

# Identify rows with missing values
missing_rows <- anti_join(
  grouped_data_3yr_country, na.omit(grouped_data_3yr_country))

# Display the rows that were removed
print(missing_rows)

# Fill missing values in "year_interval_3yr" with a placeholder text
grouped_data_3yr_country$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr_country$year_interval_3yr), "2017-2019",
  as.character(grouped_data_3yr_country$year_interval_3yr))

head(grouped_data_3yr_country, n = 10)

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# [1] East Asia & the Pacific

# Subset data for East Asia & Pacific
filtered_east_asia_pacific_data <- grouped_data_3yr_country %>%
  filter(region == "East Asia & Pacific")

# Calculate averages
filtered_east_asia_pacific_averages <- filtered_east_asia_pacific_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarise(across(starts_with("mean_"), mean, na.rm = TRUE))

# Plot
east_asia_pacific_wgi_estimates_plot <- ggplot(
  filtered_east_asia_pacific_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(
    yintercept = 0, 
    color = "#DC3545", 
    alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in East Asia & Pacific: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971, and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)"
    ) +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
    ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid")) +
  scale_x_discrete(
    breaks = unique(filtered_east_asia_pacific_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", 
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(east_asia_pacific_wgi_estimates_plot)

# Save the plot as a PDF/PNG
ggsave("east_asia_pacific_wgi_estimates_plot.pdf", plot = east_asia_pacific_wgi_estimates_plot, width = 12, height = 8)
ggsave("east_asia_pacific_wgi_estimates_plot.png", plot = east_asia_pacific_wgi_estimates_plot, width = 12, height = 8)

# [2] Europe & Central Asia

# Subset data for Europe & Central Asia
filtered_europe_central_asia_data <- grouped_data_3yr_country %>%
  filter(region == "Europe & Central Asia")

# Calculate averages
filtered_europe_central_asia_averages <- filtered_europe_central_asia_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarise(across(starts_with("mean_"), mean, na.rm = TRUE))

# Plot
europe_central_asia_wgi_estimates_plot <- ggplot(
  filtered_europe_central_asia_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in Europe & Central Asia: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971, and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)"
    ) +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
    ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid")) +
  scale_x_discrete(
    breaks = unique(filtered_europe_central_asia_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(europe_central_asia_wgi_estimates_plot)

# Save the plot as a PDF/PNG
ggsave("europe_central_asia_wgi_estimates_plot.pdf", plot = europe_central_asia_wgi_estimates_plot, width = 12, height = 8)
ggsave("europe_central_asia_wgi_estimates_plot.png", plot = europe_central_asia_wgi_estimates_plot, width = 12, height = 8)

# [3] Sub-Saharan Africa

# Subset data for Sub-Saharan Africa
filtered_sub_saharan_africa_data <- grouped_data_3yr_country %>%
  filter(region == "Sub-Saharan Africa")

# Calculate averages
filtered_sub_saharan_africa_averages <- filtered_sub_saharan_africa_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  summarise(across(starts_with("mean_"), mean, na.rm = TRUE))

# Plot
sub_saharan_africa_wgi_estimates_plot <- ggplot(
  filtered_sub_saharan_africa_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in Sub-Saharan Africa: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971, and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)"
    ) +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
    ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "#ffffff", color = "lightgrey", size = 1, linetype = "solid")) +
  scale_x_discrete(
    breaks = unique(filtered_sub_saharan_africa_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(sub_saharan_africa_wgi_estimates_plot)

# Save the plot as a PDF/PNG
ggsave("sub_saharan_africa_wgi_estimates_plot.pdf", plot = sub_saharan_africa_wgi_estimates_plot, width = 12, height = 8)
ggsave("sub_saharan_africa_wgi_estimates_plot.png", plot = sub_saharan_africa_wgi_estimates_plot, width = 12, height = 8)


#
# [4] Middle East & North Africa States
#

# Subset data for Middle East & North Africa States and keep country information
filtered_middle_east_north_africa_data <- grouped_data_3yr_country %>%
  filter(region == "Middle East & North Africa")

# Calculate averages
filtered_middle_east_north_africa_averages <- filtered_middle_east_north_africa_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  dplyr::summarize(
    mean_CC_EST = mean(mean_CC_EST),
    mean_GE_EST = mean(mean_GE_EST),
    mean_RQ_EST = mean(mean_RQ_EST),
    mean_RL_EST = mean(mean_RL_EST),
    mean_VA_EST = mean(mean_VA_EST),
    mean_PV_EST = mean(mean_PV_EST)
  )

# Plot
middle_east_north_africa_wgi_esitmates_plot <- ggplot(
  filtered_middle_east_north_africa_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in Middle East & North Africa: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)"
  ) +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
   ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid")
    ) +
  
  scale_x_discrete(
    breaks = unique(filtered_middle_east_north_africa_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", 
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(middle_east_north_africa_wgi_esitmates_plot)

ggsave("middle_east_north_africa_wgi_esitmates_plot_plot.pdf", plot = middle_east_north_africa_wgi_esitmates_plot, width = 12, height = 8)
ggsave("middle_east_north_africa_wgi_esitmates_plot_plot.png", plot = middle_east_north_africa_wgi_esitmates_plot, width = 12, height = 8)

#
# [5] North America
#

# Subset North America
filtered_north_america_data <- grouped_data_3yr_country %>%
  filter(region == "North America")

# Calculate averages
filtered_north_america_averages <- filtered_north_america_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  dplyr::summarize(
    mean_CC_EST = mean(mean_CC_EST),
    mean_GE_EST = mean(mean_GE_EST),
    mean_RQ_EST = mean(mean_RQ_EST),
    mean_RL_EST = mean(mean_RL_EST),
    mean_VA_EST = mean(mean_VA_EST),
    mean_PV_EST = mean(mean_PV_EST)
  )

# Plot
north_america_wgi_esitmates_plot <- ggplot(
  filtered_north_america_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in North America: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)"
  ) +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid")
  ) +
  
  scale_x_discrete(
    breaks = unique(filtered_north_america_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", 
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(north_america_wgi_esitmates_plot)

ggsave("north_america_wgi_esitmates_plot_plot.pdf", plot = north_america_wgi_esitmates_plot, width = 12, height = 8)
ggsave("north_america_wgi_esitmates_plot_plot.png", plot = north_america_wgi_esitmates_plot, width = 12, height = 8)

#
# [6] South Asia
#

# Subset South Asia
filtered_south_asia_data <- grouped_data_3yr_country %>%
  filter(region == "South Asia")

# Filter
filtered_south_asia_averages <- filtered_south_asia_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  dplyr::summarize(
    mean_CC_EST = mean(mean_CC_EST),
    mean_GE_EST = mean(mean_GE_EST),
    mean_RQ_EST = mean(mean_RQ_EST),
    mean_RL_EST = mean(mean_RL_EST),
    mean_VA_EST = mean(mean_VA_EST),
    mean_PV_EST = mean(mean_PV_EST)
  )

# Plot
south_asia_wgi_estimates_plot <- ggplot(
  filtered_south_asia_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in South Asia: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators and United Nations (2024)") +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
    ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid")
    ) +
  
  scale_x_discrete(
    breaks = unique(filtered_south_asia_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(south_asia_wgi_estimates_plot)

# Save the plot as a PDF/PNG
ggsave("south_asia_wgi_estimates_plot.pdf", plot = south_asia_wgi_estimates_plot, width = 14, height = 8)
ggsave("south_asia_wgi_estimates_plot.png", plot = south_asia_wgi_estimates_plot, width = 14, height = 8)

#
# [7] Sub-Saharan Africa  
#

# Subset Sub-Saharan Africa
filtered_sub_saharan_africa_data <- grouped_data_3yr_country %>%
  filter(region == "Sub-Saharan Africa")

# Filter
filtered_sub_saharan_africa_averages <- filtered_sub_saharan_africa_data %>%
  group_by(year_interval_3yr, any_UN) %>%
  dplyr::summarize(mean_CC_EST = mean(mean_CC_EST),
                   mean_GE_EST = mean(mean_GE_EST),
                   mean_RQ_EST = mean(mean_RQ_EST),
                   mean_RL_EST = mean(mean_RL_EST),
                   mean_VA_EST = mean(mean_VA_EST),
                   mean_PV_EST = mean(mean_PV_EST)
  )

# Plot
sub_saharan_africa_wgi_estimates_plot <- ggplot(
  filtered_sub_saharan_africa_averages, aes(x = year_interval_3yr)) +
  
  geom_point(aes(y = mean_CC_EST, color = "mean_CC_EST"), size = 3) +
  geom_line(
    aes(y = mean_CC_EST, color = "mean_CC_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_GE_EST, color = "mean_GE_EST"), size = 3) +
  geom_line(
    aes(y = mean_GE_EST, color = "mean_GE_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RQ_EST, color = "mean_RQ_EST"), size = 3) +
  geom_line(
    aes(y = mean_RQ_EST, color = "mean_RQ_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_RL_EST, color = "mean_RL_EST"), size = 3) +
  geom_line(
    aes(y = mean_RL_EST, color = "mean_RL_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_VA_EST, color = "mean_VA_EST"), size = 3) +
  geom_line(
    aes(y = mean_VA_EST, color = "mean_VA_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_point(aes(y = mean_PV_EST, color = "mean_PV_EST"), size = 3) +
  geom_line(
    aes(y = mean_PV_EST, color = "mean_PV_EST", group = any_UN), 
    size = 2.5, 
    alpha = 0.475) +
  
  geom_hline(yintercept = 0, color = "#DC3545", alpha = 0.8) +
  
  labs(
    title = stringr::str_wrap("Dynamics of Governance in Sub-Saharan Africa: Comparing Indicator Trends and Commitment to United Nations Conventions", width = 70),
    subtitle = stringr::str_wrap("Mean estimates for WGI highlight potential differences between nations adhering to United Nations 1961, 1971 and 1988 Conventions and those not", width = 100),
    y = "Mean WGI Indicator Estimates",
    x = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators and United Nations (2024)") +
  
  scale_color_manual(
    name = "",
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
               "mean_PV_EST" = "Political Stability and Absence of Violence/Terrorism")
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    plot.title = element_text(size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid")
    ) +
  
  scale_x_discrete(
    breaks = unique(filtered_sub_saharan_africa_averages$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")
    ) +
  
  facet_wrap(~any_UN, scales = "free_y", ncol = 1)

print(sub_saharan_africa_wgi_estimates_plot)

# Save the plot as a PDF/PNG
ggsave("sub_saharan_africa_wgi_estimates_plot.pdf", plot = sub_saharan_africa_wgi_estimates_plot, width = 14, height = 8)
ggsave("sub_saharan_africa_wgi_estimates_plot.png", plot = sub_saharan_africa_wgi_estimates_plot, width = 14, height = 8)

# End