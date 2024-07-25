# Start 

# title: "World Governance Indicator Mapping"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Install
install.packages(c("readr", "countrycode", "rnaturalearth", "sf", "ggplot2", "dplyr", "maps", "mapproj"))
library(c(readr, dplyr, rnaturalearth, sf, countrycode, ggplot2, maps, mapproj, tidyverse, scales))

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")

# Data Wrangling
data$year_interval_3yr <- cut(
  data$year,
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = FALSE
)

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
    mean_PV_EST = mean(PV.EST, na.rm = TRUE)
  )

# Fill missing values in "year_interval_3yr"
grouped_data_3yr_country$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr_country$year_interval_3yr), "2017-2019", grouped_data_3yr_country$year_interval_3yr
)

# Calculate the average of the six columns and create a new column called "wgi"
grouped_data_3yr_country$mean_wgi <- rowMeans(
  grouped_data_3yr_country[, c("mean_CC_EST", "mean_GE_EST", "mean_RQ_EST", "mean_RL_EST", "mean_VA_EST", "mean_PV_EST")], na.rm = TRUE
)

# Identify and count duplicate rows within each group
duplicates <- grouped_data_3yr_country %>%
  group_by(region, code, country, year_interval_3yr) %>%
  filter(n() > 1) %>%
  arrange(region, code, country, year_interval_3yr)

# Load the world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge world map with data
wgi_world_mapping <- merge(
  world, 
  grouped_data_3yr_country, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = TRUE
)

# Convert mean_wgi to numeric
wgi_world_mapping$mean_wgi <- as.numeric(wgi_world_mapping$mean_wgi)

# Create breaks and labels based on quartiles
wgi_world_mapping$mean_wgi <- cut(
  wgi_world_mapping$mean_wgi,
  breaks = c(-Inf, -0.66341, -0.23938, -0.01904, 0.66184, Inf),
  labels = c("Very Low", "Low", "Medium", "High", "Very High")
)

# Define individual colors for each category
colors <- c("Very Low" = "#B3446C", "Low" = "#DC3545", "Medium" = "#F08030", "High" = "#FFC107", "Very High" = "#C7B8E6")

# Create the plot with dots
wgi_plot <- ggplot() +
  geom_sf(
    data = wgi_world_mapping, 
    aes(fill = mean_wgi), 
    color = "white") +
  coord_sf(crs = "+proj=robin") +
  labs(
    title = "Exploring Global Governance: A Geospatial Analysis of World Governance Index (WGI) from 1996 to 2019",
    subtitle = "Patterns and Trends Through Shading: An Examination of World Governance Indicator Estimates Across Nations",
    caption = "Source: World Bank (WB) Data Bank") +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "lightgrey", linetype = "dotted")) +
  scale_fill_manual(values = colors, name = "WGI Estimates")

# Print and save the plot
print(wgi_plot)
ggsave("avg_wgi_plot_region_plot.pdf", plot = wgi_plot, width = 12, height = 8)
ggsave("avg_wgi_plot_region_plot.png", plot = wgi_plot, width = 12, height = 8)

#
# Plot Indicators by Region
#

# Define custom labels for year intervals
custom_labels <- c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")

# Define custom colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# [1] CC Est
grouped_data_3yr_country_mean_CC_EST <- grouped_data_3yr_country %>%
  group_by(region, year_interval_3yr) %>%
  summarise(mean_CC_EST = mean(mean_CC_EST, na.rm = TRUE))

# Arrange and keep only the last observation for each region
last_points <- grouped_data_3yr_country_mean_CC_EST %>%
  group_by(region) %>%
  slice(n())

# Plotting
mean_cc_est_plot <- ggplot(
  grouped_data_3yr_country_mean_CC_EST,
  aes(x = year_interval_3yr, y = mean_CC_EST, fill = region, group = region)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_CC_EST), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "#FF0000", alpha = 0.5) +
  geom_text(
    data = last_points, 
    aes(label = region), 
    vjust = -0.5, 
    size = 3.5, 
    font = "bold", 
    show.legend = FALSE) +
  labs(
    x = "",
    y = "Mean Control of Corruption (CC) Estimate",
    title = "Mean Control of Corruption (CC) Estimate Trends (1996-2019)",
    subtitle = "Patterns and Anomalies of Control of Corruption through Geospatial Exploration by Region",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)") +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.border = element_blank()) +
  scale_fill_manual(values = colors, name = "") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(
    breaks = seq(
      min(grouped_data_3yr_country_mean_CC_EST$mean_CC_EST, na.rm = TRUE),
      max(grouped_data_3yr_country_mean_CC_EST$mean_CC_EST, na.rm = TRUE),
      by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  )

# Print and save the plot
print(mean_cc_est_plot)
ggsave("avg_mean_CC_est_region_plot.pdf", plot = mean_cc_est_plot, width = 12, height = 8)
ggsave("avg_mean_CC_est_region_plot.png", plot = mean_cc_est_plot, width = 12, height = 8)

# [2] GE Est
grouped_data_3yr_country_mean_GE_EST <- grouped_data_3yr_country %>%
  group_by(region, year_interval_3yr) %>%
  summarise(mean_GE_EST = mean(mean_GE_EST, na.rm = TRUE))

# Arrange and keep only the last observation for each region
last_points <- grouped_data_3yr_country_mean_GE_EST %>%
  group_by(region) %>%
  slice(n())

# Plotting
mean_GE_est_plot <- ggplot(
  grouped_data_3yr_country_mean_GE_EST,
  aes(x = year_interval_3yr, y = mean_GE_EST, fill = region, group = region)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_GE_EST), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "#FF0000", alpha = 0.5) +
  geom_text(
    data = last_points, 
    aes(label = region), 
    vjust = -0.5, 
    size = 3.5, 
    font = "bold", 
    show.legend = FALSE) +
  labs(
    x = "",
    y = "Mean Government Effectiveness (GE) Estimate",
    title = "Mean Government Effectiveness (GE) Estimate Trends (1996-2019)",
    subtitle = "Patterns and Anomalies of Government Effectiveness through Geospatial Exploration by Region",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)") +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.border = element_blank()) +
  scale_fill_manual(values = colors, name = "") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(
    breaks = seq(
      min(grouped_data_3yr_country_mean_GE_EST$mean_GE_EST, na.rm = TRUE),
      max(grouped_data_3yr_country_mean_GE_EST$mean_GE_EST, na.rm = TRUE),
      by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  )

# Print and save the plot
print(mean_GE_est_plot)
ggsave("avg_mean_GE_est_region_plot.pdf", plot = mean_GE_est_plot, width = 12, height = 8)
ggsave("avg_mean_GE_est_region_plot.png", plot = mean_GE_est_plot, width = 12, height = 8)

# [3] RQ Est
grouped_data_3yr_country_mean_RQ_EST <- grouped_data_3yr_country %>%
  group_by(region, year_interval_3yr) %>%
  summarise(mean_RQ_EST = mean(mean_RQ_EST, na.rm = TRUE))

# Arrange and keep only the last observation for each region
last_points <- grouped_data_3yr_country_mean_RQ_EST %>%
  group_by(region) %>%
  slice(n())

# Plotting
mean_RQ_est_plot <- ggplot(
  grouped_data_3yr_country_mean_RQ_EST,
  aes(x = year_interval_3yr, y = mean_RQ_EST, fill = region, group = region)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_RQ_EST), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "#FF0000", alpha = 0.5) +
  geom_text(
    data = last_points, 
    aes(label = region), 
    vjust = -0.5, 
    size = 3.5, 
    font = "bold", 
    show.legend = FALSE) +
  labs(
    x = "",
    y = "Mean Regulatory Quality (RQ) Estimate",
    title = "Mean Regulatory Quality (RQ) Estimate Trends (1996-2019)",
    subtitle = "Patterns and Anomalies of Regulatory Quality through Geospatial Exploration by Region",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)") +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.border = element_blank()) +
  scale_fill_manual(values = colors, name = "") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(
    breaks = seq(
      min(grouped_data_3yr_country_mean_RQ_EST$mean_RQ_EST, na.rm = TRUE),
      max(grouped_data_3yr_country_mean_RQ_EST$mean_RQ_EST, na.rm = TRUE),
      by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  )

# Print and save the plot
print(mean_RQ_est_plot)
ggsave("avg_mean_RQ_est_region_plot.pdf", plot = mean_RQ_est_plot, width = 12, height = 8)
ggsave("avg_mean_RQ_est_region_plot.png", plot = mean_RQ_est_plot, width = 12, height = 8)

# [4] RL Est
grouped_data_3yr_country_mean_RL_EST <- grouped_data_3yr_country %>%
  group_by(region, year_interval_3yr) %>%
  summarise(mean_RL_EST = mean(mean_RL_EST, na.rm = TRUE))

# Arrange and keep only the last observation for each region
last_points <- grouped_data_3yr_country_mean_RL_EST %>%
  group_by(region) %>%
  slice(n())

# Plotting
mean_RL_est_plot <- ggplot(
  grouped_data_3yr_country_mean_RL_EST,
  aes(x = year_interval_3yr, y = mean_RL_EST, fill = region, group = region)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_RL_EST), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "#FF0000", alpha = 0.5) +
  geom_text(
    data = last_points, 
    aes(label = region), 
    vjust = -0.5, 
    size = 3.5, 
    font = "bold", 
    show.legend = FALSE) +
  labs(
    x = "",
    y = "Mean Rule of Law (RL) Estimate",
    title = "Mean Rule of Law (RL) Estimate Trends (1996-2019)",
    subtitle = "Patterns and Anomalies of Rule of Law through Geospatial Exploration by Region",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)") +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.border = element_blank()) +
  scale_fill_manual(values = colors, name = "") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(
    breaks = seq(
      min(grouped_data_3yr_country_mean_RL_EST$mean_RL_EST, na.rm = TRUE),
      max(grouped_data_3yr_country_mean_RL_EST$mean_RL_EST, na.rm = TRUE),
      by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  )

# Print and save the plot
print(mean_RL_est_plot)
ggsave("avg_mean_RL_est_region_plot.pdf", plot = mean_RL_est_plot, width = 12, height = 8)
ggsave("avg_mean_RL_est_region_plot.png", plot = mean_RL_est_plot, width = 12, height = 8)

# [5] VA Est
grouped_data_3yr_country_mean_VA_EST <- grouped_data_3yr_country %>%
  group_by(region, year_interval_3yr) %>%
  summarise(mean_VA_EST = mean(mean_VA_EST, na.rm = TRUE))

# Arrange and keep only the last observation for each region
last_points <- grouped_data_3yr_country_mean_VA_EST %>%
  group_by(region) %>%
  slice(n())

# Plotting
mean_VA_est_plot <- ggplot(
  grouped_data_3yr_country_mean_VA_EST,
  aes(x = year_interval_3yr, y = mean_VA_EST, fill = region, group = region)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_VA_EST), alpha = 0.2) +
  geom_hline(yintercept = 0, color = "#FF0000", alpha = 0.5) +
  geom_text(
    data = last_points, 
    aes(label = region), 
    vjust = -0.5, 
    size = 3.5, 
    font = "bold", 
    show.legend = FALSE) +
  labs(
    x = "",
    y = "Mean Voice and Accountability (VA) Estimate",
    title = "Mean Voice and Accountability (VA) Estimate Trends (1996-2019)",
    subtitle = "Patterns and Anomalies of Voice and Accountability through Geospatial Exploration by Region",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators, and United Nations (2024)") +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.border = element_blank()) +
  scale_fill_manual(values = colors, name = "") +
  scale_x_discrete(labels = custom_labels) +
  scale_y_continuous(
    breaks = seq(
      min(grouped_data_3yr_country_mean_VA_EST$mean_VA_EST, na.rm = TRUE),
      max(grouped_data_3yr_country_mean_VA_EST$mean_VA_EST, na.rm = TRUE),
      by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  )

# Print and save the plot
print(mean_VA_est_plot)
ggsave("avg_mean_VA_est_region_plot.pdf", plot = mean_VA_est_plot, width = 12, height = 8)
ggsave("avg_mean_VA_est_region_plot.png", plot = mean_VA_est_plot, width = 12, height = 8)
       
# End 
