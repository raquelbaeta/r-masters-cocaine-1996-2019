# Start 

# title: "Economic Graphs"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Install
install.packages(c("readr", "tidyverse", "ggplot2", "scales", "ggrepel", "dplyr",))

# Load
library(c(readr, tidyverse, dplyr, ggplot2, ggrepel, scales))

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Set the working directory
setwd("~/Desktop/working-sessions/maps_thesis")

# Load Data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")
print(data)

# Data Wrangling
data$year_interval_3yr <- cut(
  data$year, 
  breaks = seq(1996, 2019, by = 3), 
  include.lowest = TRUE, 
  labels = FALSE
)

grouped_data_3yr <- data %>%
  group_by(region, year_interval_3yr, any_UN, seizures_binary) %>%
  summarise(
    mean_seizures = mean(seizures),
    mean_log_adjusted_gdp = mean(log_adjusted_gdp),
    mean_milex_gdp = mean(milex_gdp),
    mean_exports_gdp = mean(exports_gdp),
    mean_imports_gdp = mean(imports_gdp),
    mean_trade_ratio = mean(trade_ratio),
    .groups = 'drop'
  )

# Handling Missing Values
missing_rows <- anti_join(grouped_data_3yr, na.omit(grouped_data_3yr))
print(missing_rows)

grouped_data_3yr$year_interval_3yr <- ifelse(
  is.na(grouped_data_3yr$year_interval_3yr), "8",
  grouped_data_3yr$year_interval_3yr
)

grouped_data_3yr$year_interval_3yr <- as.factor(grouped_data_3yr$year_interval_3yr)

# GDP by Commitment Plot
log_adjusted_gdp_commitment_plot <- ggplot(
  grouped_data_3yr,
  aes(
    x = year_interval_3yr, 
    y = mean_log_adjusted_gdp, 
    color = as.factor(any_UN))) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  geom_line(
    aes(group = any_UN), 
    stat = "summary", 
    fun = mean, 
    size = 1, 
    linetype = "solid") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  labs(
    title = "Commitment Clash: Drug Policies vs. Economic Growth (1996-2019)",
    subtitle = "Comparing log-adjusted GDP between nations committed and non-committed to United Nations 1961, 1971, and 1988 Conventions",
    x = "",
    y = "Log-Adjusted GDP (Mean ± SD) in USD",
    caption = "Source: Baeta, using data from World Bank and United Nations Office on Drugs and Crime (2024)",
    color = ""
  ) +
  scale_color_manual(
    values = c("0" = "#F08030", "1" = "#5386E4"),
    labels = c("Committed to No Conventions", "Committed to One or More Conventions")) +
  scale_x_discrete(
    breaks = unique(grouped_data_3yr$year_interval_3yr),
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019")
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "B")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    legend.position = "top",
    axis.text.x = element_text(hjust = 0.5),
    plot.title = element_text(size = 16)
  )

print(log_adjusted_gdp_commitment_plot)
ggsave("log_adjusted_gdp_commitment_plot.pdf", 
       plot = log_adjusted_gdp_commitment_plot, width = 14, height = 8)
ggsave("log_adjusted_gdp_commitment_plot.png", 
       plot = log_adjusted_gdp_commitment_plot, width = 14, height = 8)

# Regional Trends Plot
regional_gdp <- ggplot(
  region_means, 
  aes(
    x = region, 
    y = mean_log_adjusted_gdp, 
    fill = factor(
      ifelse(percentage_increase > 0, "Increase", "Decrease"), 
      levels = c("Decrease", "Increase")))) +
  geom_bar(stat = "identity", color = "white") + 
  geom_text(
    aes(
      label = ifelse(
        percentage_increase != 0, 
        paste0(round(abs(percentage_increase), 2), "%"), "")), 
    vjust = -0.5, 
    size = 3, 
    color = "black", 
    fontface = "bold") +
  scale_fill_manual(
    values = c("Decrease" = "#B3446C", "Increase" = "#38A3A5")) +
  labs(
    title = "Global GDP Patterns Diverge: Commitment to United Nations Drug Conventions and Regional Economic Shifts (1996-2019)",
    subtitle = "Comparing economic growth or decline across regions with varying levels of commitment to United Nations 1961, 1971, and 1988 Conventions",
    x = "",
    y = "Mean Log-Adjusted GDP in USD",
    fill = "",
    caption = "Source: Baeta, using data from World Bank, World Development Indicators and United Nations (2024)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)), 
    breaks = seq(0, 11, by = 1), 
    labels = function(x) paste0(x, "B")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    panel.border = element_blank(),
    plot.title = element_text(size = 16),
    axis.text = element_text(color = "black", hjust = 1),
    axis.text.x = element_text(angle = 50, hjust = 1),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid"),
    strip.text = element_text(face = "bold"),
    legend.position = "top") +
  facet_wrap(~any_UN.y, scales = "free_y", ncol = 2)

print(regional_gdp)
ggsave("regional_trends_mean_gdp.pdf", 
       plot = regional_gdp, width = 14, height = 8)
ggsave("regional_trends_mean_gdp.png", 
       plot = regional_gdp, width = 14, height = 8)

# Trade Ratio Plot
data$trade_ratio <- data$trade_ratio * 100
grouped_data_3yr_country <- data %>%
  group_by(region, code) %>%
  summarise(
    mean_seizures = mean(seizures),
    mean_log_adjusted_gdp = mean(log_adjusted_gdp),
    mean_milex_gdp = mean(milex_gdp),
    mean_trade_ratio = mean(trade_ratio),
    .groups = 'drop'
  )

trade_ratio_plot <- ggplot(
  grouped_data_3yr_country, 
  aes(x = region, y = mean_trade_ratio, fill = code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Trade Ratios: Drug Policy Commitments vs. Economic Impacts (1996-2019)",
    subtitle = "Analyzing trade ratios in relation to drug policy commitments by region",
    x = "Region",
    y = "Trade Ratio (%)",
    caption = "Source: Baeta, using data from World Bank and United Nations Office on Drugs and Crime (2024)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    legend.position = "top",
    plot.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(
      fill = "#ffffff", 
      color = "lightgrey", 
      size = 1, 
      linetype = "solid"),
    strip.text = element_text(face = "bold")
  )

print(trade_ratio_plot)
ggsave("trade_ratio_plot.pdf", plot = trade_ratio_plot, width = 14, height = 8)
ggsave("trade_ratio_plot.png", plot = trade_ratio_plot, width = 14, height = 8)

# Import/Export Analysis
ordered_regions_imports <- data %>%
  arrange(desc(mean_imports_gdp)) %>%
  pull(region) %>%
  unique()

imports_exports_plot <- ggplot(
  data, 
  aes(x = region, y = mean_imports_gdp, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colors) +
  labs(
    title = "Imports Analysis: Drug Policy Commitments and Regional Trade",
    subtitle = "Exploring the relationship between drug policy commitments and trade imports by region",
    x = "Region",
    y = "Imports as a Percentage of GDP",
    caption = "Source: Baeta, using data from World Bank and United Nations Office on Drugs and Crime (2024)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 16)
  )

print(imports_exports_plot)
ggsave("imports_exports_plot.pdf", plot = imports_exports_plot, width = 14, height = 8)
ggsave("imports_exports_plot.png", plot = imports_exports_plot, width = 14, height = 8)

# End
