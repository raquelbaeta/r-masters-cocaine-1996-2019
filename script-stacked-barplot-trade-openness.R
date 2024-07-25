# Start 

# title: "Stacked Bar Plot on Trade Openess."
# author: "Raquel Baeta"
# date: "2024-07-25"

# Set the working directory
setwd("~/Desktop/working-sessions/maps_thesis")

# Groups the dataset by region, 3-year intervals, and code, calculating means for seizures, GDP, military expenditures, and trade ratio.
print(grouped_data_3yr_region <- data %>%
        group_by(region, year_interval_3yr, code) %>%
        dplyr::summarise(
          mean_seizures = mean(seizures),
          mean_log_adjusted_gdp = mean(log_adjusted_gdp),
          mean_milex_gdp = mean(milex_gdp),
          mean_trade_ratio = mean(trade_ratio))
)

# Handle Missing Data
grouped_data_3yr_region$year_interval_3yr <- factor(grouped_data_3yr_region$year_interval_3yr)
missing_rows <- anti_join(grouped_data_3yr_region, na.omit(grouped_data_3yr_region))
print(missing_rows)

grouped_data_3yr_region$year_interval_3yr <- ifelse(
        is.na(grouped_data_3yr_region$year_interval_3yr), "8", grouped_data_3yr_region$year_interval_3yr)

# Converts year_interval_3yr to a factor for proper x-axis ordering, identifies, 
# and handles missing values.

# Set custom color palettes
trade_ratio_palette <- c("#007BFF", "#38A3A5", "#DC3545", "#B3446C", "#F08030", "#FFC107", "#C7B8E6")

# Summarise Data for Plotting
summarized_data <- grouped_data_3yr_region %>%
  group_by(region, year_interval_3yr, any_UN) %>%
  summarize(mean_trade_ratio = mean(mean_trade_ratio))

# Order data
ordered_summarized_data <- summarized_data %>%
  group_by(region, year_interval_3yr, any_UN) %>%
  dplyr::summarize(mean_trade_ratio = mean(mean_trade_ratio)) %>%
  group_by(region) %>%
  dplyr::summarize(mean_trade_ratio = mean(mean_trade_ratio)) %>%
  arrange(mean_trade_ratio) %>%
  pull(region)

# Plotting
trade_ratio_gdp <- ggplot(
  summarized_data, 
  aes(x = year_interval_3yr, y = mean_trade_ratio, fill = region)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = trade_ratio_palette) +
  labs(
    x = "", 
    y = "Mean Trade Ratio as a % of GDP", 
    fill = "",
    title = "Shifting Trade Openness: How Regional Commitments to Drug Conventions Influence Trade Activity (1996-2019)",
    subtitle = "Comparing mean trade openness (as a % of GDP), categorised by commitment to United Nations 1961, 1971 and 1988 Conventions",
    caption = "Source: Baeta, using data from World Bank, World Devlopment Indicators (2024)") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(
    breaks = unique(grouped_data_3yr$year_interval_3yr),  
    labels = c("1996-1998", "1999-2001", "2002-2004", "2005-2007", "2008-2010",
               "2011-2013", "2014-2016", "2017-2019")) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(size = 16),
        axis.text.x = element_text(hjust = 0.5), 
        panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        strip.background = element_rect(color = "lightgray", fill = "white", size = 1)) +
  facet_wrap(~any_UN, scales = "free_y", ncol = 2)

# Print
print(trade_ratio_gdp)

# Save Plot as a PDF/PNG
ggsave("region_trade_ratio_gdp_region_plot.pdf", plot = trade_ratio_gdp, width = 12, height = 11)
ggsave("region_trade_ratio_gdp_region_plot.png", plot = trade_ratio_gdp, width = 14, height = 11)

# End
