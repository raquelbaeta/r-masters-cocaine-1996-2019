# Start 

# Install packages
install.packages(c("readr", "countrycode", "tidyverse", "dplyr", "ggplot2", "sf", "ggspatial"))

# Load packages
library(readr) # reading CSV
library(countrycode) # country codes
library(tidyverse) # data manipulation and plotting
library(dplyr) # data manipulation and summarising data
library(ggplot2) # plotting
library(sf) # handling spatial data
library(ggspatial) # spatial plotting

# Colour codes 
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Set the working directory
setwd("~/Desktop/working-sessions")

# Map
WorldMap <- map_data("world") %>%
  fortify
head(WorldMap)

# Rename 
colnames(WorldMap)[colnames(WorldMap) == "region"] <- "country"

WorldMap$code <- countrycode(sourcevar = WorldMap$country, origin = "country.name", destination = "iso3c")

# Load data
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")
print(data)

# Group data
commitment_mapping <- data %>%
  group_by(region, code, country, any_UN) %>%
  summarize(total_seizures = sum(seizures), mean_seizures = mean(seizures))

# Check the column names
print(colnames(WorldMap))
print(colnames(commitment_mapping))

WorldData <- left_join(WorldMap, commitment_mapping, by = c("code" = "code"))
head(WorldData)

# Remove unwanted columns
WorldData <- subset(WorldData, select = -c(country.x, subregion))

# Rename 
colnames(WorldData)[colnames(WorldData) == "country.y"] <- "country"

head(WorldData)

# Plot the map with fill based on "any_UN"
convention_commitment_map <- ggplot(WorldData, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(
    fill = factor(any_UN)), color = "gray90", size = 0.3) +
    labs(fill = "") + 
    xlab("Longitude") + 
    ylab("Latitude") +
  scale_fill_manual(
    values = c("0" = "#C7B8E6", "1" = "#B3446C"), labels = c("Committed to None", "Committed to One")) +
  theme_minimal() +
  theme(
    legend.position = "top", panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
        legend.title = element_text(size = 12, margin = margin(t = 5)), legend.text = element_text(size = 10)) +
  ggtitle("Global Distribution of United Nations Drug Control Convention Commitments",
          subtitle = "Nations shaded by their commitment to the 1961, 1971, and 1988 Conventions.") 

# Print
convention_commitment_map

# Save the plot as a PDF
ggsave("convention_commitment_map.pdf", plot = convention_commitment_map, width = 10, height = 6)
ggsave("convention_commitment_map.png", plot = convention_commitment_map, width = 10, height = 6)

# End of Commitment 

#
# Commitment and Seizures
#

# Create a categorical variable for mean_seizures
print(WorldData$mean_seizures_cat <- cut(WorldData$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High")))

# then uses summarize to calculate the mean of "mean_seizures" within each group
print(WorldData %>%
  group_by(mean_seizures_cat) %>%
  summarize(avg_seizures = mean(mean_seizures, na.rm = TRUE)))

# Breaks
print(breaks <- seq(0, 373001, length.out = 5))
# 0, 93125, 186250, 279375, 373001

# Plot using ggplot
WorldData_sf <- st_as_sf(WorldData, coords = c("long", "lat"), crs = 4326)

# Calculate centroids
centroids <- st_centroid(WorldData_sf)

# Create a data frame for centroids
centroids_df <- st_as_sf(data.frame(centroid = centroids))

# Combine WorldData_sf and centroids_df for plotting
combined_data <- cbind(WorldData_sf, st_coordinates(centroids_df))

# Impute missing values with the mean
combined_data$mean_seizures_cat[is.na(combined_data$mean_seizures_cat)] <- mean(combined_data$mean_seizures_cat, na.rm = TRUE)

# Plot again
ggplot() +
  geom_sf(data = combined_data, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf(data = combined_data, aes(color = factor(any_UN)), size = 0.2) +
  geom_sf(data = combined_data, aes(size = mean_seizures_cat), color = "#972D15", alpha = 0.5) +
  scale_fill_manual(values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One")) +
  labs(fill = "Status of Commitment") + 
  theme_minimal() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8), legend.position = "top") +
  ggtitle("United Nations Convention Commitment and Seizure Map", 
          subtitle = "Shading based on annual average cocaine seizures (in kgs) from 1996 to 2019") +
  xlab("Longitude") + 
  ylab("Latitude") +
  guides(size = guide_legend(title = "Average Cocaine Seizures")) +
  scale_size_manual(
    values = c("Low" = 1, "Low-Medium" = 2, "Medium" = 4, "Medium-High" = 6, "High" = 8),
    breaks = c("Low", "Low-Medium", "Medium", "Medium-High", "High"),
    labels = c("Low (0 kgs)", "Low-Medium (93,125 kgs)", "Medium (186,250 kgs)", "Medium-High (279,375 kgs)", "High (373,001 kgs)"),
    guide = "legend") +
  theme(legend.position = "right")

seizure_convention_commitment_map

# Save the plot as a PDF
ggsave("seizure_convention_commitment_map.pdf", plot = seizure_convention_commitment_map, width = 10, height = 6)

# End
