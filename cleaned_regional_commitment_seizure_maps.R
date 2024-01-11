# Start 

# Install packages
install.packages(c("readr", "countrycode", "tidyverse", "dplyr", "ggplot2", "sf", "ggspatial", "rworldmap", "rworldxtra", 
                    "RColorBrewer", "maptools", "classInt", "ggtext"))
install.packages("https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.3/rworldmap_1.3-8.tgz", repos = NULL, type = "source")

## Load packages
library(sf)
library(rnaturalearth)
library(ggrepel)
library(stringr)
library(readr) # reading CSV
library(countrycode) # country codes
library(tidyverse) # data manipulation and plotting
library(dplyr) # data manipulation and summarising data
library(ggplot2) # plotting
library(sf) # handling spatial data
library(ggspatial) # spatial plotting

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Set the working directory
setwd("~/Desktop/working-sessions")

# Merging data sets

# Load data
print(data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv"))

# Group data
print(grouped_data <- data %>%
  group_by(region, code, country, any_UN, UN1961, UN1971, UN1988) %>%
  summarize(mean_seizures = mean(seizures)))

# Filter the data for East Asia & Pacific region
east_asia_data <- grouped_data[grouped_data$region == "East Asia & Pacific", ]
unique(east_asia_data$country)

# Download the world map dataset form natrualearth()
world <- ne_countries(scale = "large", returnclass = "sf")

#
#
# [1] East Asia & the Pacific
#
#

library(ggplot2)
library(sf)
library(gtable)
library(ggrepel)
library(ggspatial)
library(grid)
library(gridExtra)

# Merging East Asia
east_asia_mapping <- merge(world, east_asia_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(east_asia_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(east_asia_mapping) # Print the current CRS of the dataset
east_asia_mapping <- st_set_crs(east_asia_mapping, 4326)

# Specify the desired bounding box for East Asia and the Pacific
east_asia_bbox <- c(xmin = 10, xmax = 180, ymin = -45, ymax = 50)

# Plot the map with a zoomed-in view of East Asia and the Pacific
ggplot() +
  geom_sf(data = east_asia_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_label(data = st_centroid(east_asia_mapping), aes(label = iso_a3), size = 2, color = "black", fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  labs(title = "United Nations Convention Commitment and Seizures in East Asia & the Pacific",
       subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(xlim = c(east_asia_bbox["xmin"], east_asia_bbox["xmax"]), ylim = c(east_asia_bbox["ymin"], east_asia_bbox["ymax"]))

# Create a categorical variable for average seizures
east_asia_mapping$category <- cut(east_asia_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Breaks
print(breaks <- seq(0, 700, length.out = 5))
# 0, 175, 350, 525, 700

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Plot seizures in East Asia & the Pacific
eap_un_cocaine_plot <- ggplot() +
  geom_sf(data = east_asia_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(east_asia_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.3, point.padding = 0.5, parse = TRUE) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = east_asia_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", color = "#DC3545", 
    alpha = 0.6, stroke = 0.4) +
  scale_size_continuous(
    range = c(2, 12), name = "Cocaine Seizures", breaks = c(0, 175, 350, 525, 700),
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (175 kg)", "Medium (350 kg)", "Medium-High (525 kg)", "High (700 kg)"), width = 50)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.title = element_text(size = 10),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"), legend.text = element_text(size = 8),
    legend.position = c(0.195, 0.48),  legend.background = element_rect(color = "gray", linetype = "dashed")) +
 labs(
   title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of East Asia & the Pacific (1996-2019)", width = 70), 
   subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 90)
   caption = "United Nations Office on Drugs and Crime (UNODC)") +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(east_asia_bbox["xmin"], east_asia_bbox["xmax"]), ylim = c(east_asia_bbox["ymin"], east_asia_bbox["ymax"])) +
  guides(size = guide_legend(title = "Mean Cocaine Seizures"))


eap_un_cocaine_plot # print

# Save the plot as a PDF/PNG
ggsave("eap_un_cocaine_plot.pdf", plot = eap_un_cocaine_plot, width = 14, height = 8.5)
ggsave("eap_un_cocaine_plot.png", plot = eap_un_cocaine_plot, width = 14, height = 8.5)

#
#
# [2] Europe & Central Asia
#
#

# Filter the data for Europe & Central Asia region
europe_asia_data <- grouped_data[grouped_data$region == "Europe & Central Asia", ]
unique(europe_asia_data$country)

# Merge
europe_central_asia_mapping <- merge(world, europe_asia_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(europe_central_asia_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(europe_central_asia_mapping) # Print the current CRS of the data set
europe_central_asia_mapping <- st_set_crs(europe_central_asia_mapping, 4326)

# Specify the desired bounding box for Europe and Central Asia
europe_central_asia_bbox <- c(xmin = -10, xmax = 175, ymin = 35, ymax = 80)

# Plot the map with a zoomed-in view of Europe and Central Asia
ggplot() +
  geom_sf(data = europe_central_asia_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_label(data = st_centroid(europe_central_asia_mapping), aes(label = iso_a3), size = 2, color = "black", fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in Europe & Central Asia",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(europe_central_asia_bbox["xmin"], europe_central_asia_bbox["xmax"]), 
    ylim = c(europe_central_asia_bbox["ymin"], europe_central_asia_bbox["ymax"]))

# Create a categorical variable for average seizures
europe_central_asia_mapping$category <- cut(
  europe_central_asia_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# group_by() to group the data by the levels of "total_seizures_cat" and 
# then uses summarize to calculate the mean of "total_seizures" within each group
print(seizures_summary <- europe_central_asia_mapping %>%
        group_by(category) %>%
        summarize(avg_seizures = mean(mean_seizures, na.rm = TRUE)))

# breaks
print(breaks <- seq(0, 13000, length.out = 5))
# 0, 3250, 6500, 9750, 13000

# Plot
eca_un_cocaine_plot <- ggplot() +
  geom_sf(data = europe_central_asia_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(europe_central_asia_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.3, point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = europe_central_asia_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", 
             color = "#DC3545", alpha = 0.7, stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), name = "Mean Cocaine Seizures", breaks = c(0, 3250, 6500, 9750, 13000),
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (3,250 kg)", "Medium (6,500 kg)", "Medium-High (9,750 kg)", "High (13,000 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.text = element_text(size = 8),
    legend.title = element_text(size = 10), legend.position = c(0.8, 0.55),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Europe & Central Asia (1996-2019)", width = 70), 
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 90),
    caption = "Source: United Nations Office on Drugs and Crime (UNODC)") +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(europe_central_asia_bbox["xmin"], europe_central_asia_bbox["xmax"]),
    ylim = c(europe_central_asia_bbox["ymin"], europe_central_asia_bbox["ymax"])) +
  guides(size = guide_legend(title = "Average Cocaine Seizures"))

# Print
print(eca_un_cocaine_plot)

# Save the plot as a PDF
ggsave("eca_un_cocaine_plot.pdf", plot = eca_un_cocaine_plot, width = 14, height = 7)
ggsave("eca_un_cocaine_plot.png", plot = eca_un_cocaine_plot, width = 14, height = 7)

#
#
# [3] Latin America & Caribbean
#
#

# Filter the data for Latin America & Caribbean region
latin_america_carribean_data <- grouped_data[grouped_data$region == "Latin America & Caribbean", ]
unique(latin_america_carribean_data$country)

# Merge
latin_america_carribean_mapping <- merge(world, latin_america_carribean_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(latin_america_carribean_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(latin_america_carribean_mapping) # Print the current CRS of the dataset
latin_america_carribean_mapping <- st_set_crs(latin_america_carribean_mapping, 4326)

# Specify the desired bounding box for Latin America & Caribbean
latin_america_caribbean_bbox <- c(xmin = -120, xmax = -35, ymin = -60, ymax = 40)

# Plot the map with a zoomed-in view in Latin America & Caribbean
ggplot() +
  geom_sf(data = latin_america_carribean_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_label(data = st_centroid(latin_america_carribean_mapping), aes(label = iso_a3), size = 2, color = "black", fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in Latin America & the Carribean",
    subtitle = "Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(latin_america_caribbean_bbox["xmin"], latin_america_caribbean_bbox["xmax"]),
    ylim = c(latin_america_caribbean_bbox["ymin"], latin_america_caribbean_bbox["ymax"]))

# Create a categorical variable for average seizures
latin_america_carribean_mapping$category <- cut(
  latin_america_carribean_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# group_by() to group the data by the levels of "mean_seizures_cat" and 
# then uses summarize to calculate the mean of "mean_seizures" within each group
print(seizures_summary <- latin_america_carribean_mapping %>%
        group_by(category) %>%
        summarize(avg_seizures = mean(mean_seizures, na.rm = TRUE)))

# Breaks
print(breaks <- seq(0, 373000, length.out = 5))
# 0, 93250, 186500, 279750, 373000

# Plot
lac_un_cocaine_plot <- ggplot() +
  geom_sf(data = latin_america_carribean_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(latin_america_carribean_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.3, point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = latin_america_carribean_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", 
    color = "#DC3545", alpha = 0.7, stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), name = "Mean Cocaine Seizures", breaks = c(0, 93250, 186500, 279750, 373000),
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (93,000 kg)", "Medium (187,000 kg)", "Medium-High (280,000 kg)", "High (373,000 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8), legend.position = c(0.23, 0.32),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Latin Amercia & the Caribbean (1996-2019)", width = 50),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 80),
    caption = "Source: United Nations Office on Drugs and Crime (UNODC)") +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(latin_america_caribbean_bbox["xmin"], latin_america_caribbean_bbox["xmax"]),
    ylim = c(latin_america_caribbean_bbox["ymin"], latin_america_caribbean_bbox["ymax"])) +
  guides(size = guide_legend(title = "Mean Cocaine Seizures")) 

# Print
lac_un_cocaine_plot

# Save the plot as a PDF
ggsave("lac_un_cocaine_plot.pdf", plot = lac_un_cocaine_plot, width = 8, height = 11)
ggsave("lac_un_cocaine_plot.png", plot = lac_un_cocaine_plot, width = 8, height = 11)

#
#
# [4] Middle East & North Africa
#
#

# Filter the data for Middle East & North Africa region
middle_east_north_africa_data <- grouped_data[grouped_data$region == "Middle East & North Africa", ]
unique(middle_east_north_africa_data$country)

# Merge
middle_east_north_africa_mapping <- merge(world, middle_east_north_africa_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(middle_east_north_africa_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(middle_east_north_africa_mapping) # Print the current CRS of the dataset
middle_east_north_africa_mapping <- st_set_crs(middle_east_north_africa_mapping, 4326)

# Sorting issues
st_is_valid(middle_east_north_africa_mapping)
middle_east_north_africa_mapping <- st_make_valid(middle_east_north_africa_mapping)

# Specify the desired bounding box for Middle East & North Africa
middle_east_north_africa_bbox <- c(xmin = -20, xmax = 60, ymin = 5, ymax = 40)

# Plot the map with a zoomed-in view in Middle East & North Africa
ggplot() +
  geom_sf(data = middle_east_north_africa_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_label(data = st_centroid(middle_east_north_africa_mapping), aes(label = iso_a3), size = 2, color = "black", fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in Middle East & North Africa",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(middle_east_north_africa_bbox["xmin"], middle_east_north_africa_bbox["xmax"]),
    ylim = c(middle_east_north_africa_bbox["ymin"], middle_east_north_africa_bbox["ymax"]))

# Create a categorical variable for average seizures
middle_east_north_africa_mapping$category <- cut(
  middle_east_north_africa_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Breaks
print(breaks <- seq(0, 100, length.out = 5))
# 0, 25, 50, 75, 100

# Plot
mena_un_cocaine_plot <- ggplot() +
  geom_sf(data = middle_east_north_africa_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(middle_east_north_africa_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.3, point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = middle_east_north_africa_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", 
    color = "#DC3545", alpha = 0.7, stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), name = "Mean Cocaine Seizures", breaks = breaks,
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (25 kg)", "Medium (50 kg)", "Medium-High (75 kg)", "High (100 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8), legend.position = c(0.10, 0.4),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
labs(
  title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Middle East & North Africa (1996-2019)", width = 60),
  subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 80),
  caption = "Source: United Nations Office on Drugs and Crime (UNODC)")) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(middle_east_north_africa_bbox["xmin"], middle_east_north_africa_bbox["xmax"]),
    ylim = c(middle_east_north_africa_bbox["ymin"], middle_east_north_africa_bbox["ymax"])) 

# Print
mena_un_cocaine_plot

# Save the plot as a PDF
ggsave("mena_un_cocaine_plot.pdf", plot = mena_un_cocaine_plot, width = 14, height = 8)
ggsave("mena_un_cocaine_plot.png", plot = mena_un_cocaine_plot, width = 14, height = 8)

#
#
# [5] North America
#
#

# Filter the data for the North American region
north_america_data <- grouped_data[grouped_data$region == "North America", ]
unique(north_america_data$country)

# Merge
north_america_mapping <- merge(world, north_america_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(north_america_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(north_america_mapping) # Print the current CRS of the dataset
north_america_mapping <- st_set_crs(north_america_mapping, 4326)

# Sorting issues
st_is_valid(north_america_mapping)
north_america_mapping <- st_make_valid(north_america_mapping)

# Specify the desired bounding box for North America
north_america_mapping_bbox <- c(xmin = -170, xmax = -50, ymin = 10, ymax = 85)

# Plot the map with a zoomed-in view in North America
ggplot() +
  geom_sf(data = north_america_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_text(data = north_america_mapping, aes(label = iso_a3), size = 2, color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  labs(title = "United Nations Convention Commitment and Seizures in North America",
       subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(north_america_mapping_bbox["xmin"], north_america_mapping_bbox["xmax"]),
    ylim = c(north_america_mapping_bbox["ymin"], north_america_mapping_bbox["ymax"]))


# Create a categorical variable for average seizures
north_america_mapping$category <- cut(
  north_america_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Breaks
print(breaks <- seq(0, 50000, length.out = 5))
# 0, 12500, 25000, 37500, 50000

# Plot
na_un_cocaine_plot <- ggplot() +
  geom_sf(data = north_america_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(north_america_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.5, point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to One", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = north_america_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", color = "#DC3545", 
    alpha = 0.7, stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), name = "Mean Cocaine Seizures", breaks = breaks,
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (12,500 kg)", "Medium (25,000 kg)", "Medium-High (37,500 kg)", "High (50,000 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8), legend.position = c(0.2, 0.45),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of North America (1996-2019)", width = 50),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 70),
     caption = "Source: United Nations Office on Drugs and Crime (UNODC)") +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(north_america_mapping_bbox["xmin"], north_america_mapping_bbox["xmax"]),
    ylim = c(north_america_mapping_bbox["ymin"], north_america_mapping_bbox["ymax"])) 

# print
na_un_cocaine_plot

# Save the plot as a PDF
ggsave("na_un_cocaine_plot.pdf", plot = na_un_cocaine_plot , width = 8, height = 9)
ggsave("na_un_cocaine_plot.png", plot = na_un_cocaine_plot , width = 8, height = 9)

#
#
# [6] South Asia
#
#

# Filter the data for South Asia region
south_asia_data <- grouped_data[grouped_data$region == "South Asia", ]
unique(south_asia_data$country)

# Merge
south_asia_mapping <- merge(world, south_asia_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(south_asia_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(south_asia_mapping) # Print the current CRS of the dataset
south_asia_mapping <- st_set_crs(south_asia_mapping, 4326)

# Sorting issues
st_is_valid(south_asia_mapping)
south_asia_mapping <- st_make_valid(south_asia_mapping)

# Specify the desired bounding box for South Asia
south_asia_mapping_bbox <- c(xmin = 60, xmax = 100, ymin = 5, ymax = 40)

# Plot the map with a zoomed-in view in South Asia
ggplot() +
  geom_sf(data = south_asia_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_text(data = south_asia_mapping, aes(label = iso_a3), size = 2, color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in South Asia",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(south_asia_mapping_bbox["xmin"], south_asia_mapping_bbox["xmax"]),
    ylim = c(south_asia_mapping_bbox["ymin"], south_asia_mapping_bbox["ymax"]))


# Create a categorical variable for average seizures
south_asia_mapping$category <- cut(
  south_asia_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Breaks
print(breaks <- seq(0, 85, length.out = 5))
# 0, 21, 43, 64, 85

# Plot
sa_un_cocaine_plot <- ggplot() +
  geom_sf(data = south_asia_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(south_asia_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.5, point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to One", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = south_asia_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", color = "#DC3545", 
    alpha = 0.7, stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), name = "Mean Cocaine Seizures", breaks = breaks, 
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (21 kg)", "Medium (43 kg)", "Medium-High (64 kg)", "High (85 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8), legend.position = c(0.195, 0.295), 
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of South Asia (1996-2019)", width = 70),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 70)
    caption = "Source: United Nations Office on Drugs and Crime (UNODC)") +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(south_asia_mapping_bbox["xmin"], south_asia_mapping_bbox["xmax"]), 
    ylim = c(south_asia_mapping_bbox["ymin"], south_asia_mapping_bbox["ymax"]))

# Save the plot as a PDF
ggsave("sa_un_cocaine_plot.png", plot = sa_un_cocaine_plot, width = 8, height = 8)
ggsave("sa_un_cocaine_plot.pdf", plot = sa_un_cocaine_plot, width = 8, height = 8)

sa_un_cocaine_plot

# Set up a layout with one row and two columns
library(patchwork)

# Combine the two maps side by side
latin_america_middle_east_north_africa_cocaine_maps <- lac_un_cocaine_plot + mena_un_cocaine_plot + plot_layout(ncol = 2)

# Print the combined plot
latin_america_middle_east_north_africa_cocaine_maps

# Save
ggsave("latin_america_middle_east_north_africa_cocaine_maps.pdf", plot = latin_america_middle_east_north_africa_cocaine_maps, 
       width = 21.75, height = 9)
ggsave("latin_america_middle_east_north_africa_cocaine_maps.png", plot = latin_america_middle_east_north_africa_cocaine_maps, 
       width = 21.75, height = 9)

#
#
# [7] Sub-Saharan Africa
#
#

# Filter the data for Sub-Saharan African region
sub_saharan_africa_data <- grouped_data[grouped_data$region == "Sub-Saharan Africa", ]
unique(sub_saharan_africa_data$country)

# Merge
sub_saharan_africa_mapping <- merge(world, sub_saharan_africa_data, by.x = "iso_a3", by.y = "code", all.x = FALSE)
str(sub_saharan_africa_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(sub_saharan_africa_mapping) # Print the current CRS of the dataset
sub_saharan_africa_mapping <- st_set_crs(sub_saharan_africa_mapping, 4326)

# Sorting issues
st_is_valid(sub_saharan_africa_mapping)
sub_saharan_africa_mapping <- st_make_valid(sub_saharan_africa_mapping)

# Specify the desired bounding box for Sub-Saharan Africa
sub_saharan_africa_mapping_bbox <- c(xmin = -30, xmax = 60, ymin = -35, ymax = 30)

# Plot the map with a zoomed-in view in Sub-Saharan Africa
ggplot() +
  geom_sf(data = sub_saharan_africa_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.2) +
  geom_sf_text(data = sub_saharan_africa_mapping, aes(label = iso_a3), size = 2, color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"), labels = c("Committed to None", "Committed to One"), name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in Sub-Saharan Africa",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(sub_saharan_africa_mapping_bbox["xmin"], sub_saharan_africa_mapping_bbox["xmax"]),
    ylim = c(sub_saharan_africa_mapping_bbox["ymin"], sub_saharan_africa_mapping_bbox["ymax"]))


# Create a categorical variable for average seizures
sub_saharan_africa_mapping$category <- cut(
  sub_saharan_africa_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# group_by() to group the data by the levels of "mean_seizures_cat" and 
# then uses summarize to calculate the mean of "mean_seizures" within each group
print(seizures_summary <- sub_saharan_africa_mapping %>%
        group_by(category) %>%
        summarize(avg_seizures = mean(mean_seizures, na.rm = TRUE)))

# Breaks
print(breaks <- seq(0, 120, length.out = 5))
# 0, 30, 60, 90, 120

# Plot
subsa_un_cocaine_plot <- ggplot() +
  geom_sf(data = sub_saharan_africa_mapping, aes(fill = factor(any_UN)), color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(sub_saharan_africa_mapping), aes(x = label_x, y = label_y, label = abbrev), size = 3.5, color = "black", 
    box.padding = 0.5, point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"), labels = c("Committed to One", "Committed to One"), name = "Status of Commitment") +
  geom_point(
    data = sub_saharan_africa_mapping, aes(x = label_x, y = label_y, size = mean_seizures), shape = 16, fill = "#B3446C", 
    color = "#DC3545", alpha = 0.7, stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), name = "Mean Cocaine Seizures", breaks = breaks, 
    labels = stringr::str_wrap(c("Low (0 kg)", "Low-Medium (30)", "Medium (60 kg)", "Medium-High (90)", "High (120 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16), axis.text = element_text(size = 8), legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8), legend.position = "right",
    legend.background = element_rect(color = "lightgray", linetype = "dashed"), 
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Sub-Saharan Africa (1996-2019)", width = 70),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 90)
     caption = "Source: United Nations Office on Drugs and Crime (UNODC)") +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(sub_saharan_africa_mapping_bbox["xmin"], sub_saharan_africa_mapping_bbox["xmax"]),
    ylim = c(sub_saharan_africa_mapping_bbox["ymin"], sub_saharan_africa_mapping_bbox["ymax"]))

# Print
subsa_un_cocaine_plot

# Save the plot as a PDF
ggsave("subsa_un_cocaine_plot.pdf", plot = subsa_un_cocaine_plot, width = 9.5, height = 10)
ggsave("subsa_un_cocaine_plot.png", plot = subsa_un_cocaine_plot, width = 9.5, height = 10)

# End

# Plots to merge 
sa_un_cocaine_plot # South Asia
eap_un_cocaine_plot # East Asia & Pacific

eca_un_cocaine_plot # Europe & Central Asia

na_un_cocaine_plot # North America
lac_un_cocaine_plot  # Latin America & Caribbean

mena_un_cocaine_plot # Middle East & North Africa
subsa_un_cocaine_plot # Sub-Saharan Africa

# Set up a layout with one row and two columns
library(patchwork)

#
# East, South Asia & the Pacific 
#

# Combine the two maps side by side
south_east_asia <- eap_un_cocaine_plot + sa_un_cocaine_plot + plot_layout(ncol = 2)

# Print the combined plot
south_east_asia

# Save
ggsave("south_east_asia_cocaine_map.pdf", plot = south_east_asia, width = 19, height = 8)
ggsave("south_east_asia_cocaine_map.png", plot = south_east_asia, width = 19, height = 8)

#
# North, Latin America and the Caribbean 
#

# Combine the two maps side by side
north_south_america_maps <-  lac_un_cocaine_plot + na_un_cocaine_plot + plot_layout(ncol = 2)

# Print the combined plot
north_south_america_maps

# Save
ggsave("north_south_america_maps.pdf", plot = north_south_america_maps, width = 14, height = 8)
ggsave("north_south_america_maps.png", plot = north_south_america_maps, width = 14, height = 8)

#
# Middle East & North, Sub-Saharan Africa
#

# Combine the two maps side by side
middle_east_north_sub_saharan_africa_maps <-  mena_un_cocaine_plot + subsa_un_cocaine_plot + plot_layout(ncol = 2)

# Print the combined plot
middle_east_north_sub_saharan_africa_maps

# Save
ggsave("middle_east_north_sub_saharan_africa_maps.pdf", plot = middle_east_north_sub_saharan_africa_maps, width = 20, height = 8)
ggsave("middle_east_north_sub_saharan_africa_maps.png", plot = middle_east_north_sub_saharan_africa_maps, width = 20, height = 8)

# End 
