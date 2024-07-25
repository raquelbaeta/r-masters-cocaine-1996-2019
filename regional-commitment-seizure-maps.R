# Start 

# title: "Commitment to United Nations Conventions 1961, 1971, 1988 and Cocaine Seizures"
# author: "Raquel Baeta"
# date: "2024-07-25"

# Install packages
install.packages(c("readr", "countrycode", "tidyverse", "dplyr", "ggplot2", "sf", "ggspatial", "rworldmap", "rworldxtra",
                   "RColorBrewer", "maptools", "classInt", "ggtext"))
install.packages("https://cran.rstudio.com/bin/macosx/big-sur-x86_64/contrib/4.3/rworldmap_1.3-8.tgz", 
                 repos = NULL, type = "source")

## Load packages
library(c(sf, rnaturalearth, ggrepel, stringr, readr, countrycode, tidyverse, dplyr, ggplot2, sf, ggspatial))

# Define colors for each region
colors <- c("#FFC107", "#38A3A5", "#B3446C", "#DC3545", "#007BFF", "#F08030", "#C7B8E6")

# Set working directory and load data
setwd("~/Desktop/working-sessions")
data <- read_csv("~/Desktop/working-sessions/cleaning_data/cleaned_data.csv")

# Group data by region and summarize seizures
grouped_data <- data %>%
  group_by(region, code, country, any_UN, UN1961, UN1971, UN1988) %>%
  dplyr::summarize(mean_seizures = mean(seizures, na.rm = TRUE))

# Filter data for specific regions
east_asia_data <- grouped_data[grouped_data$region == "East Asia & Pacific", ]
world <- ne_countries(scale = "large", returnclass = "sf")

#
#
# [1] East Asia & the Pacific
#
#

# Merge data with world map
east_asia_mapping <- merge(
  world, 
  east_asia_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)

st_crs(east_asia_mapping) # Check CRS
east_asia_mapping <- st_set_crs(east_asia_mapping, 4326)

# Define bounding box for plotting
east_asia_bbox <- c(xmin = 10, xmax = 180, ymin = -45, ymax = 50)

# Plot map
ggplot() +
  geom_sf(
    data = east_asia_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.2) +
  geom_sf_label(
    data = st_centroid(east_asia_mapping), 
    aes(label = iso_a3), 
    size = 2, 
    color = "black", 
    fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in East Asia & the Pacific",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(east_asia_bbox["xmin"], east_asia_bbox["xmax"]),
    ylim = c(east_asia_bbox["ymin"], east_asia_bbox["ymax"]))

# Create categorical variable for seizures and plot
east_asia_mapping$category <- cut(east_asia_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

eap_un_cocaine_plot <- ggplot() +
  geom_sf(
    data = east_asia_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.3) +
  geom_text_repel(
    data = st_centroid(east_asia_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.5, 
    parse = TRUE) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"), 
    name = "Status of Commitment") +
  geom_point(
    data = east_asia_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures),
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.6, 
    stroke = 0.4) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Cocaine Seizures",
    breaks = c(0, 175, 350, 525, 700),
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (175 kg)", "Medium (350 kg)", "Medium-High (525 kg)", "High (700 kg)"), width = 50)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16), 
        axis.text = element_text(size = 8), 
        panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
        legend.title = element_text(size = 10), legend.text = element_text(size = 8),
        legend.position = c(0.195, 0.48), 
        legend.background = element_rect(color = "gray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of East Asia & the Pacific (1996-2019)", width = 70), 
       subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 90)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(east_asia_bbox["xmin"], east_asia_bbox["xmax"]),
    ylim = c(east_asia_bbox["ymin"], east_asia_bbox["ymax"])) +
  guides(size = guide_legend(title = "Mean Cocaine Seizures")) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

# Save plots
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

# Merge data for Europe & Central Asia
europe_central_asia_mapping <- merge(
  world, 
  europe_asia_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)
str(europe_central_asia_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(europe_central_asia_mapping) # Print the current CRS of the dataset
europe_central_asia_mapping <- st_set_crs(europe_central_asia_mapping, 4326)

# Specify the desired bounding box for Europe and Central Asia
europe_central_asia_bbox <- c(xmin = -10, xmax = 175, ymin = 35, ymax = 80)

# Plot the map with a zoomed-in view of Europe and Central Asia
ggplot() +
  geom_sf(
    data = europe_central_asia_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.2) +
  geom_sf_label(
    data = st_centroid(europe_central_asia_mapping), 
    aes(label = iso_a3), 
    size = 2, 
    color = "black", 
    fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(title = "United Nations Convention Commitment and Seizures in Europe & Central Asia",
       subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(europe_central_asia_bbox["xmin"], europe_central_asia_bbox["xmax"]),
    ylim = c(europe_central_asia_bbox["ymin"], europe_central_asia_bbox["ymax"]))

# Create a categorical variable for average seizures
europe_central_asia_mapping$category <- cut(
  europe_central_asia_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Define breaks for size scale
breaks <- seq(0, 13000, length.out = 5)

# Plot seizures in Europe & Central Asia
eca_un_cocaine_plot <- ggplot() +
  geom_sf(
    data = europe_central_asia_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.3) +
  geom_text_repel(
    data = st_centroid(europe_central_asia_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  geom_point(
    data = europe_central_asia_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures), 
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.6, stroke = 0.4) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Cocaine Seizures",
    breaks = c(0, 2500, 5000, 7500, 10000),
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (2500 kg)", "Medium (5000 kg)", "Medium-High (7500 kg)", "High (10000 kg)"), 
      width = 50)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 8),  # Adjust font size of axis text
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.position = c(0.195, 0.48),
    legend.background = element_rect(color = "gray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Europe & Central Asia (1996-2019)", width = 70), 
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 90)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(europe_central_asia_bbox["xmin"], europe_central_asia_bbox["xmax"]),
    ylim = c(europe_central_asia_bbox["ymin"], europe_central_asia_bbox["ymax"])) +
  guides(size = guide_legend(title = "Mean Cocaine Seizures")) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

# Save the plot as a PDF/PNG
ggsave("eca_un_cocaine_plot.pdf", plot = eca_un_cocaine_plot, width = 14, height = 8.5)
ggsave("eca_un_cocaine_plot.png", plot = eca_un_cocaine_plot, width = 14, height = 8.5)

#
#
# [3] Latin America & Caribbean
#
#

# Filter the data for Latin America & Caribbean region
latam_caribbean_data <- grouped_data[grouped_data$region == "Latin America & Caribbean", ]
unique(latam_caribbean_data$country)

# Merge data for Latin America & Caribbean
latam_caribbean_mapping <- merge(
  world, 
  latam_caribbean_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)
str(latam_caribbean_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(latam_caribbean_mapping) # Print the current CRS of the dataset
latam_caribbean_mapping <- st_set_crs(latam_caribbean_mapping, 4326)

# Specify the desired bounding box for Latin America and the Caribbean
latam_caribbean_bbox <- c(xmin = -120, xmax = -30, ymin = -60, ymax = 20)

# Plot the map with a zoomed-in view of Latin America and the Caribbean
ggplot() +
  geom_sf(
    data = latam_caribbean_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.2) +
  geom_sf_label(
    data = st_centroid(latam_caribbean_mapping), 
    aes(label = iso_a3), 
    size = 2, 
    color = "black", 
    fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in Latin America & the Caribbean",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(latam_caribbean_bbox["xmin"], latam_caribbean_bbox["xmax"]),
    ylim = c(latam_caribbean_bbox["ymin"], latam_caribbean_bbox["ymax"]))

# Create a categorical variable for average seizures
latam_caribbean_mapping$category <- cut(
  latam_caribbean_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Define breaks for size scale
breaks <- seq(0, 80000, length.out = 5)

# Plot seizures in Latin America & the Caribbean
latam_un_cocaine_plot <- ggplot() +
  geom_sf(
    data = latam_caribbean_mapping,
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.3) +
  geom_text_repel(
    data = st_centroid(latam_caribbean_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.5) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  geom_point(
    data = latam_caribbean_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures), 
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.6, 
    stroke = 0.4) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Cocaine Seizures",
    breaks = c(0, 10000, 20000, 30000, 40000),
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (10000 kg)", "Medium (20000 kg)", "Medium-High (30000 kg)", "High (40000 kg)"), 
      width = 50)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 8),  # Adjust font size of axis text
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.position = c(0.195, 0.48),
    legend.background = element_rect(color = "gray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Latin America & the Caribbean (1996-2019)", width = 70), 
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 90)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(latam_caribbean_bbox["xmin"], latam_caribbean_bbox["xmax"]),
    ylim = c(latam_caribbean_bbox["ymin"], latam_caribbean_bbox["ymax"])) +
  guides(size = guide_legend(title = "Mean Cocaine Seizures")) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

# Save the plot as a PDF/PNG
ggsave("latam_un_cocaine_plot.pdf", plot = latam_un_cocaine_plot, width = 14, height = 8.5)
ggsave("latam_un_cocaine_plot.png", plot = latam_un_cocaine_plot, width = 14, height = 8.5)

#
#
# [4] Middle East & North Africa
#
#

# Filter the data for Middle East & North Africa region
middle_east_north_africa_data <- grouped_data[grouped_data$region == "Middle East & North Africa", ]
unique(middle_east_north_africa_data$country)

# Merge
middle_east_north_africa_mapping <- merge(
  world, 
  middle_east_north_africa_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)
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
  geom_sf(
    data = middle_east_north_africa_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.2) +
  geom_sf_label(
    data = st_centroid(middle_east_north_africa_mapping), 
    aes(label = iso_a3),
    size = 2, 
    color = "black", 
    fill = NA) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
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

# group_by() to group the data by the levels of "mean_seizures_cat" and 
# then uses summarize to calculate the mean of "mean_seizures" within each group
print(seizures_summary <- middle_east_north_africa_mapping %>%
        group_by(category) %>%
        dplyr::summarize(avg_seizures = mean(mean_seizures, na.rm = TRUE)))

# Breaks
print(breaks <- seq(0, 100, length.out = 5))
# 0, 25, 50, 75, 100

# Plot
mena_un_cocaine_plot <- ggplot() +
  geom_sf(
    data = middle_east_north_africa_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.3) +
  geom_text_repel(
    data = st_centroid(middle_east_north_africa_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  geom_point(
    data = middle_east_north_africa_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures), 
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.7, 
    stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Mean Cocaine Seizures",
    breaks = breaks,
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (25 kg)", "Medium (50 kg)", "Medium-High (75 kg)", "High (100 kg)"), width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.position = c(0.10, 0.4),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Middle East & North Africa (1996-2019)", width = 60),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 80)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(middle_east_north_africa_bbox["xmin"], middle_east_north_africa_bbox["xmax"]),
    ylim = c(middle_east_north_africa_bbox["ymin"], middle_east_north_africa_bbox["ymax"])) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

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

# Filter the data for North America region
north_america_data <- grouped_data[grouped_data$region == "North America", ]
unique(north_america_data$country)

# Merge
north_america_mapping <- merge(
  world, 
  north_america_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)
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
  geom_sf(
    data = north_america_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white",
    size = 0.2) +
  geom_sf_text(
    data = north_america_mapping, 
    aes(label = iso_a3),
    size = 2, 
    color = "black", 
    check_overlap = TRUE) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in North America",
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
  geom_sf(
    data = north_america_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.3) +
  geom_text_repel(
    data = st_centroid(north_america_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  geom_point(
    data = north_america_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures), 
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.7, 
    stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Mean Cocaine Seizures",
    breaks = breaks,
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (12500 kg)", "Medium (25000 kg)", "Medium-High (37500 kg)", "High (50000 kg)"), 
      width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.position = c(0.10, 0.4),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of North America (1996-2019)", width = 60),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 80)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(north_america_mapping_bbox["xmin"], north_america_mapping_bbox["xmax"]),
    ylim = c(north_america_mapping_bbox["ymin"], north_america_mapping_bbox["ymax"])) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

# Print
na_un_cocaine_plot

# Save the plot as a PDF
ggsave("na_un_cocaine_plot.pdf", plot = na_un_cocaine_plot, width = 14, height = 8)
ggsave("na_un_cocaine_plot.png", plot = na_un_cocaine_plot, width = 14, height = 8)

#
#
# [6] South Asia
#
#

# Filter the data for South America region
south_america_data <- grouped_data[grouped_data$region == "South America", ]
unique(south_america_data$country)

# Merge
south_america_mapping <- merge(
  world, 
  south_america_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)
str(south_america_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(south_america_mapping) # Print the current CRS of the dataset
south_america_mapping <- st_set_crs(south_america_mapping, 4326)

# Sorting issues
st_is_valid(south_america_mapping)
south_america_mapping <- st_make_valid(south_america_mapping)

# Specify the desired bounding box for South America
south_america_bbox <- c(xmin = -80, xmax = -35, ymin = -60, ymax = 15)

# Plot the map with a zoomed-in view in South America
ggplot() +
  geom_sf(
    data = south_america_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.2) +
  geom_sf_text(
    data = south_america_mapping,
    aes(label = iso_a3),
    size = 2, 
    color = "black", 
    check_overlap = TRUE) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in South America",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(south_america_bbox["xmin"], south_america_bbox["xmax"]),
    ylim = c(south_america_bbox["ymin"], south_america_bbox["ymax"]))

# Create a categorical variable for average seizures
south_america_mapping$category <- cut(
  south_america_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Breaks
print(breaks <- seq(0, 20000, length.out = 5))
# 0, 5000, 10000, 15000, 20000

# Plot
sa_un_cocaine_plot <- ggplot() +
  geom_sf(
    data = south_america_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", size = 0.3) +
  geom_text_repel(
    data = st_centroid(south_america_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  geom_point(
    data = south_america_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures), 
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.7, 
    stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Mean Cocaine Seizures",
    breaks = breaks,
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (5000 kg)", "Medium (10000 kg)", "Medium-High (15000 kg)", "High (20000 kg)"), 
      width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.position = c(0.10, 0.4),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(
    title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of South America (1996-2019)", width = 60),
    subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 80)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(south_america_bbox["xmin"], south_america_bbox["xmax"]),
    ylim = c(south_america_bbox["ymin"], south_america_bbox["ymax"])) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

# Print
sa_un_cocaine_plot

# Save the plot as a PDF
ggsave("sa_un_cocaine_plot.pdf", plot = sa_un_cocaine_plot, width = 14, height = 8)
ggsave("sa_un_cocaine_plot.png", plot = sa_un_cocaine_plot, width = 14, height = 8)


#
#
# [7] Sub-Saharan Africa
#
#

# Filter the data for Sub-Saharan Africa region
sub_saharan_africa_data <- grouped_data[grouped_data$region == "Sub-Saharan Africa", ]
unique(sub_saharan_africa_data$country)

# Merge
sub_saharan_africa_mapping <- merge(
  world, 
  sub_saharan_africa_data, 
  by.x = "iso_a3", 
  by.y = "code", 
  all.x = FALSE)
str(sub_saharan_africa_mapping)

# Set the coordinate reference system (CRS) to WGS 84
st_crs(sub_saharan_africa_mapping) # Print the current CRS of the dataset
sub_saharan_africa_mapping <- st_set_crs(sub_saharan_africa_mapping, 4326)

# Sorting issues
st_is_valid(sub_saharan_africa_mapping)
sub_saharan_africa_mapping <- st_make_valid(sub_saharan_africa_mapping)

# Specify the desired bounding box for Sub-Saharan Africa
sub_saharan_africa_bbox <- c(xmin = -20, xmax = 55, ymin = -40, ymax = 20)

# Plot the map with a zoomed-in view in Sub-Saharan Africa
ggplot() +
  geom_sf(
    data = sub_saharan_africa_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", size = 0.2) +
  geom_sf_text(
    data = sub_saharan_africa_mapping, aes(label = iso_a3),
    size = 2, 
    color = "black", 
    check_overlap = TRUE) +
  scale_fill_manual(
    values = c("0" = "#c4d1cb", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  labs(
    title = "United Nations Convention Commitment and Seizures in Sub-Saharan Africa",
    subtitle = "Shading based on commitment to United Nations 1961, 1971 and 1988 Conventions") +
  theme_minimal() +
  coord_sf(
    xlim = c(sub_saharan_africa_bbox["xmin"], sub_saharan_africa_bbox["xmax"]),
    ylim = c(sub_saharan_africa_bbox["ymin"], sub_saharan_africa_bbox["ymax"]))

# Create a categorical variable for average seizures
sub_saharan_africa_mapping$category <- cut(
  sub_saharan_africa_mapping$mean_seizures, 5, labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

# Breaks
print(breaks <- seq(0, 15000, length.out = 5))
# 0, 3000, 6000, 9000, 12000, 15000

# Plot
ssa_un_cocaine_plot <- ggplot() +
  geom_sf(
    data = sub_saharan_africa_mapping, 
    aes(fill = factor(any_UN)), 
    color = "white", 
    size = 0.3) +
  geom_text_repel(
    data = st_centroid(sub_saharan_africa_mapping), 
    aes(x = label_x, y = label_y, label = abbrev),
    size = 3.5, 
    color = "black", 
    box.padding = 0.3, 
    point.padding = 0.6) +
  scale_fill_manual(
    values = c("0" = "#C5E1A5", "1" = "#79ae97"),
    labels = c("Committed to None", "Committed to One"),
    name = "Status of Commitment") +
  geom_point(
    data = sub_saharan_africa_mapping, 
    aes(x = label_x, y = label_y, size = mean_seizures), 
    shape = 16, 
    fill = "#B3446C", 
    color = "#DC3545", 
    alpha = 0.7, 
    stroke = 0.2) +
  scale_size_continuous(
    range = c(2, 12), 
    name = "Mean Cocaine Seizures",
    breaks = breaks,
    labels = stringr::str_wrap(
      c("Low (0 kg)", "Low-Medium (3000 kg)", "Medium (6000 kg)", "Medium-High (9000 kg)", "High (12000 kg)"), 
      width = 30)) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8),
    legend.position = c(0.10, 0.4),
    legend.background = element_rect(color = "lightgray", linetype = "dashed"),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")) +
  labs(title = stringr::str_wrap("Mean Annual Cocaine Seizures and United Nations Convention Commitments: A Map of Sub-Saharan Africa (1996-2019)", width = 60),
       subtitle = stringr::str_wrap("Exploring the spatial patterns of cocaine seizures and commitment to United Nations 1961, 1971 and 1988 Conventions", width = 80)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  coord_sf(
    xlim = c(sub_saharan_africa_bbox["xmin"], sub_saharan_africa_bbox["xmax"]),
    ylim = c(sub_saharan_africa_bbox["ymin"], sub_saharan_africa_bbox["ymax"])) +
  labs(caption = "Source: Baeta, using data from United Nations Office on Drugs and Crime (2024)")

# Print
ssa_un_cocaine_plot

# Save the plot as a PDF
ggsave("ssa_un_cocaine_plot.pdf", plot = ssa_un_cocaine_plot, width = 14, height = 8)
ggsave("ssa_un_cocaine_plot.png", plot = ssa_un_cocaine_plot, width = 14, height = 8)

# End 
