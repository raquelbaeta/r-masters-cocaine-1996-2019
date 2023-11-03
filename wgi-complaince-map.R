# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load required libraries
library(dplyr) # manipulation
library(rworldmap) # mapping 
library(countrycode) # iso3 country codes
library(ggplot2) # plotting
library(wesanderson) # colour palette
library(scales) # scales for gradients

# Remove all rows with "France" or "Italy" in the country column
complete_data <- complete_data %>%
  filter(!(country %in% c("France", "Italy")))

# Save the new data to a CSV file
write.csv(complete_data, "complete_data.csv")

# Get the world map
worldMap <- getMap()

# States involved in the study
europeanUnion <- c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Greece", "Ireland", "Luxembourg", "Netherlands", "Norway",
                   "Portugal", "Spain", "Sweden", "Switzerland")

# Convert the country names to ISO2 codes
europeanUnionISO3 <- countrycode(europeanUnion, "country.name", "iso3c")

# Select only the index of states involved in the study
indEU <- which(worldMap$ISO3%in%europeanUnionISO3)

# Extract the longitude and latitude border coordinates of member states of E.U. 
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$country =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

# Assuming your data frame is named europeCoords
europeCoords$code <- countrycode(europeCoords$region, "country.name", "iso3c")

# Create a ggplot object with the coordinates
p <- ggplot(data = europeCoords, aes(x = long, y = lat, group = region))

# Add polygons with a fill color and a white border
p <- p + geom_polygon(fill = "steelblue", color = "white")

# Set the coordinate limits to zoom in on Europe
p <- p + coord_cartesian(xlim = c(-25, 40), ylim = c(35, 70))

# Remove the axis labels and grid lines
p <- p + theme_void()

# Add country codes as labels using geom_text
country_centroids <- aggregate(cbind(long, lat) ~ region, data = europeCoords, FUN = function(x) mean(range(x)))
p <- p + geom_text(data = country_centroids, aes(label = region, x = long, y = lat), size = 3, fontface = "bold")

# Display the plot
print(p)

# Get a Wes Anderson palette
wes_palette <- rev(wesanderson::wes_palette("GrandBudapest2"))

# Fill the graph 
wgi_map_data <- complete_data

# Calculate the wgi_score for each country
wgi_map_data$wgi_score <- rowMeans(wgi_map_data[, c("va_est", "pv_est", "ge_est", "rq_est", "rl_est", "cc_est")])

# Delete the 'region' column and rename the 'country' column to 'region'
wgi_map_data <- wgi_map_data %>%
  select(-region) %>%
  rename(region = country)

# Check for missing values in region column in wgi_map_data 
missing_regions <- sum(is.na(wgi_map_data$region))

# If there are missing regions, clean or preprocess your data to address this issue
# if (missing_regions > 0) {
  # You can fill in missing regions or apply appropriate data-cleaning steps
  # For example, if your region names are present in other columns, you can use those
# }

# Omit any na's 
wgi_map_data <- na.omit(wgi_map_data)

# Merge the europeCoords data frame with europeCoords
europeCoords <- left_join(europeCoords, wgi_map_data, by = c("region" = "region", "code" = "code"))

# Plot a basic graph
p <- ggplot(data = europeCoords, aes(x = long, y = lat, group = region))

# Add polygons with a fill color based on wgi_score and a white border
p <- p + geom_polygon(aes(fill = wgi_score), color = "white")

# Add a gradient scale for wgi_score
p <- p + scale_fill_gradientn(colors = wes_palette, 
                              values = rescale(c(min(europeCoords$wgi_score, na.rm = TRUE),
                                                 median(europeCoords$wgi_score, na.rm = TRUE), 
                                                 max(europeCoords$wgi_score, na.rm = TRUE))),
                              name = "WGI Avg. Score")  # Rename the legend

# Set the coordinate limits to zoom in on Europe
p <- p + coord_cartesian(xlim = c(-25, 40), ylim = c(35, 70))

# Remove the axis labels and grid lines
p <- p + theme_void()

# Add country codes as labels using geom_text
country_centroids <- aggregate(cbind(long, lat) ~ region, data = europeCoords, FUN = function(x) mean(range(x)))
p <- p + geom_text(data = country_centroids, aes(label = region, x = long, y = lat), size = 2.5, fontface = "bold")

# Add a title
p <- p + labs(title = "An Overview of Governance, measured using World Governance Indicators (WGI)",
              subtitle = "The average World Governance Indicator (WGI) score of each European state from 1996 and 2018")

# View graph
print(p)

# Save the plots to a file
ggsave("governance_avg.png", plot = p, width = 7, height = 6, dpi = 600)

# Statistical Testing 
subset_data <- europeCoords %>%
  select(region, un1971, seizure_log, va_est, pv_est, ge_est, rq_est, rl_est, cc_est, wgi_score)

indicators <- c("seizure_log", "va_est", "pv_est", "ge_est", "rq_est", "rl_est", "cc_est", "wgi_score")

for (indicator in indicators) {
  cat("T-test for", indicator, "\n")
  
  t_test_results <- t.test(subset_data[[indicator]] ~ subset_data$un1971)
  
  cat("Group 0 (Not signed):\n")
  cat("  Mean:", mean(subset_data[subset_data$un1971 == 0, indicator]), "\n")
  cat("Group 1 (Signed):\n")
  cat("  Mean:", mean(subset_data[subset_data$un1971 == 1, indicator]), "\n")
  print(t_test_results)
  cat("\n")
}

# End
