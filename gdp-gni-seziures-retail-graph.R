# Start

# Thesis title: The Role of International Commitments in Combating the Illicit Distribution of Cocaine.
# Author: Raquel Baeta

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(stargazer)
library(wesanderson)


# Get a Wes Anderson palette
wes_palette <- rev(wes_palette("GrandBudapest2"))

# Create a scatter plot with GDP, retail prices and cocaine seizures
gdp_retail_seizure <- ggplot(
  complete_data, aes(x = gdp_euro_log, y = retail, color = seizure_log)) +
  geom_point(size = 2.2, alpha = 0.5) +  # Adjust the alpha level here
  labs(x = "Gross Domestic Product (GDP) per capita (in Euros)", 
       y = "Retail Price (in Euros)", 
       color = "Seizures (in grams)",
       title = "A scatter plot: The relationship between Cocaine Seizures and Economic Indicators between 1996 and 2019",
       subtitle = "Gross Domestic Product (GDP in euros), Retail Price (in euros) and Cocaine Seizures (in grams) reported by European States") + # add \n to break the title
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        legend.title = element_text("Seizures (grams)"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradientn(colors=wes_palette) # Apply the Wes Anderson palette

gdp_retail_seizure # check

# Statistical Testing
gdp_anov <- aov(seizure_log ~ retail * gdp_euro_log, data = complete_data)
summary(gdp_anov)

# Then, use ggsave() to the plot to a file
ggsave("gdp_retail_seizure.png",  width = 9.5, height = 6, dpi = 500, plot = gdp_retail_seizure)

# Create a scatter plot with GDP, retail prices and cocaine seizures
gni_retail_seizure <- ggplot(
  complete_data, aes(x = gnicap_log, y = retail, color = seizure_log)) +
  geom_point(size = 2.2, alpha = 0.5) +  # Adjust the alpha level here
  labs(x = "Gross National Income (in Euros)", 
       y = "Retail Price (in Euros)", 
       color = "Seizures (in grams)",
       title = "A scatter plot: The relationship between Cocaine Seizures and Economic Indicators between 1996 and 2019",
       subtitle = "Gross National Income (GNI in euros), Retail Price (in euros) and Cocaine Seizures (in grams) reported by European States") + # add \n to break the title
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        legend.title = element_text("Seizures (grams)"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradientn(colors=wes_palette) # Apply the Wes Anderson palette

gni_retail_seizure # check

gni_anov <- aov(seizure_log ~ retail * gnicap_log, data = complete_data)
summary(gni_anov)

# Then, use ggsave() to the plot to a file
ggsave("gni_retail_seizure.png",  width = 9.5, height = 6, dpi = 500, plot = gni_retail_seizure)

# ANOVA test: This test compares the means of a continuous response variable 
# across different levels of one or more categorical predictor variables.
multi_way <- aov(seizure_log ~ retail * gdp_euro_log * gnicap_log * un1961 *  un1971 * un1988, data = complete_data)
summary(multi_way)

# ggarange() can arrange multiple ggplots 
gdp_gni_retail_seizure <- ggarrange(
  gdp_retail_seizure, gni_retail_seizure + 
    rremove("x.text"), labels = c("A", "B"), ncol = 1, nrow = 2)

# Save the plots to a file
ggsave("gdp_gni_retail_seizure.png", width = 10, height = 13, dpi = 500)

# End
