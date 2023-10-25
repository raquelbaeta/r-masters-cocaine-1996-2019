# Start

# Thesis title: The Role of International Commitments in Combating the Illicit 
# Distribution of Cocaine.
# Author: Raquel Baeta

# Set the working directory
setwd("~/Desktop/working-sessions")

# Load necessary libraries
library(ggplot2)
library(ggpubr)
library(stargazer)

# ANOVA test: This test compares the means of a continuous response variable 
# across different levels of one or more categorical predictor variables.
anova_gdp_log_euro <- aov(seizure_log ~ retail + gdp_euro_log, data = complete_data)
summary(anova_gdp_log_euro)

# Create a scatter plot with GDP, retail prices and cocaine seizures
gdp_retail_seizure <- ggplot(
  complete_data, aes(x = gdp_euro_log, y = retail, color = seizure_log)) +
  geom_point() +
  labs(x = "Gross Domestic Product (in Euros)", 
       y = "Retail Price (in Euros)", 
       color = "Cocaine Seizures (in grams)",
       title = "Relationship between Gross Domestic Product (GDP), Retail Price and Cocaine Seizures in Europe between 1996 and 2019") + # add \n to break the title
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        legend.title = element_text("Cocaine Seizures"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradientn(colors=c("navy", "steelblue", "darkorange"))

# Then, use ggsave() to the plot to a file
ggsave("gdp_retail_seizure.png", plot = gdp_retail_seizure)

# ANOVA test: This test compares the means of a continuous response variable 
# across different levels of one or more categorical predictor variables.
gnicap_log <- aov(seizure_log ~ retail + gnicap_log, data = complete_data)
summary(gnicap_log)

# Create a scatter plot with GDP, retail prices and cocaine seizures
gni_retail_seizure <- ggplot(
  complete_data, aes(x = gnicap_log, y = retail, color = seizure_log)) +
  geom_point() +
  labs(x = "Gross National Income (in Euros)", 
       y = "Retail Price (in Euros)", 
       color = "Cocaine Seizures (in grams)",
       title = "Relationship between Gross National Income (GNI), Retail Price and Cocaine Seizures in Europe between 1996 and 2019") + # add \n to break the title
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        legend.title = element_text("Cocaine Seizures"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")) +
  scale_color_gradientn(colors=c("navy", "steelblue", "darkorange"))

# Then, use ggsave() to the plot to a file
ggsave("gni_retail_seizure.png", plot = gni_retail_seizure)

# ggarange() can arrange multiple ggplots 
gdp_gni_retail_seizure <- ggarrange(
  gdp_retail_seizure, gni_retail_seizure + 
    rremove("x.text"), labels = c("A", "B"), ncol = 1, nrow = 2)

# Save the plots to a file
ggsave("gdp_gni_retail_seizure.png", width = 11, height = 13, dpi = 500)

# End