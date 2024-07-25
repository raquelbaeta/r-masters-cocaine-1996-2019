# Governance Dynamics Analysis

This project explores governance dynamics across different regions, specifically South Asia and Sub-Saharan Africa, by analysing and visualising World Governance Indicators (WGI) data. 

The objective is to compare various governance indicators—such as Control of Corruption, Government Effectiveness, Regulatory Quality, Rule of Law, Voice and Accountability, and Political Stability—over time. 

The analysis particularly examines the impact of countries' adherence to United Nations conventions on these indicators. The project includes scripts for data preparation, analysis, and visualization, providing insights into regional governance trends and their alignment with international standards.

## Contents

- Data Preparation: Scripts for loading and preparing data.
- Analysis: Scripts for calculating and filtering governance indicators.
- Visualisation: Scripts for generating plots to visualise governance trends.

## Requirements

The following R packages are required to run the scripts:
install.packages(
  c("readxl", "readr", "tidyverse", "dplyr", "ggplot2", "scales", "ggrepel", "countrycode", "sf", "ggspatial", "rworldmap", "rworldxtra", "RColorBrewer", "maptools", "classInt",   
    "ggtext", "maps", "mapproj"))

Load Libraries
library(readxl, readr, tidyverse, dplyr, ggplot2, scales, ggrepel, countrycode, sf, ggspatial, rworldmap, rworldxtra, RColorBrewer, maptools, classInt, ggtext, maps, mapproj)

## Data Source

The analysis uses data from the World Bank’s World Development Indicators and United Nations data.

## Contributing

If you would like to contribute to this project, please follow these steps:

Fork the repository. Create a new branch (git checkout -b feature-branch). Make your changes. Commit your changes (git commit -m 'Add some feature'). Push to the branch (git push origin feature-branch). Create a new Pull Request.

If you have any questions or need further assistance, please open an issue or contact the repository maintainer.

## Contact

For any inquiries, please contact raquel@aside.co.za.
