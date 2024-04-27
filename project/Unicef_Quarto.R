---
  
title: "final_unicef_Quarto"
subtitle: "Insights into Global Progress: A Data-Driven Approach to UNICEF's Key Indicators"
author: "Dhanashri G"
date: "April 25, 2024"
html_document:
  theme: minty
toc: true
toc_float: true
format: html
---
  
  ```{r setup, include=FALSE}
#libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(plotly)
library(maps)
library(dplyr)
library(readr)
library(RColorBrewer)

#data
unicef_data <- read_csv("Unicef_Indicator.csv")
```
```{r, fig.cap="Global distribution of life expectancy at birth. This map visualizes disparities in life expectancy across countries, with varying shades representing the observed values. Countries in blue show higher life expectancy, while those in red have lower, indicating significant global health inequalities."}
world_map <- map_data("world")
world_data <- left_join(world_map, unicef_data, by = c("region" = "country"))

# Plotting the map with the corrected data column for fill
ggplot(world_data, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon() +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  labs(title = "Indicator Visualization: Life Expectancy at Birth",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
```


```{r,echo=TRUE, message=FALSE,fig.cap=="This scatter plot visualizes the relationship between Gross National Income (GNI) per capita and life expectancy at birth across different countries. Each point represents a country, with its position on the plot indicating its GNI and life expectancy. The plot allows us to observe any potential correlation or trend between these two variables, which can provide insights into the socioeconomic factors influencing life expectancy globally"}

#Plotting the scatterplot 

unicef_data$country <- as.character(unicef_data$country)

# Create the ggplot object
basic_plot <- ggplot(unicef_data, aes(x = `GNI (current US$)`, y = `Life expectancy at birth, total (years)`, 
                                      text = country, group = country, color = country)) +
  geom_point() +
  labs(title = "Scatter Plot of GNI vs. Life Expectancy",
       x = "GNI (current US$)", y = "Life Expectancy at Birth (years)") +
  theme_minimal() +
  theme(legend.position = "none")

# Converting the ggplot object to an interactive plotly object
interactive_plot <- ggplotly(basic_plot, tooltip = "text")

# Printing the interactive plot
print(interactive_plot)
```

