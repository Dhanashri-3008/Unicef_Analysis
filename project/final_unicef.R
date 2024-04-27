install.packages("tidyverse")
install.packages("ggplot2")
install.packages("maps")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("plotly")
install.packages("readr")
install.packages("scales")

library(tidyverse)
library(ggplot2)
library(maps)
library(dplyr)
library(plotly)
library(readr)

world_map <- map_data("world")

# Load your data from the CSV file
unicef_data <- read_csv("Unicef_Indicator.csv")


world_data <- left_join(world_map, unicef_data, by = c("region" = "country"))

library(ggplot2)
library(RColorBrewer)


# Plotting the map with the corrected data column for fill
ggplot(world_data, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon() +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  labs(title = "Indicator Visualization: Life Expectancy at Birth",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

#print(data_join)

library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
unicef_data <- read_csv("Unicef_Indicator.csv")  

# Selected countries for the scatter plot
selected_countries <- c('Sao Tome and Principe', 'Guinea-Bissau', 'Timor-Leste', 'Liberia', 'Maldives', 
                        'Mauritania', 'Rwanda', 'Niger', 'Mongolia', 'Madagascar', 'Mozambique', 
                        "Lao People's Democratic Republic", 'Honduras', 'Senegal', 'Nepal', 
                        'Congo, the Democratic Republic of the', 'Cameroon', 
                        'Tanzania, United Republic of', 'Burma', 'Kenya')

# Filter for the selected countries and remove NA values for GNI and Life Expectancy
unicef_data_selected <- unicef_data %>%
  filter(country %in% selected_countries) %>%
  drop_na(`GNI (current US$)`, `Life expectancy at birth, total (years)`)

# Scatter plot for GNI vs Life Expectancy for the selected countries
ggplot(unicef_data_selected, aes(x = `GNI (current US$)`, y = `Life expectancy at birth, total (years)`, color = country)) +
  geom_point() +
  scale_x_log10() +  # Log scale for the GNI axis
  labs(title = "Scatter Plot of GNI vs. Life Expectancy for Selected Countries",
       x = "GNI (current US$) (log scale)",
       y = "Life Expectancy at Birth (years)") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Country"))



#GDP per Capita Comparison of Top 10 Countries"

library(tidyverse)
library(ggplot2)

# Assuming gdp_evolution is already loaded
# Convert the time_period to a factor to ensure it's treated as a discrete variable
unicef_data$time_period <- factor(unicef_data$time_period)
print(unicef_data)
# First, find the top 10 countries with the highest average GDP per capita over the period from 2012 to 2018
top_10_countries <- unicef_data %>%
  group_by(country) %>%
  summarize(Average_GDP_per_capita = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE)) %>%
  top_n(10, Average_GDP_per_capita) %>%
  pull(country)

# Filter the gdp_evolution data for just these top 10 countries
gdp_evolution_top10 <- unicef_data %>%
  filter(country %in% top_10_countries)


# Create the bar chart for just the top 10 countries
gdp_bar_chart_top10 <- ggplot(gdp_evolution_top10, aes(x = time_period, y = `GDP per capita (constant 2015 US$)`, fill = country)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = rainbow(10)) +
  labs(
    x = "Year",
    y = "Average GDP per Capita (constant 2015 US$)",
    title = "Evolution of Average GDP per Capita from 2012 to 2018 for Top 10 Countries",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
print(gdp_bar_chart_top10)


#Print Time series plot

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Load data from the CSV file
unicef_data <- read_csv("unicef_metadata.csv")

# Filter data for China and Japan, and ensure the Population column is numeric
china_japan_data <- unicef_data %>%
  filter(country %in% c('China', 'Japan')) %>%
  mutate(Population_total = as.numeric(`Population, total`)) %>%
  drop_na(Population_total)

# Plotting the time series for China and Japan
population_ggplot <- china_japan_data %>%
  ggplot(aes(x = year, y = Population_total, group = country, color = country)) +
  geom_line() +
  scale_y_continuous(labels = function(x) paste0(x / 1e6, "M")) +
  labs(x = 'Year', y = 'Total Population (Million)',
       title = 'Time Series Plot of Total Population of China and Japan') +
  theme_light()

# Print the plot
print(population_ggplot)
