library(tidyverse)
install.packages("plotly")
install.packages("countrycode")
library(plotly)
library(countrycode)
library(dplyr)
library(readr)
unicef_indicator_1_5_ <- read_csv("C:/Users/rissu/Downloads/unicef_indicator_1 (5).csv")
unicef_metadata_5_ <- read_csv("C:/Users/rissu/Downloads/unicef_metadata (5).csv")
Data_join <- full_join(unicef_indicator_1_5_, unicef_metadata_5_, by = join_by(country==country, time_period==year))
install.packages("maps")
library(maps)
map_world <- map_data("world")
map_data_join <- full_join(Data_join, map_world, by = c("country" = "region"))
install.packages("ggplot2")
library(ggplot2)

 #map 1 Life expectancy
install.packages("viridis")
library(ggplot2)
library(viridis)
ggplot(map_data_join, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "black") +  
  scale_fill_viridis(name = "Life Expectancy", option = "plasma", na.value = "gray50", guide = "legend") + 
  labs(title = "World Map: Life Expectancy", x = NULL, y = NULL) + 
  theme_minimal()


 #map 2 life expectancy in 1960
data_join_1960 <- Data_join %>%
  filter(time_period == "1960")
map_data_join_1960 <- full_join(data_join_1960, map_world, by = c("country" = "region"))
ggplot(map_data_join_1960, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "black") +  # Add black borders to polygons
  scale_fill_gradient(name = "Life Expectancy", low = "lightblue", high = "darkblue") +  # Use gradient fill colors
  labs(title = "World Map: Life Expectancy in 1960", x = NULL, y = NULL) +  # Add title and remove axis labels
  theme_minimal()



 #time series
timeseries_plot_1 <- Data_join %>%
  ggplot() +
  aes(x = time_period, y = `Life expectancy at birth, total (years)`, color = country) +
  geom_line() +
  labs(title = "**Time Series of Life Expectancy by Country**",
       x = "Time Period",
       y = "Life Expectancy at Birth (years)") +
  scale_color_viridis_d() +  # Using Viridis color palette for countries
  theme_minimal()  # Use a minimal theme for better readability

# Convert country names to continent names
Data_join$continent <- countrycode(Data_join$country, "country.name", "continent")

# Revised time series plot with continents
timeseries_plot_2 <- Data_join %>%
  ggplot() +
  aes(x = time_period, y = `Life expectancy at birth, total (years)`, color = continent) +
  geom_line() +
  labs(title = "**Time Series of Life Expectancy by Continent**",
       x = "Time Period",
       y = "Life Expectancy at Birth (years)") +
  scale_color_brewer(palette = "Set1") +  # Using Set1 color palette for continents
  theme_minimal()  # Use a minimal theme for better readability

# Print the plots
print(timeseries_plot_2)

ggplotly(timeseries_plot_2)


  


 #scatter plot with regression line

library(ggplot2)
library(plotly)

# Create the scatter plot
Scatter_plot_1 <- ggplot(Data_join, aes(x = obs_value, y = `Life expectancy at birth, total (years)`, size = `Population, total`)) +
  geom_point(alpha = 0.6, color = "darkblue") +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Red regression line
  labs(
    x = "Observed Value",
    y = "Life Expectancy (Years)",
    title = "Life Expectancy vs. Observed Value",
    size = "Population"
  ) + 
  scale_x_continuous(labels = scales::comma) +  # Format x-axis labels with commas
  scale_size_continuous(range = c(2, 10)) +  # Adjust size range for better visibility
  theme_minimal() +  # Use minimal theme for simplicity
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.title = element_text(size = 10),  # Adjust axis title size
    legend.position = "none"  # Remove legend
  )

# Convert ggplot to plotly object
ggplotly(Scatter_plot_1)




  
  # BAR CHART 1960-2021
  library(ggplot2)
  library(dplyr)
  library(countrycode)
  
  # Create a function to assign continents based on countries
  assign_continent <- function(country) {
    # Use countrycode package to get continents from countries
    continent <- countrycode(country, "country.name", "continent")
    return(continent)
  }
  
  # Apply the function to create a new column 'continent'
  Data_join <- Data_join %>%
    mutate(continent = assign_continent(country))
  
  # Filter data for years 1980, 2000, 2010, and 2020
  Data_filtered <- Data_join %>% 
    filter(time_period %in% c(1980, 2000, 2010, 2020))
  
  # Group by continent and time_period, then calculate average life expectancy
  Data_filtered %>%
    group_by(continent, time_period) %>%
    summarise(avg_life_exp = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE), .groups = "drop") %>%
    
    # Plotting the data as a bar chart
    ggplot(aes(x = reorder(continent, avg_life_exp), y = avg_life_exp, fill = continent)) +
    geom_bar(stat = "identity") +
    
    # Facet by time period
    facet_wrap(~time_period) +
    
    # Adjusting aesthetics and labels
    labs(
      x = "",
      y = "Average Life Expectancy",
      fill = "Continent",
      title = "Evolution of Average Life Expectancy per Continent for 1960-2021"
    ) +
    
    # Applying a classic theme and serif font
    theme_classic() +
    theme(
      text = element_text(family = "Helvetica", size = 12),
      axis.text.x = element_blank()
    )
  
  
 
  
  
 
  
  #pie chart
  
  library(ggplot2)
  library(scales)

  continent_counts <- Data_join %>%
    group_by(continent) %>%
    summarise(total_obs = sum(obs_value, na.rm = TRUE))
  
  continent_counts$percent <- continent_counts$total_obs / sum(continent_counts$total_obs) * 100
  

  continent_counts$percent_rounded <- round(continent_counts$percent, 2)
  

  pie_chart <- ggplot(continent_counts, aes(x = "", y = percent, fill = continent)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(percent_rounded, "%")), position = position_stack(vjust = 0.5), color = "white", size = 4) +
    labs(title = "Distribution of Unemployed Adolescents by Continent", fill = "Continent") +
    theme_minimal() +
    theme(legend.position = "right") +
    scale_fill_brewer(palette = "Set3") +
    guides(fill = guide_legend(title = "Continent")) +
    scale_y_continuous(labels = percent_format()) +
    theme(axis.text.x = element_blank()) +  # Hide x-axis labels
    theme(axis.text.y = element_blank()) +  # Hide y-axis labels
    theme(plot.title = element_text(hjust = 0.5))  # Center plot title
  
  pie_chart
  
  
  
  #Histogram
  
  library(ggplot2)
  
  # Assuming Data_join is your data frame
  # Adjust the binwidth parameter for desired bin size
  ggplot(Data_join, aes(x = obs_value)) +
    geom_histogram(binwidth = 5, fill = "#66c2a5", color = "#1f78b4", alpha = 0.8) +
    labs(
      x = "Unemployed Adolescent",
      y = "Frequency",
      title = "Distribution of Unemployed Adolescents",
      subtitle = "Histogram showing the frequency of unemployed adolescents"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold")
    )
 
  
  
  
  
  # Stacked bar chart by continent
  
  install.packages("countrycode")
  library(countrycode)

unicef_indicator_1_5_$continent <- countrycode(unicef_indicator_1_5_$country, "country.name", "continent")
continent_data <- unicef_indicator_1_5_ %>%
  group_by(continent, sex) %>%
  summarise(total_obs = sum(obs_value))


ggplot(continent_data, aes(x = continent, y = total_obs, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Continent",
    y = "Number of Unemployed Adolescents",
    fill = "Sex",
    title = "Distribution of Unemployed Adolescents by Sex Across Continents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



 #stacked bar chart

library(ggplot2)

ggplot(unicef_indicator_1_5_, aes(x = current_age, fill = sex)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  labs(
    x = "Current Age",
    y = "Number of Unemployed Adolescents",
    fill = "Sex",
    title = "Stacked Bar Chart of Unemployed Adolescents by Sex and Current Age"
  ) +
  theme_minimal()






# with regression line

scatter_plot <- ggplot(Data_join, aes(x = obs_value, y = `Population, total`)) +
  geom_point(alpha = 0.6, color = "darkblue") +  # Scatter plot points
  geom_smooth(method = "lm", aes(color = "Regression Line"), se = FALSE) +  # Regression line with unique color
  labs(
    x = "Observed Value",
    y = "Population",
    title = "Population vs. Observed Value"
  ) + 
  theme_minimal() +  # Use minimal theme for simplicity
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(hjust = 0.5),  # Center plot title
    axis.title = element_text(size = 10),  # Adjust axis title size
    legend.position = "bottom",  # Move legend to the bottom
    legend.title = element_blank()  # Remove legend title
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.5)))  # Adjust legend size

# Show the plot
print(scatter_plot)
