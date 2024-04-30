
library(readr)
library(dplyr)
library(ggplot2)

# Read and prep data ------------------------------------------------------

density_data_raw = read_csv("C:/Users/carys/Documents/GitHub/r-course/courses/dataviz/data/historicalAverageData.csv")

density_data = density_data_raw %>%
  # filter for just the UK, France & Switzerland
  filter(country %in% c("united_kingdom", "france", "switzerland"))

# Create plot -------------------------------------------------------------

density_data %>% 
  ggplot(aes(mean, 
             fill = theYear, 
             color = theYear)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue"))+
  scale_fill_manual(values = c("darkgreen", "darkblue"))+
  facet_grid(country~name, scales = "free_y")

#used both a fill and color aesthetic, so need to scale them individually 


