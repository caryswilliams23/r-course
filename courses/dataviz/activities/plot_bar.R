
library(readr)
library(dplyr)
library(ggplot2)

# Read and prep data ------------------------------------------------------

bar_data_raw = read_csv("C:/Users/carys/Documents/GitHub/r-course/courses/dataviz/data/historicalAverageData.csv")

bar_data = bar_data_raw %>% 
  # group by country, year & pollutant
  group_by(country, theYear, name) %>%
  # calculate means for these categories
  summarise(country_avg = mean(mean))

# Create plot -------------------------------------------------------------

bar_data %>% 
  ggplot(aes(y = country, 
             x = country_avg, 
             fill = theYear))+
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.placement = "outside" )+
  facet_wrap(~name, 
             strip.position = "bottom", 
             scales = "free_x")

#display.brewer.all to look at the color palette options 

