
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read and prep data ------------------------------------------------------

codes = c("lu0101a", "lu0102a", "ch0010a", "ch0011a", "MY1", "KC1")

scatter_data_raw = read_csv("C:/Users/carys/Documents/GitHub/r-course/courses/Dataviz/data/timeSeriesData.csv")

scatter_data = scatter_data_raw %>%
  # filter just for the codes we're interested in
  filter(code %in% codes) %>%
  # drop the median column - we don't need it
  select(-median) %>%
  # reshape so the pollutants are in their own columns
  pivot_wider(names_from = name, values_from = mean)

# Create plot -------------------------------------------------------------

scatter_data %>% 
  ggplot(aes(o3, no2))+
  geom_point(aes(color = code))+
  geom_smooth(method = "lm", 
              aes(group = code))+
  scale_x_continuous(name = "Ozone", 
                     limits = c(0, NA), # can use inf instead of NA
                     expand = c(0,0))+ 
  scale_y_continuous(name = "Nitrogen Dioxide", 
                     limits = c(0,NA), 
                     expand = c(0,0)) +
  facet_grid(~country)
  
