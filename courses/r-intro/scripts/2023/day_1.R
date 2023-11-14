library(dplyr)
library(openair)
library(lubridate)

#read NO

MY1_no = read.csv("courses/r-intro/data/taught/part_1/MY1_no_2018.csv") |>
  tibble() |> #extra class of being a tibble, easier to view later down the line 
  mutate(date = ymd_hms(date, tz = "GMT"))  #changing date column to date and setting the time zone 

#read NO2

MY1_no2 = read.csv("courses/r-intro/data/taught/part_1/MY1_no2_2018.csv") |>
  tibble() |> #extra class of being a tibble, easier to view later down the line 
  mutate(date = ymd_hms(date, tz = "GMT"))  #changing date column to date and setting the time zone 

#read ozone 

MY1_o3 = read.csv("courses/r-intro/data/taught/part_1/MY1_o3_2018.csv") |>
  tibble() |> #extra class of being a tibble, easier to view later down the line 
  mutate(date = ymd_hms(date, tz = "GMT"))  #changing date column to date and setting the time zone 

#read met data 

MY1_met = read.csv("courses/r-intro/data/taught/part_1/MY1_met_2018.csv") |>
  tibble() |> #extra class of being a tibble, easier to view later down the line 
  mutate(date = ymd_hms(date, tz = "GMT"))  #changing date column to date and setting the time zone 

#joining data 

my1 <- MY1_no |>
  left_join(MY1_no2, by = "date") |>
  left_join(MY1_o3, by = "date") |>
  left_join(MY1_met, by = "date")

#stats

mean(my1$no, na.rm = TRUE)
summary(my1$no) #good sanity check to understand data 
summary(my1)

#ways to sanity check data and get a better understanding of data 
hist(my1$no2)
density(my1$no2, na.rm = TRUE) |> plot()
plot(my1$no2, my1$o3)

#linear model 

mod <- lm(no2 ~  no, data = my1)
coef(mod)

#to get r2 save the output of summary
mod_sum <- summary(mod)
mod_sum$r.squared
names(mod_sum)

plot(my1$no, my1$no2)
abline(a=coefficients[1], b = coefficients[2], col= "red")

#looking at colours in R


#notes
#dbl is a number with decimal point#never ignore a warning message 