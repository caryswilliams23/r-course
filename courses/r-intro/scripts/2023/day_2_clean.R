# day_2.R
# ~~~~~~~

library(dplyr)
library(lubridate)
library(stringr)

#reading in the data

files<- list.files("courses/r-intro/data/taught/part_2/", 
                   full.names = TRUE)

siteNames = basename(files) |>
  str_remove(pattern = "_2018.csv") |>
  tolower()
#OR
siteNames = basename(files) |>
  word(1,1,sep = "_") |>
  tolower()

siteList = list()

for(i in 1:length(files)){
  siteList[[i]] = read.csv(files[i]) |>
    tibble() |>
    mutate(site = siteNames[i]) |>
    select(-X)
  
  if(siteNames[i] == "lon6"){
    siteList[[i]] = siteList [[i]] |>
      mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00"))
  }else{
    siteList[[i]] = siteList[[i]] |>
      mutate(date = ymd_hms(date))
  }
  
  if(siteNames[i] == "kc1") {
    siteList[[i]] = siteList[[i]] |>
    mutate(no = ifelse(no == "missing", NA, no), 
  no = as.numeric(no)) 
  } 
}

sites_middle <- bind_rows(siteList)
sites_middle |> count(site)
mean(sites_middle$no2, na.rm = TRUE)

#summarise 

sites_middle |>
  summarise(mean_no2 = mean(no2, na.rm = TRUE), 
            sd_no2 = sd(no2, na.rm = TRUE))

sites_middle |>
  group_by(site) |> 
  summarise(mean_no2 = mean(no2, na.rm = TRUE), 
            sd_no2 = sd(no2, na.rm = TRUE)) |>
  arrange(desc(mean_no2))

library(tidyr)

sites_wide <- sites_middle |>
  pivot_wider(names_from = site, values_from = c(no2, no, o3))

sites_long <- sites_middle |> 
  pivot_longer(c(no, no2, o3), names_to = "species", values_to = "conc")

nrow(sites_middle)
nrow(sites_long)

sites_long |>
  group_by(site, species) |> 
  summarise(mean_conc = mean(conc, na.rm = TRUE), 
            sd_conc = sd(conc, na.rm = TRUE)) |>
  ungroup() #still get the same output but now its a normal tibble

quantile(cll2$no2, na.rm = TRUE)

#flag values >95% quantile 

sites_long |>
  group_by(site, species) |>
  mutate(p_95 = quantile(conc, 0.95, na.rm = TRUE), 
         flag = conc > p_95) |>
  ungroup()

#filter

sites_long |>
  filter(site == "cll2",
         species == "no") #double == for equality

sites_long |> 
  filter(site == "cll2" | site == "kc1") |>
  count(site)

sites_long |> 
  filter(site %in% c("cll2", "kc1", "my1")) |>
  count(site)

#filtering for concentrations above a certain number 
sites_long |> filter(species == "no2", 
                     conc > 100)

sites_long |> 
  filter(date >= as_datetime("2018-12-25 00:00:00"), 
         date < as_datetime("2018-12-31 00:00:00"))

sites_long |> 
  filter(conc >= quantile(conc, 0.95, na.rm = TRUE))

# More time-based functionality 
#time averaging from hourly to daily 
sites_long |> 
  mutate(date = floor_date(date, "day")) |>
  group_by(date, site, species)|>
  summarise(conc = mean(conc, na.rm = TRUE)) |>
  ungroup()

sites_long |>
  mutate(hour_of_the_day = hour(date)) |>
  group_by(hour_of_the_day, site, species) |>
  summarise(conc = mean(conc, na.rm = TRUE))|>
  ungroup() |> 
  group_by(site, species) |> 
  filter(conc == max(conc, na.rm = TRUE)) |> 
  ungroup()

#wday and yday
sites_long |> 
  mutate(week_day = wday(date), 
         year_day = yday(date))

#date arithmetic 
sites_long |> 
  select(date) |> 
  mutate(
    plus_one = date +days (1))

#lag gets previous row values 
sites_long |> 
  group_by(site, species) |>
  arrange(date) |> 
  mutate(prev_conc = lag(conc), 
         conc_diff = conc - prev_conc) |> 
  ungroup() |> 
  arrange(site, species, date)
