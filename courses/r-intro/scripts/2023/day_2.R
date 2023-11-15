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


cll2 <- read.csv(files[1]) |>
  tibble()|>
  select(-X) |>
  mutate(date = ymd_hms(date), 
         site = "cll2")

hors <- read.csv(files[2]) |>
  tibble()|>
  select(-X) |>
  mutate(date = ymd_hms(date), 
         site = "hors")

kc1 <- read.csv(files[3]) |>
  tibble()|>
  select(-X) |>
  mutate(date = ymd_hms(date), 
         site = "kc1", 
         no = ifelse(no == "missing", NA, no), 
         no = as.numeric(no)) 

lon6 <- read.csv(files[4]) |>
  tibble()|>
  select(-X) |>
  mutate(date = as_datetime(date), 
         site = "lon6")

my1 <- read.csv(files[5]) |>
  tibble()|>
  select(-X) |>
  mutate(date = as_datetime(date), 
         site = "my1")

left_join(cll2, lon6, by="date", suffix = c("_cll2", "_lon6"))

bind_rows(cll2, my1, hors, kc1, lon6)


# section 2 ---------------------------------------------------------------


#looping data read in 

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


