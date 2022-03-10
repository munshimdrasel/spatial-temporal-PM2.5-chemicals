#Script 1: downloading data

#AQS data download on Hopper cluster


rm(list = ls())

library(downloader)
library(plyr)
library(fst)
library(tidyverse)



# setwd ("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals")
setwd ("/projects/HAQ_LAB/mrasel/R/spatial-temporal-PM2.5-chemicals")

species.data_dir <- './data/'

year <- c (1999:2020)

for (i in 1:length(year)) {
  url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_SPEC_",year[i],".zip")
  download(url, dest= paste0(species.data_dir,"AQS/","AQS_",year[i],".zip"), mode="wb") 
}



# getting all the zip files
zipF <- list.files(path = paste0(species.data_dir,"AQS"), 
                   pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = paste0(species.data_dir,"AQS"))


# get the csv files
csv_files <- list.files(path = paste0(species.data_dir,"AQS"), pattern = "*.csv", full.names = TRUE)

# read the csv files
my_data <- ldply(.data = csv_files, .fun = read.csv)

my_data <- my_data%>% distinct()

write.fst(my_data, "./data/AQS-daily-data-1999-2020.fst")



#daily PM data download

pm_data_dir <- './data/AQS-PM/'

year <- c (1999:2020)

for (i in 1:length(year)) {
  url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_88101_",year[i],".zip")
  download(url, dest= paste0(pm_data_dir,"AQS_",year[i],".zip"), mode="wb") 
}


# getting all the zip files
zipF <- list.files(path = paste0(pm_data_dir), 
                   pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = paste0(pm_data_dir))


# get the csv files
csv_files <- list.files(path = paste0(pm_data_dir), pattern = "*88101*", full.names = TRUE)

# read the csv files
pm_data <- ldply(.data = csv_files, .fun = read.csv)

pm_data <- pm_data%>% distinct()

write.fst(pm_data, "./data/AQS-daily-pm-data-1999-2020.fst")



