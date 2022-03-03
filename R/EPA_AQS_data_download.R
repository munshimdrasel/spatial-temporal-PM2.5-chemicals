#AQS data



rm(list = ls())

library(downloader)
library(plyr)
library(fst)

require(tidyverse)



setwd ("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals")

data_dir <- '/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals/data/'

year <- c (2001:2020)

for (i in 1:length(year)) {
  url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_SPEC_",year[i],".zip")
  download(url, dest= paste0(data_dir,"AQS/","AQS_",year[i],".zip"), mode="wb") 
}



# getting all the zip files
zipF <- list.files(path = paste0(data_dir,"AQS"), 
                   pattern = "*.zip", full.names = TRUE)

# unzip all your files
ldply(.data = zipF, .fun = unzip, exdir = paste0(data_dir,"AQS"))


# get the csv files
csv_files <- list.files(path = paste0(data_dir,"AQS"), pattern = "*.csv", full.names = TRUE)

# read the csv files
my_data <- ldply(.data = csv_files, .fun = read.csv)

my_data <- my_data%>% distinct()

write.fst(my_data, "/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals/data/AQS-data.fst")




