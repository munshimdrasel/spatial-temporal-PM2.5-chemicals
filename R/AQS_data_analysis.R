rm(list = ls())


library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
require(readxl)
require(tidyverse)
library(lubridate)
library(fst)


setwd ("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals")


aqs.data <- read.fst("./data/AQS-data.fst")

aqs.data$Date.Local <- as.Date(aqs.data$Date.Local)

aqs.data.2001<- aqs.data %>%  filter (Date.Local>="2001-02-01" & Date.Local<="2005-12-31")
aqs.data.2001<- aqs.data %>% mutate(Date = ymd(Date.Local)) %>% mutate_at(vars(Date), funs(year, month, day))


# parameters.given <- as.vector(unique(aqs.data$Parameter.Name))

parameters.want <- c("Antimony PM2.5 LC",	
"Arsenic PM2.5 LC",
"Aluminum PM2.5 LC",
"Bromine PM2.5 LC",
"Calcium PM2.5 LC",
"Cobalt PM2.5 LC",
"Chlorine PM2.5 LC",
"Cesium PM2.5 LC",
"Gallium PM2.5 LC",
"Hafnium PM2.5 LC",
"Indium PM2.5 LC",
"Iridium PM2.5 LC",
"Nickel PM2.5 LC",
"Mercury PM2.5 LC",
"Lanthanum PM2.5 LC",
"Phosphorus PM2.5 LC",
"Tin PM2.5 LC",
"Samarium PM2.5 LC",
"Vanadium PM2.5 LC",
"Silver PM2.5 LC",
"Strontium PM2.5 LC",
"Tantalum PM2.5 LC",
"Rubidium PM2.5 LC",
"Yttrium PM2.5 LC",
"Zirconium PM2.5 LC",
"Ammonium Ion PM2.5 LC",
"Potassium Ion PM2.5 LC",
"OC CSN Unadjusted PM2.5 LC TOT",
"Sulfate PM2.5 LC",
"Chloride PM2.5 LC",
"Reconstructed Mass PM2.5 LC",
"Uranium PM2.5 LC",
"Barium PM2.5 LC",
"Cadmium PM2.5 LC",
"Chromium PM2.5 LC",
"Copper PM2.5 LC",
"Cerium PM2.5 LC",
"Europium PM2.5 LC",
"Iron PM2.5 LC",
"Lead PM2.5 LC",
"Manganese PM2.5 LC",
"Molybdenum PM2.5 LC",
"Magnesium PM2.5 LC",
"Gold PM2.5 LC",
"Niobium PM2.5 LC",
"Selenium PM2.5 LC",
"Titanium PM2.5 LC",
"Scandium PM2.5 LC",
"Silicon PM2.5 LC",
"Zinc PM2.5 LC",
"Sulfur PM2.5 LC",
"Terbium PM2.5 LC",
"Potassium PM2.5 LC",
"Sodium PM2.5 LC",
"Tungsten PM2.5 LC",
"Sodium Ion Pm2.5 LC",
"Total Nitrate PM2.5 LC",
"Beryllium PM2.5 LC",
"EC PM2.5 LC TOT")

aqs.data.2001 <- aqs.data.2001 %>% filter (Parameter.Name %in% parameters.want)

library(stringr)

aqs.data.2001$chemicals <- word(aqs.data.2001$Parameter.Name, 1)



