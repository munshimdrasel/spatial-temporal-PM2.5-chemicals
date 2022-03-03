rm(list = ls())


library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
require(readxl)
require(tidyverse)
library(lubridate)
library(maptools)
library(gstat)

setwd ("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals")

ground.data <- read.fst("data/improve.pm.chemicals.fst")

# improve.data <- ground.data %>% filter (Dataset=="EPACSN")

# csn.data <- csn.data %>% dplyr::select(-"Ammonium_Nitrate",-"Ammonium_Sulfate", -"Copper", -"Nitrite", -"PM10" )

#converting daily data to monthly data
improve.monthly.ym<- setDT(ground.data)[, .(Aluminum = mean(Aluminum, na.rm=TRUE),
                                     Ammonium_Nitrate = mean(Ammonium_Nitrate, na.rm=TRUE),
                                     Ammonium_Sulfate = mean(Ammonium_Sulfate, na.rm=TRUE),
                                     Arsenic= mean(Arsenic, na.rm=TRUE),
                                     Bromine= mean(Bromine, na.rm=TRUE),
                                     Calcium= mean(Calcium, na.rm=TRUE),
                                     #Cerium= mean(Cerium, na.rm=TRUE),
                                     #Cesium= mean(Cesium, na.rm=TRUE),
                                     Chlorine= mean(Chlorine, na.rm=TRUE),
                                     Chloride= mean(Chloride, na.rm=TRUE),
                                     Chromium= mean(Chromium, na.rm=TRUE),
                                     #Cobalt= mean(Cobalt, na.rm=TRUE),
                                     Copper= mean(Copper, na.rm=TRUE),
                                     Elemental_Carbon= mean(Elemental_Carbon, na.rm=TRUE),
                                     #Europium= mean(Europium, na.rm=TRUE),
                                     #Gallium= mean(Gallium, na.rm=TRUE),
                                     # Gold= mean(Gold, na.rm=TRUE),
                                     # Hafnium= mean(Hafnium, na.rm=TRUE),
                                     # Indium= mean(Indium, na.rm=TRUE),
                                     # Iridium= mean(Iridium, na.rm=TRUE),
                                     Iron= mean(Iron, na.rm=TRUE),
                                     # Lanthanum= mean(Lanthanum, na.rm=TRUE),
                                     Lead= mean(Lead, na.rm=TRUE),
                                     Magnesium= mean(Magnesium, na.rm=TRUE),
                                     Manganese= mean(Manganese, na.rm=TRUE),
                                     # Mercury= mean(Mercury, na.rm=TRUE),
                                     # Molybdenum       = mean(Molybdenum, na.rm=TRUE),
                                     Nickel= mean(Nickel, na.rm=TRUE),
                                     # Niobium          = mean(Niobium, na.rm=TRUE),
                                     Nitrite               = mean(Nitrite, na.rm=TRUE),
                                     Nitrate       = mean(Nitrate, na.rm=TRUE),
                                     Phosphorus       = mean(Phosphorus, na.rm=TRUE),
                                     # Potassium       = mean(Potassium, na.rm=TRUE),
                                     Rubidium        = mean(Rubidium, na.rm=TRUE),
                                     # Samarium       = mean(Samarium, na.rm=TRUE),
                                     # Scandium        = mean(Scandium, na.rm=TRUE),
                                     Selenium         = mean(Selenium, na.rm=TRUE),
                                     Silicon         = mean(Silicon, na.rm=TRUE),
                                     # Silicon          = mean(Silicon, na.rm=TRUE),
                                     # Silver          = mean(Silver, na.rm=TRUE),
                                     Sodium           = mean(Sodium, na.rm=TRUE),
                                     Strontium       = mean(Strontium, na.rm=TRUE),
                                     Sulfate          = mean(Sulfate, na.rm=TRUE),
                                     # Tantalum         = mean(Tantalum, na.rm=TRUE),
                                     # Terbium          = mean(Terbium, na.rm=TRUE),
                                     # Tin              = mean(Tin, na.rm=TRUE),
                                     Titanium        = mean(Titanium, na.rm=TRUE),
                                     # Tungsten         = mean(Tungsten, na.rm=TRUE),
                                     Vanadium         = mean(Vanadium, na.rm=TRUE),
                                     # Yttrium          = mean(Yttrium, na.rm=TRUE),
                                     Zinc             = mean(Zinc, na.rm=TRUE),
                                     Zirconium       = mean(Zirconium, na.rm=TRUE),
                                     PM2.5           = mean(PM2.5, na.rm=TRUE),
                                     OC               = mean(OC, na.rm=TRUE),
                                     Sulfur           = mean(Sulfur, na.rm=TRUE)),,
                                     # Chloride         = mean(Chloride, na.rm=TRUE),
                                     # Sodium_Ion      = mean(Sodium_Ion, na.rm=TRUE),, 
                                 by = .(Dataset, SiteCode, SiteName, Latitude,
                                        Longitude, State, CountyFIPS, EPACode, year, month)]


#PM2.5
improve.monthly.ym%>% filter (year >= 2000 ) %>%
  ggplot(aes( year, PM2.5, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "PM2.5 (ug/m^3)",
                                                              title = "IMPROVE data PM2.5 over years in U.S.")  +
  theme(legend.position = "") 


#OC
improve.monthly.ym%>% filter (year >= 2000 ) %>%
  ggplot(aes( year, OC, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(2000, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "OC (ug/m^3)",
                                                              title = "OC over years in U.S.")  +
  theme(legend.position = "") 
