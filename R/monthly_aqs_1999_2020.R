#converting daily data to monthly data from 1999 to 2020

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
library(stringr)

# setwd ("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals")

setwd ("/projects/HAQ_LAB/mrasel/R/spatial-temporal-PM2.5-chemicals")

aqs.data <- read.fst("./data/AQS-daily-data-1999-2020.fst")
pm.data <- read.fst("./data/AQS-daily-pm-data-1999-2020.fst")

aqs.data$Date.Local <- as.Date(aqs.data$Date.Local)
pm.data$Date.Local <- as.Date(pm.data$Date.Local)



aqs.data.1999.2020<- aqs.data %>% filter(Date.Local>="2000-01-01" & Date.Local <= "2005-12-31" & State.Name=="Virginia")
pm.data.1999.2020<- pm.data %>% filter(Date.Local>="2000-01-01" & Date.Local <= "2005-12-31" & State.Name=="Virginia")

# setDT(aqs.data.1999.2020)[, lat.lon := paste(Latitude, Longitude, sep = "")]
# setDT(pm.data.1999.2020)[, lat.lon := paste(Latitude, Longitude, sep = "")]


#creating SITE ID
aqs.data.1999.2020$County.Code <- as.character(aqs.data.1999.2020$SCounty.Code)
aqs.data.1999.2020$State.Code <- str_pad(aqs.data.1999.2020$State.Code, 2, pad = "0")
aqs.data.1999.2020$County.Code <- str_pad((aqs.data.1999.2020$County.Code), 3, pad = "0")
aqs.data.1999.2020$Site.Num <- str_pad(aqs.data.1999.2020$Site.Num, 4, pad = "0")
setDT(aqs.data.1999.2020)[, ID := paste(State.Code, County.Code, Site.Num, sep = "")]

pm.data.1999.2020$County.Code <- as.character(pm.data.1999.2020$County.Code)
pm.data.1999.2020$State.Code <- str_pad(pm.data.1999.2020$State.Code, 2, pad = "0")
pm.data.1999.2020$County.Code <- str_pad((pm.data.1999.2020$County.Code), 3, pad = "0")
pm.data.1999.2020$Site.Num <- str_pad(pm.data.1999.2020$Site.Num, 4, pad = "0")
setDT(pm.data.1999.2020)[, ID := paste(State.Code, County.Code, Site.Num, sep = "")]

pm.data.1999.2020.test <- pm.data.1999.2020 %>% dplyr::select(ID, State.Code, County.Code, Site.Num,
                                                              Parameter.Name,Parameter.Code, Latitude, Longitude,
                                                              Date.Local, Datum, Units.of.Measure, Local.Site.Name,
                                                              Address, City.Name,
                                                         Arithmetic.Mean, AQI, State.Name, County.Name)

aqs.data.1999.2020 <- full_join(aqs.data.1999.2020, pm.data.1999.2020.test)
unique(aqs.data.1999.2020$Parameter.Name)


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
                     "OC PM2.5 LC TOR",
                     "Sulfate PM2.5 LC",
                     "Chloride PM2.5 LC",
                     "PM2.5 - Local Conditions",
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
                     "EC PM2.5 LC TOR")

aqs.data.1999.2020 <- aqs.data.1999.2020 %>% filter (Parameter.Name %in% parameters.want)

aqs.data.1999.2020$chemicals <- word(aqs.data.1999.2020$Parameter.Name, 1)

aqs.data.1999.2020$chemicals <- ifelse(aqs.data.1999.2020$chemicals=="Total", "Nitrate", aqs.data.1999.2020$chemicals)
#aqs.data.1999.2020$chemicals <- ifelse(aqs.data.1999.2020$chemicals=="Reconstructed", "PM2.5", aqs.data.1999.2020$chemicals)

unique(aqs.data.1999.2020$chemicals)

aqs.data.1999.2020<- aqs.data.1999.2020 %>% mutate(Date = ymd(Date.Local)) %>% mutate_at(vars(Date), funs(year, month, day))

aqs.data.1999.2020 <- as.data.table(aqs.data.1999.2020)

#aqs.yearly.1999.2020 <- aqs.yearly.1999.2020  %>%
#  filter (!State.Name %in% c("Arkansas", "Hawaii", "Country Of Mexico" ,"Virgin Islands") )

county <- unique(aqs.data.1999.2020$County.Code)

datalist=list()


for(i in 1:length(county)) {
  aqs.data.1999.2020.day <- aqs.data.1999.2020 %>% filter(County.Code %in%county[i])
  aqs.data.1999.2020.day <-   aqs.data.1999.2020.day[ ,list(mean=mean(Arithmetic.Mean)),
                                                      by=c("chemicals", "year", "month", "State.Name", "County.Name",
                                                           "Latitude", "Longitude", "State.Code",
                                                           "County.Code")]
  datalist[[i]] <-  aqs.data.1999.2020.day
}

aqs.monthly.yearly = do.call(rbind, datalist)

write.fst(aqs.yearly, "./data/aqs.1999.2020.monthly.fst")

# for (i in 1: length(dates)) {
#   for(i in 1:length(sites)) {
#     aqs.data.1999.2020.day <- aqs.data.1999.2020 %>% filter(Date.Local %in% dates[i] & Local.Site.Name %in%sites[i])
#     aqs.data.1999.2020.day$percentage <- aqs.data.1999.2020.day$Arithmetic.Mean/aqs.data.1999.2020.day$Arithmetic.Mean [aqs.data.1999.2020.day$chemicals=="PM2.5"]
#   }






