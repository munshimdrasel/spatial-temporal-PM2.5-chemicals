#for data validation with bell study (they did 2000 feb-2005 dec) analysis

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

# setwd ("/projects/HAQ_LAB/mrasel/R/spatial-temporal-PM2.5-chemicals")

aqs.data <- read.fst("./data/AQS-daily-data-1999-2020.fst")

aqs.data$Date.Local <- as.Date(aqs.data$Date.Local)

aqs.data.2001<- aqs.data #%>%  filter (State.Name=="Virginia")


unique(aqs.data$Parameter.Name)
unique(aqs.data.2001$Parameter.Name)


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
"EC PM2.5 LC TOR",
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
"Beryllium PM2.5 LC")


aqs.data.2001 <- aqs.data.2001 %>% filter (Parameter.Name %in% parameters.want)

library(stringr)

#taking first word of Parameter Name variable 
aqs.data.2001$chemicals <- word(aqs.data.2001$Parameter.Name, 1)

unique(aqs.data.2001$chemicals)

#total nitrate should be nitrate, Reconstructed Mass PM2.5 LC should be PM2.5
aqs.data.2001$chemicals <- ifelse(aqs.data.2001$chemicals=="Total", "Nitrate", aqs.data.2001$chemicals)
aqs.data.2001$chemicals <- ifelse(aqs.data.2001$chemicals=="Reconstructed", "PM2.5", aqs.data.2001$chemicals)


unique(aqs.data.2001$chemicals)

#different columns for year, month and day
aqs.data.2001<- aqs.data.2001 %>% mutate(Date = ymd(Date.Local)) %>% mutate_at(vars(Date), funs(year, month, day))

aqs.data.2001 <- as.data.table(aqs.data.2001)

county <- unique(aqs.data.2001$County.Code)
#yearly average of species
datalist=list()


for(i in 1:length(county)) {
    aqs.data.2001.day <- aqs.data.2001 %>% filter( County.Code %in%county[i])
  aqs.data.2001.day <- aqs.data.2001.day[ ,list(mean=mean(Arithmetic.Mean)),
                                          by=c("chemicals", "year",  "State.Name", "County.Name",
                                               "Latitude", "Longitude", "State.Code",
                                                             "County.Code")]
  datalist[[i]] <-  aqs.data.2001.day
}

aqs.yearly.2000.2005 = do.call(rbind, datalist)


# write.fst(aqs.yearly.2000.2005, "./data/aqs.2000.2005.bell.fst")
  
# for (i in 1: length(dates)) {
#   for(i in 1:length(sites)) {
#     aqs.data.2001.day <- aqs.data.2001 %>% filter(Date.Local %in% dates[i] & Local.Site.Name %in%sites[i])
#     aqs.data.2001.day$percentage <- aqs.data.2001.day$Arithmetic.Mean/aqs.data.2001.day$Arithmetic.Mean [aqs.data.2001.day$chemicals=="PM2.5"]
#   }

aqs.yearly <- read.fst("./data/aqs.1999.2020.yearly.fst")


# aqs.yearly.2000.2005 <- aqs.yearly.2000.2005  %>%
  # filter (!State.Name %in% c("Arkansas", "Hawaii", "Country Of Mexico" ,"Virgin Islands") )

aqs.yearly%>% filter (year >= 1999& chemicals=="PM2.5" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + 
  labs(x= "Year", y = expression(paste(PM[2.5], " ug/", m^3)), title = expression(paste(PM[2.5]," over years in U.S.")))  +
  theme(legend.position = "")

ggsave("pm2.5.png", path = "./plots/")

aqs.yearly%>% filter (year >= 1999 & chemicals=="Sulfate" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + 
  labs(x= "Year", y = expression(paste("Sulfate ug/", m^3)), title = expression(paste("Sulfate over years in U.S.")))  +
  theme(legend.position = "")

ggsave("sulfate.png", path = "./plots/")

aqs.yearly%>% filter (year >= 1999 & chemicals=="Nitrate" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "Nitrate (ug/m^3)",
                                                              title = "Nitrate over years in U.S.")  +
  theme(legend.position = "")

ggsave("nitrate.png", path = "./plots/")

aqs.yearly%>% filter (year >= 1999 & chemicals=="Ammonium" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "Ammonium (ug/m^3)",
                                                              title = "Ammonium over years in U.S.")  +
  theme(legend.position = "")

ggsave("ammonium.png", path = "./plots/")

aqs.yearly%>% filter (year >= 1999 & chemicals=="OC" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "OC (ug/m^3)",
                                                              title = "OC over years in U.S.")  +
  theme(legend.position = "")

ggsave("OC.png", path = "./plots/")

aqs.yearly%>% filter (year >= 1999 & chemicals=="EC" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "EC (ug/m^3)",
                                                              title = "EC over years in U.S.")  +
  theme(legend.position = "")

ggsave("EC.png", path = "./plots/")

aqs.yearly%>% filter (year >= 1999 & chemicals=="EC" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "EC (ug/m^3)",
                                                              title = "EC over years in U.S.")  +
  theme(legend.position = "")

ggsave("EC.png", path = "./plots/")

#sodium
aqs.yearly%>% filter (year >= 1999 & chemicals=="Sodium" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "Sodium (ug/m^3)",
                                                              title = "Sodium over years in U.S.")  +
  theme(legend.position = "")

ggsave("Sodium.png", path = "./plots/")



#Silicon
aqs.yearly%>% filter (year >= 1999 & chemicals=="Silicon" ) %>%
  ggplot(aes( year, mean, group= year)) + geom_boxplot(aes(fill=year))+
  scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
                                                              y = "Silicon (ug/m^3)",
                                                              title = "Silicon over years in U.S.")  +
  theme(legend.position = "")

ggsave("Silicon.png", path = "./plots/")

#check with bell et al. study
aqs.yearly.2000.2005 <- aqs.yearly %>%  filter( year>=2000 & year<=2005)

# aqs.yearly.2000.2005$mean<- format(aqs.yearly.2000.2005$mean, scientific=F)
# 
check <- aqs.yearly.2000.2005 %>%
  group_by(chemicals) %>%
  summarise(Freq = mean(mean))

check$Freq <- format(check$Freq, scientific=F)


#monthly data check

aqs.monthly <- read.fst("./data/aqs.1999.2020.monthly.fst")
# 
# aqs.monthly%>% filter (year >= 1999  & chemicals=="PM2.5" ) %>%
#   ggplot(aes( month, mean, group= year)) + geom_boxplot(aes(fill=year))+
#   scale_x_continuous(breaks = seq(1999, 2021, by = 2)) + labs(x= "Year",
#                                                               y = "PM2.5 (ug/m^3)",
#                                                               title = "PM2.5 over years in U.S.")  +
#   theme(legend.position = "")
# 
# ggsave("pm2.5.png", path = "./plots/")

