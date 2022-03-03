#data read and clean of IMPROVE data



rm(list = ls())


library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
require(readxl)
require(tidyverse)
library(lubridate)


setwd ("/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals")

data_dir <- '/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals/data/'

data.files = list.files(data_dir, pattern = "*.xlsx")
read_xlsx_files <- function(x){
  df <- read_xlsx(path = paste(data_dir, x, sep = "/"))
  return(df)
}

df <- lapply(data.files, read_xlsx_files ) %>%
  bind_rows()

# write.fst(df, "data/all.PM.chemicals.fst")

df.improve <- read.fst("data/all.PM.chemicals.fst")
#selecting chemical components

#data cleaning 
# http://views.cira.colostate.edu/fed/Reports/DatasetDetail.aspx?dssl=1&dsidse=10001

names(df)


df_selected <- df.improve %>% dplyr::select(Dataset, SiteCode, Date, SiteName, Latitude, Longitude, Elevation, State,
                     CountyFIPS, EPACode, ALf_Val, ammNO3f_Val, ammSO4f_Val, ASf_Val, BRf_Val, CAf_Val,
                     CLf_Val, CHLf_Val, CRf_Val, CUf_Val, ECf_Val, FEf_Val, Kf_Val, MF_Val, MGf_Val, MNf_Val, 
                     NAf_Val, NIf_Val, N2f_Val, NO3f_Val, OMCf_Val, Pf_Val, 
                     PBf_Val,  RBf_Val, Sf_Val, SEf_Val, SIf_Val, SO4f_Val, SRf_Val, TIf_Val,Vf_Val,
                     ZNf_Val, ZRf_Val, MT_Val, RCFM_Val)

names(df_selected)
df_selected <- df_selected %>% distinct()

old.names <- c ("ALf_Val", "ammNO3f_Val", "ammSO4f_Val", "ASf_Val", "BRf_Val", "CAf_Val",
                "CLf_Val", "CHLf_Val", "CRf_Val", "CUf_Val", "ECf_Val", "FEf_Val", "Kf_Val", 
                "MF_Val", "MGf_Val", "MNf_Val", "NAf_Val", "NIf_Val", "N2f_Val", "NO3f_Val", 
                "OMCf_Val", "Pf_Val", "PBf_Val", "RBf_Val", "Sf_Val", "SEf_Val", "SIf_Val", 
                "SO4f_Val", "SRf_Val", "TIf_Val", "Vf_Val", "ZNf_Val", "ZRf_Val", "MT_Val")
new.names <- c ("Aluminum", "Ammonium_Nitrate", "Ammonium_Sulfate", "Arsenic", "Bromine", "Calcium",
                "Chlorine", "Chloride", "Chromium", "Copper", "Elemental_Carbon", "Iron", "Potassium",
                "PM2.5", "Magnesium", "Manganese", "Sodium", "Nickel", "Nitrite", "Nitrate", 
                "OC", "Phosphorus", "Lead", "Rubidium", "Sulfur", "Selenium", "Silicon", 
                "Sulfate", "Strontium","Titanium", "Vanadium", "Zinc", "Zirconium", "PM10")




df_selected  <- df_selected %>% rename_at(vars(old.names), ~ new.names)

df_selected [ df_selected== -999.000] <- NA 

summary(df_selected)

#removing rows with no latitude and longitude values

df_selected <- df_selected[which(!is.na(df_selected$Latitude) & !is.na(df_selected$Longitude)),]

#date on different columns: year, month, day
df_selected$Date <- as.Date(df_selected$Date)

df_selected<- df_selected %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(Date), funs(year, month, day))

summary(df_selected)




#discarding states outside of CONUS US

df_selected<- df_selected %>% filter (!State %in% c("AK", "HI") )


df.2000.2005 <-df_selected %>% filter (year>=2000 & year <=2005)

summary(df.2000.2005)


write.fst(df_selected, "data/improve.pm.chemicals.fst")

all.chem <- read.fst("data/improve.pm.chemicals.fst")

# all.chem %>% top_n (5, PM2.5 )

all.chem.2000.2005 <- all.chem %>% filter (year>=2000 & year <=2005)
