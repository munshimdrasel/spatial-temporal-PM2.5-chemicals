#data clean from EPA CSN data and compare it with EPA websites data

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

data_dir <- '/Volumes/GoogleDrive/My Drive/R/spatial-temporal-PM2.5-chemicals/data/CSN/'

csn.data.files = list.files(data_dir, pattern = "*.xlsx")
read_xlsx_files <- function(x){
  df <- read_xlsx(path = paste(data_dir, x, sep = "/"))
  return(df)
}

df.csn <- lapply(csn.data.files, read_xlsx_files ) %>%
  bind_rows()

# write.fst(df.csn, "data/all.csn.pm.chemicals.fst")

df.csn <- read.fst("data/all.csn.pm.chemicals.fst")
#selecting chemical components

#data cleaning 
#parameters
#http://views.cira.colostate.edu/fed/Reports/DatasetDetail.aspx?dsidse=20002

names(df.csn)


df_csn_selected <- df.csn %>% dplyr::select(Dataset, SiteCode, Date, SiteName, Latitude, Longitude, Elevation, State,
                                    CountyFIPS, EPACode, ALf_Val, "88301_Val", "88102_Val", "ASf_Val",
                                    "88107_Val", "BRf_Val", "88110_Val", "CAf_Val", "88117_Val", "88118_Val",
                                     "CLf_Val", "CRf_Val",  "88113_Val",  "ECf_Val", "88121_Val",
                                    "88124_Val", "88143_Val", "88127_Val",  "88131_Val",
                                    "88133_Val",  "FEf_Val", "88146_Val", "PBf_Val",  
                                    "MGf_Val",  "MNf_Val", "88142_Val", "MOf_Val",  "NIf_Val",
                                    "88147_Val", "NO3f_Val", "Pf_Val", "Kf_Val", "RBf_Val",
                                    "88162_Val", "88163_Val", "SEf_Val", "SIf_Val", "88166_Val",
                                    "NAf_Val", "SRf_Val", "SO4f_Val", "88170_Val",
                                    "88172_Val", "88160_Val", "TIf_Val", "88186_Val", "Vf_Val",
                                    "88183_Val", "ZNf_Val", "ZRf_Val",  "88502_Val", "OCf_Val",
                                    
                                    "Sf_Val", "88203_Val", "88302_Val", "88303_Val")



df_csn_selected <- df_csn_selected %>% distinct()

old.names <- c ( "ALf_Val", "88301_Val", "88102_Val", "ASf_Val",
                 "88107_Val", "BRf_Val", "88110_Val", "CAf_Val", "88117_Val", "88118_Val",
                 "CLf_Val", "CRf_Val",  "88113_Val",  "ECf_Val", "88121_Val",
                 "88124_Val", "88143_Val", "88127_Val",  "88131_Val",
                 "88133_Val",  "FEf_Val", "88146_Val", "PBf_Val",  
                 "MGf_Val",  "MNf_Val", "88142_Val", "MOf_Val",  "NIf_Val",
                 "88147_Val", "NO3f_Val", "Pf_Val", "Kf_Val", "RBf_Val",
                 "88162_Val", "88163_Val", "SEf_Val", "SIf_Val", "88166_Val",
                 "NAf_Val", "SRf_Val", "SO4f_Val", "88170_Val",
                 "88172_Val", "88160_Val", "TIf_Val", "88186_Val", "Vf_Val",
                 "88183_Val", "ZNf_Val", "ZRf_Val",  "88502_Val", "OCf_Val", "Sf_Val", "88203_Val", 
                 "88302_Val", "88303_Val")

new.names <- c("Aluminum", "Ammonium", "Antimony", "Arsenic", "Barium", "Bromine", "Cadmium", "Calcium","Cerium",
               "Cesium", "Chlorine", "Chromium", "Cobalt",
"Elemental_Carbon", "Europium", "Gallium", "Gold" ,"Hafnium", "Indium", "Iridium", "Iron", "Lanthanum", "Lead", "Magnesium",
"Manganese","Mercury", "Molybdenum", "Nickel", "Niobium", "Nitrate", "Phosphorus", "Potassium", "Rubidium", "Samarium", 
"Scandium", "Selenium","Silicon",
"Silver", "Sodium", "Strontium", "Sulfate", "Tantalum", "Terbium", "Tin", "Titanium", "Tungsten", "Vanadium",
"Yttrium", "Zinc", "Zirconium", "PM2.5", "OC", "Sulfur", "Chloride",
"Sodium_Ion", "Potassium_Ion")


df_csn_selected  <- df_csn_selected %>% rename_at(vars(old.names), ~ new.names)


# date on different columns: year, month, day
df_csn_selected$Date <- as.Date(df_csn_selected$Date)

df_csn_selected<- df_csn_selected %>% mutate(Date = ymd(Date)) %>% mutate_at(vars(Date), funs(year, month, day))

summary(df_csn_selected)




#discarding states outside of CONUS US

df_csn_selected<- df_csn_selected %>% filter (!State %in% c("AK", "HI") )

write.fst(df_csn_selected, "data/csn.pm.chemicals.fst")

df_csn_selected<- read.fst("data/csn.pm.chemicals.fst")

df_csn_2001_2005 <- df_csn_selected %>%  filter (Date>="2000-02-01" & Date<="2005-12-31")

summary(df_csn_2001_2005)

all.chem <- read.fst("data/improve.pm.chemicals.fst")

all.chem <- all.chem %>% filter (year>=2000)

ground.data <- dplyr::bind_rows(df_csn_selected, all.chem)

ground.data <- ground.data %>% distinct()

write.fst(ground.data, "data/ground.pm.chemicals.fst")

ground.data <- read.fst("data/ground.pm.chemicals.fst")

ground.data.2000 <- ground.data %>%  filter (Date>="2000-02-01" & Date<="2005-12-31")

summary(ground.data.2000)



csn.data.2000 <- ground.data %>% filter(Date>="2000-02-01" & Date<="2005-12-31" & Dataset=="EPACSN")

summary(csn.data.2000)

# 
# 
# 
# 
# 
# ##################################################################
# ## Spatial Interpolation
# ##################################################################
# 
# idw_pm2.5 <- csn.data.2000 %>% dplyr::select(PM2.5, Latitude, Longitude)
# idw_pm2.5 <- na.omit(idw_pm2.5)
# sp::coordinates(idw_pm2.5) <- ~ Longitude + Latitude
# x.range <- as.integer(range(idw_pm2.5@coords[,1]))
# y.range <- as.integer(range(idw_pm2.5@coords[,2]))
# x.range <- as.integer(c(-126, -66))
# y.range <- as.integer(c(23,51))
# 
# grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.5), 
#                    y=seq(from=y.range[1], to=y.range[2], by=0.5))
# coordinates(grd) <- ~ x+y
# gridded(grd) <- TRUE
# 
# 
# # plot(grd, cex=1.5)
# # points(lockdown, pch=1, col='red', cex=1)
# # title("Interpolation Grid and Sample Points")
# usmap <- readShapePoly("./data/cb_2019_us_nation_5m/cb_2019_us_nation_5m.shp")
# usmapoutline <- fortify(usmap)
# 
# 
# 
# ################## idw ##################
# 
# idw <- idw(PM2.5 ~ 1, locations=idw_pm2.5, newdata=grd, nmax=30)
# idw.output <- as.data.frame(idw)
# names(idw.output)[c(1:3)]<-c("lon","lat", "pm2.5.pred")
# 
# xy <- idw.output[,c("lon","lat")]
# proj4string(usmap) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# spdf <- SpatialPointsDataFrame(coords = xy, data = idw.output,
#                                proj4string = CRS("+proj=longlat +datum=WGS84 
#                                                  +ellps=WGS84 +towgs84=0,0,0"))
# over <- over(spdf, usmap, fn=NULL)
# points <- cbind(spdf@data, over)
# points$var1.var <- NULL
# points <- na.omit(points)
# plot_idw <- ggplot(data=points, aes(x=lon,y=lat))
# layer1_idw <- c(geom_tile(data=points, aes(fill=pm2.5.pred)))
# layer2_idw <- c(geom_path(data=usmap, aes(long, lat, group=group), colour = "grey40", size=1))
# 
# ## idw smoothed ATT map
# plot_idw+layer1_idw+layer2_idw+scale_fill_gradient2( mid="white", low="blue", high="red")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="PM2.5") + ggtitle("IDW-Smoothed")
# 
# ## point-level ATT map
# blank_layer <- c(geom_tile(data=points, fill="grey80"))
# plot_idw+blank_layer+borders("state")+layer2_idw+geom_point(data = ATT2, aes(x=Longitude, y=Latitude, color=ATT)) + scale_color_gradient2(midpoint = 0, mid="#fff650", high="#fa7b05", low="#18b342")+coord_equal()+xlim(-126,-66)+ylim(23,51) + xlab("Longitude") + ylab("Latitude") + labs(fill="PM2.5", color="PM2.5") + ggtitle("Point-Level Average Treatment Effect Estimates")
# 



