library(shiny)
library(sf)
library(plyr)
Norms2010 <- readRDS(file='data/Norms2010.RDS')
DaysMonth <- readRDS(file='data/DaysMonth.RDS')
savedselect <- readRDS(file='data/savedselect.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
LRUNames <- readRDS(file='data/LRUNames.RDS')
lakes <- readRDS(file='data/lakes.RDS')
states <- readRDS(file='data/states.RDS')
LRU.shptidy <- readRDS(file='data/LRU.shptidy.RDS')
geomaxmin <- readRDS(file='data/geomaxmin.rds')
geomaxmin <- geomaxmin[c(1,248,264:320,330:335,344,349,353,360,370),]
MLRA <- readRDS(file='data/LRU.RDS')
NormCoordTrans <- as.data.frame(readRDS(file='data/NormCoordTrans.RDS'))
colnames(NormCoordTrans)[2:3] <- c('x','y')
Norms2010<-merge(Norms2010,MLRA, by="Station_ID")
MLRAStationList <- unique(Norms2010[,c("LRU", "Station_ID", "Station_Name","State", "Latitude","Longitude","Elevation")])
Norms2010<- Norms2010[,c('LRU', 'Station_ID','Station_Name', 'State', 'Latitude', 'Longitude', 'Elevation','Year_',
                         "t01", "t02", "t03", "t04", "t05", "t06",
                         "t07", "t08", "t09", "t10", "t11", "t12",
                         "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                         "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                         "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                         "pp07", "pp08", "pp09", "pp10", "pp11","pp12")]
Norms2010$LRU <- paste(Norms2010$Station_Name, Norms2010$State )
Norms2010$Station_Name <- paste(Norms2010$LRU, Norms2010$Year_)
MLRAStationList$Station_Name <- paste(MLRAStationList$Station_Name, MLRAStationList$State )