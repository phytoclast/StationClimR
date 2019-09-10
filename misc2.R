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


selectClim <- Norms2010




####-------------
StationMeans <- subset(Norms2010, !is.na(t01) &!is.na(t07) &!is.na(pp01) &!is.na(pp07))
StationMeans$b01 <- 0
StationMeans$b02 <- 0
StationMeans$b03 <- 0
StationMeans$b04 <- 0
StationMeans$b05 <- 0
StationMeans$b06 <- 0
StationMeans$b07 <- 0
StationMeans$b08 <- 0
StationMeans$b09 <- 0
StationMeans$b10 <- 0
StationMeans$b11 <- 0
StationMeans$b12 <- 0
for (i in 0:11){
  StationMeans[,which(colnames(StationMeans)=='b01')+i]  <- StationMeans[,which(colnames(StationMeans)=='t01')+i]*
    (StationMeans[,which(colnames(StationMeans)=='t01')+i]>0)*1
}




StationMeans$th01 <- StationMeans$t01*2-StationMeans$tl01
StationMeans$th02 <- StationMeans$t02*2-StationMeans$tl02
StationMeans$th03 <- StationMeans$t03*2-StationMeans$tl03
StationMeans$th04 <- StationMeans$t04*2-StationMeans$tl04
StationMeans$th05 <- StationMeans$t05*2-StationMeans$tl05
StationMeans$th06 <- StationMeans$t06*2-StationMeans$tl06
StationMeans$th07 <- StationMeans$t07*2-StationMeans$tl07
StationMeans$th08 <- StationMeans$t08*2-StationMeans$tl08
StationMeans$th09 <- StationMeans$t09*2-StationMeans$tl09
StationMeans$th10 <- StationMeans$t10*2-StationMeans$tl10
StationMeans$th11 <- StationMeans$t11*2-StationMeans$tl11
StationMeans$th12 <- StationMeans$t12*2-StationMeans$tl12

#calculate extreme winter low----
StationMeans$pacificsouth <- 1/((((StationMeans$Latitude - -22.7)/13)^2 + ((StationMeans$Longitude - -82.3)/14)^2)^2+1)
StationMeans$amazon2 <- 1/((((StationMeans$Latitude - -10.2)/5)^2 + ((StationMeans$Longitude - -59.9)/10)^2)^2+1)
StationMeans$amazon1 <- 1/((((StationMeans$Latitude - -2.8)/14)^2 + ((StationMeans$Longitude - -61.3)/19)^2)^2+1)
StationMeans$pacificcent <- 1/((((StationMeans$Latitude - 4.1)/21)^2 + ((StationMeans$Longitude - -122.4)/41)^2)^2+1)
StationMeans$mexico <- 1/((((StationMeans$Latitude - 26)/6)^2 + ((StationMeans$Longitude - -98.4)/12)^2)^2+1)
StationMeans$florida <- 1/((((StationMeans$Latitude - 27.5)/4)^2 + ((StationMeans$Longitude - -81.1)/8)^2)^2+1)
StationMeans$pacificnorth <- 1/((((StationMeans$Latitude - 32.9)/26)^2 + ((StationMeans$Longitude - -145)/27)^2)^2+1)
StationMeans$oklahoma <- 1/((((StationMeans$Latitude - 33.6)/4)^2 + ((StationMeans$Longitude - -98.4)/8)^2)^2+1)
StationMeans$arizona <- 1/((((StationMeans$Latitude - 34)/12)^2 + ((StationMeans$Longitude - -113.1)/8)^2)^2+1)
StationMeans$atlantic <- 1/((((StationMeans$Latitude - 34)/15)^2 + ((StationMeans$Longitude - -60.7)/19)^2)^2+1)
StationMeans$himalayas <- 1/((((StationMeans$Latitude - 35.3)/6)^2 + ((StationMeans$Longitude - 91.3)/13)^2)^2+1)
StationMeans$kentucky <- 1/((((StationMeans$Latitude - 38.5)/3)^2 + ((StationMeans$Longitude - -87.6)/9)^2)^2+1)
StationMeans$detroit <- 1/((((StationMeans$Latitude - 41.8)/3)^2 + ((StationMeans$Longitude - -82.6)/4)^2)^2+1)
StationMeans$ontario <- 1/((((StationMeans$Latitude - 44.6)/2)^2 + ((StationMeans$Longitude - -79.2)/6)^2)^2+1)
StationMeans$montana <- 1/((((StationMeans$Latitude - 45.4)/5)^2 + ((StationMeans$Longitude - -111.8)/10)^2)^2+1)
StationMeans$minn <- 1/((((StationMeans$Latitude - 47.6)/6)^2 + ((StationMeans$Longitude - -92.6)/12)^2)^2+1)
StationMeans$hudson <- 1/((((StationMeans$Latitude - 60)/7)^2 + ((StationMeans$Longitude - -87)/34)^2)^2+1)
StationMeans$siberia <- 1/((((StationMeans$Latitude - 61.2)/20)^2 + ((StationMeans$Longitude - 105.7)/39)^2)^2+1)
StationMeans$california <- 1/((((StationMeans$Latitude - 34.8)/9)^2 + ((StationMeans$Longitude - -128.2)/9)^2)^2+1)
StationMeans$washington <- 1/((((StationMeans$Latitude - 46)/5)^2 + ((StationMeans$Longitude - -126.6)/5)^2)^2+1)
StationMeans$colorado <- 1/((((StationMeans$Latitude - 38.3)/2)^2 + ((StationMeans$Longitude - -108.8)/3)^2)^2+1)
StationMeans$hawaii <- 1/((((StationMeans$Latitude - 21.3)/7)^2 + ((StationMeans$Longitude - -157.5)/11)^2)^2+1)
StationMeans$chess <- 1/((((StationMeans$Latitude - 37)/3)^2 + ((StationMeans$Longitude - -74)/3)^2)^2+1)

StationMeans$Tg <- pmax(apply(StationMeans[,c('b05','b06','b07','b08','b09','b10')], 1, FUN = mean), apply(StationMeans[,c('b11','b12','b01','b02','b03','b04')], 1, FUN = mean))/1
StationMeans$Tc <- apply(StationMeans[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = min)
StationMeans$Tcl <- apply(StationMeans[,c('tl01', 'tl02', 'tl04', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')], 1, FUN = min)
StationMeans$Tw <- apply(StationMeans[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = max)
StationMeans$Twh <- apply(StationMeans[,c('th01', 'th02', 'th04', 'th04', 'th05', 'th06', 'th07', 'th08', 'th09', 'th10', 'th11', 'th12')], 1, FUN = max)


StationMeans$Tclx<-	-9.171	+
  StationMeans$Tcl *	1.202	+
  StationMeans$Latitude *	-0.04149	+
  StationMeans$Elevation *	0.0008691	+
  StationMeans$Latitude * StationMeans$Elevation *	-0.00002455	+
  StationMeans$pacificsouth *	-1.792	+
  StationMeans$amazon2 *	2.573	+
  StationMeans$amazon1 *	-1.014	+
  StationMeans$pacificcent *	-0.749	+
  StationMeans$mexico *	-0.8227	+
  StationMeans$florida *	-3.557	+
  StationMeans$pacificnorth *	-1.246	+
  StationMeans$oklahoma *	0.1758	+
  StationMeans$arizona *	2.605	+
  StationMeans$chess *	0.8347	+
  StationMeans$atlantic *	0.2967	+
  StationMeans$himalayas *	-1.814	+
  StationMeans$kentucky *	-2.644	+
  StationMeans$detroit *	0	+
  StationMeans$ontario *	-2.314	+
  StationMeans$montana *	-4.415	+
  StationMeans$minn *	1.136	+
  StationMeans$hudson *	-5.154	+
  StationMeans$siberia *	-3.797	+
  StationMeans$california *	4.48	+
  StationMeans$washington *	3.597	+
  StationMeans$colorado *	1.458	+
  StationMeans$hawaii *	6.673	

StationMeans <- StationMeans[!is.na(StationMeans$Tclx),]
StationMeans <- subset(StationMeans, select = -c(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida,                                               pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, 
                                                 detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii))

#daylength


StationMeans$e01 <- 0
StationMeans$e02 <- 0
StationMeans$e03 <- 0
StationMeans$e04 <- 0
StationMeans$e05 <- 0
StationMeans$e06 <- 0
StationMeans$e07 <- 0
StationMeans$e08 <- 0
StationMeans$e09 <- 0
StationMeans$e10 <- 0
StationMeans$e11 <- 0
StationMeans$e12 <- 0



#----

for (i in 0:11){
  StationMeans$hs = acos(pmin(pmax(-tan(StationMeans$Latitude/360*2*3.141592) * tan(DaysMonth[i+1,]$declination),-1),1))
  StationMeans$Ra = 
    117.5 * (StationMeans$hs*sin(StationMeans$Latitude/360*2*3.141592)*sin(DaysMonth[i+1,]$declination) +
               cos(StationMeans$Latitude/360*2*3.141592)*cos(DaysMonth[i+1,]$declination)*sin(StationMeans$hs)) / 3.141592
  
  StationMeans[,which(colnames(StationMeans)=='e01')+i] <- 0.008404*
    216.7*exp(17.26939*
                StationMeans[,which(colnames(StationMeans)=='t01')+i]/(StationMeans[,which(colnames(StationMeans)=='t01')+i]+237.3))/
    (StationMeans[,which(colnames(StationMeans)=='t01')+i]+273.3)*
    (StationMeans$Ra)*DaysMonth[1+i,c('Days')]*
    abs((StationMeans[,which(colnames(StationMeans)=='t01')+i] - StationMeans[,which(colnames(StationMeans)=='tl01')+i])*2)^0.5 + 0.001
}


StationMeans <- subset(StationMeans, 
                       select = -c(hs,Ra))

StationMeans$a01 <- 0
StationMeans$a02 <- 0
StationMeans$a03 <- 0
StationMeans$a04 <- 0
StationMeans$a05 <- 0
StationMeans$a06 <- 0
StationMeans$a07 <- 0
StationMeans$a08 <- 0
StationMeans$a09 <- 0
StationMeans$a10 <- 0
StationMeans$a11 <- 0
StationMeans$a12 <- 0
for (i in 0:11){
  StationMeans[,which(colnames(StationMeans)=='a01')+i] <- 
    pmin(StationMeans[,which(colnames(StationMeans)=='e01')+i], StationMeans[,which(colnames(StationMeans)=='pp01')+i])
}


StationMeans$pAET <- apply(StationMeans[,which(colnames(StationMeans)=='a01'):which(colnames(StationMeans)=='a12')], MARGIN = 1, FUN='max')

StationMeans$PET <- apply(StationMeans[,which(colnames(StationMeans)=='e01'):which(colnames(StationMeans)=='e12')], MARGIN = 1, FUN='sum')

StationMeans$MAP <- apply(StationMeans[,which(colnames(StationMeans)=='pp01'):which(colnames(StationMeans)=='pp12')], MARGIN = 1, FUN='sum')

StationMeans$AET <- apply(StationMeans[,which(colnames(StationMeans)=='a01'):which(colnames(StationMeans)=='a12')], MARGIN = 1, FUN='sum')
StationMeans$MAAT <- apply(StationMeans[,which(colnames(StationMeans)=='t01'):which(colnames(StationMeans)=='t12')], MARGIN = 1, FUN='mean')

StationMeans$Deficit <- pmax(StationMeans$PET - StationMeans$AET, 0)
StationMeans$Surplus <- pmax(StationMeans$MAP - StationMeans$AET, 0)


StationMeans <- 
  StationMeans[,c("LRU","Station_ID","Station_Name","State",
                  "Latitude","Longitude","Elevation","Tg","Tc","Tcl","Tw","Twh","Tclx","MAAT","MAP","PET","AET","pAET",
                  "Deficit","Surplus")]


StationMeans$PPETRatio <- StationMeans$MAP/(StationMeans$PET +0.0001)
StationMeans$Mindex <- StationMeans$PPETRatio/(StationMeans$PPETRatio+1)

Stationyears <- StationMeans

####-------------
StationMeans <- aggregate(selectClim[,c("t01", "t02", "t03", "t04", "t05", "t06",
                                        "t07", "t08", "t09", "t10", "t11", "t12",
                                        "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                                        "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                                        "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                                        "pp07", "pp08", "pp09", "pp10", "pp11","pp12")],
                          by=list(selectClim$LRU, selectClim$Station_ID, selectClim$Station_Name, selectClim$State, selectClim$Latitude, selectClim$Longitude, selectClim$Elevation), FUN='mean')
colnames(StationMeans)[1:7] <- c('LRU', 'Station_ID', 'Station_Name', 'State', 'Latitude', 'Longitude', 'Elevation')
StationMeans$b01 <- 0
StationMeans$b02 <- 0
StationMeans$b03 <- 0
StationMeans$b04 <- 0
StationMeans$b05 <- 0
StationMeans$b06 <- 0
StationMeans$b07 <- 0
StationMeans$b08 <- 0
StationMeans$b09 <- 0
StationMeans$b10 <- 0
StationMeans$b11 <- 0
StationMeans$b12 <- 0
for (i in 0:11){
  StationMeans[,which(colnames(StationMeans)=='b01')+i]  <- StationMeans[,which(colnames(StationMeans)=='t01')+i]*
    (StationMeans[,which(colnames(StationMeans)=='t01')+i]>0)*1
}




StationMeans$th01 <- StationMeans$t01*2-StationMeans$tl01
StationMeans$th02 <- StationMeans$t02*2-StationMeans$tl02
StationMeans$th03 <- StationMeans$t03*2-StationMeans$tl03
StationMeans$th04 <- StationMeans$t04*2-StationMeans$tl04
StationMeans$th05 <- StationMeans$t05*2-StationMeans$tl05
StationMeans$th06 <- StationMeans$t06*2-StationMeans$tl06
StationMeans$th07 <- StationMeans$t07*2-StationMeans$tl07
StationMeans$th08 <- StationMeans$t08*2-StationMeans$tl08
StationMeans$th09 <- StationMeans$t09*2-StationMeans$tl09
StationMeans$th10 <- StationMeans$t10*2-StationMeans$tl10
StationMeans$th11 <- StationMeans$t11*2-StationMeans$tl11
StationMeans$th12 <- StationMeans$t12*2-StationMeans$tl12

#calculate extreme winter low----
StationMeans$pacificsouth <- 1/((((StationMeans$Latitude - -22.7)/13)^2 + ((StationMeans$Longitude - -82.3)/14)^2)^2+1)
StationMeans$amazon2 <- 1/((((StationMeans$Latitude - -10.2)/5)^2 + ((StationMeans$Longitude - -59.9)/10)^2)^2+1)
StationMeans$amazon1 <- 1/((((StationMeans$Latitude - -2.8)/14)^2 + ((StationMeans$Longitude - -61.3)/19)^2)^2+1)
StationMeans$pacificcent <- 1/((((StationMeans$Latitude - 4.1)/21)^2 + ((StationMeans$Longitude - -122.4)/41)^2)^2+1)
StationMeans$mexico <- 1/((((StationMeans$Latitude - 26)/6)^2 + ((StationMeans$Longitude - -98.4)/12)^2)^2+1)
StationMeans$florida <- 1/((((StationMeans$Latitude - 27.5)/4)^2 + ((StationMeans$Longitude - -81.1)/8)^2)^2+1)
StationMeans$pacificnorth <- 1/((((StationMeans$Latitude - 32.9)/26)^2 + ((StationMeans$Longitude - -145)/27)^2)^2+1)
StationMeans$oklahoma <- 1/((((StationMeans$Latitude - 33.6)/4)^2 + ((StationMeans$Longitude - -98.4)/8)^2)^2+1)
StationMeans$arizona <- 1/((((StationMeans$Latitude - 34)/12)^2 + ((StationMeans$Longitude - -113.1)/8)^2)^2+1)
StationMeans$atlantic <- 1/((((StationMeans$Latitude - 34)/15)^2 + ((StationMeans$Longitude - -60.7)/19)^2)^2+1)
StationMeans$himalayas <- 1/((((StationMeans$Latitude - 35.3)/6)^2 + ((StationMeans$Longitude - 91.3)/13)^2)^2+1)
StationMeans$kentucky <- 1/((((StationMeans$Latitude - 38.5)/3)^2 + ((StationMeans$Longitude - -87.6)/9)^2)^2+1)
StationMeans$detroit <- 1/((((StationMeans$Latitude - 41.8)/3)^2 + ((StationMeans$Longitude - -82.6)/4)^2)^2+1)
StationMeans$ontario <- 1/((((StationMeans$Latitude - 44.6)/2)^2 + ((StationMeans$Longitude - -79.2)/6)^2)^2+1)
StationMeans$montana <- 1/((((StationMeans$Latitude - 45.4)/5)^2 + ((StationMeans$Longitude - -111.8)/10)^2)^2+1)
StationMeans$minn <- 1/((((StationMeans$Latitude - 47.6)/6)^2 + ((StationMeans$Longitude - -92.6)/12)^2)^2+1)
StationMeans$hudson <- 1/((((StationMeans$Latitude - 60)/7)^2 + ((StationMeans$Longitude - -87)/34)^2)^2+1)
StationMeans$siberia <- 1/((((StationMeans$Latitude - 61.2)/20)^2 + ((StationMeans$Longitude - 105.7)/39)^2)^2+1)
StationMeans$california <- 1/((((StationMeans$Latitude - 34.8)/9)^2 + ((StationMeans$Longitude - -128.2)/9)^2)^2+1)
StationMeans$washington <- 1/((((StationMeans$Latitude - 46)/5)^2 + ((StationMeans$Longitude - -126.6)/5)^2)^2+1)
StationMeans$colorado <- 1/((((StationMeans$Latitude - 38.3)/2)^2 + ((StationMeans$Longitude - -108.8)/3)^2)^2+1)
StationMeans$hawaii <- 1/((((StationMeans$Latitude - 21.3)/7)^2 + ((StationMeans$Longitude - -157.5)/11)^2)^2+1)
StationMeans$chess <- 1/((((StationMeans$Latitude - 37)/3)^2 + ((StationMeans$Longitude - -74)/3)^2)^2+1)

StationMeans$Tg <- pmax(apply(StationMeans[,c('b05','b06','b07','b08','b09','b10')], 1, FUN = mean), apply(StationMeans[,c('b11','b12','b01','b02','b03','b04')], 1, FUN = mean))/1
StationMeans$Tc <- apply(StationMeans[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = min)
StationMeans$Tcl <- apply(StationMeans[,c('tl01', 'tl02', 'tl04', 'tl04', 'tl05', 'tl06', 'tl07', 'tl08', 'tl09', 'tl10', 'tl11', 'tl12')], 1, FUN = min)
StationMeans$Tw <- apply(StationMeans[,c('t01', 't02', 't04', 't04', 't05', 't06', 't07', 't08', 't09', 't10', 't11', 't12')], 1, FUN = max)
StationMeans$Twh <- apply(StationMeans[,c('th01', 'th02', 'th04', 'th04', 'th05', 'th06', 'th07', 'th08', 'th09', 'th10', 'th11', 'th12')], 1, FUN = max)


StationMeans$Tclx<-	-9.171	+
  StationMeans$Tcl *	1.202	+
  StationMeans$Latitude *	-0.04149	+
  StationMeans$Elevation *	0.0008691	+
  StationMeans$Latitude * StationMeans$Elevation *	-0.00002455	+
  StationMeans$pacificsouth *	-1.792	+
  StationMeans$amazon2 *	2.573	+
  StationMeans$amazon1 *	-1.014	+
  StationMeans$pacificcent *	-0.749	+
  StationMeans$mexico *	-0.8227	+
  StationMeans$florida *	-3.557	+
  StationMeans$pacificnorth *	-1.246	+
  StationMeans$oklahoma *	0.1758	+
  StationMeans$arizona *	2.605	+
  StationMeans$chess *	0.8347	+
  StationMeans$atlantic *	0.2967	+
  StationMeans$himalayas *	-1.814	+
  StationMeans$kentucky *	-2.644	+
  StationMeans$detroit *	0	+
  StationMeans$ontario *	-2.314	+
  StationMeans$montana *	-4.415	+
  StationMeans$minn *	1.136	+
  StationMeans$hudson *	-5.154	+
  StationMeans$siberia *	-3.797	+
  StationMeans$california *	4.48	+
  StationMeans$washington *	3.597	+
  StationMeans$colorado *	1.458	+
  StationMeans$hawaii *	6.673	

StationMeans <- StationMeans[!is.na(StationMeans$Tclx),]
StationMeans <- subset(StationMeans, select = -c(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida,                                               pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, 
                                                 detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii))

#daylength


StationMeans$e01 <- 0
StationMeans$e02 <- 0
StationMeans$e03 <- 0
StationMeans$e04 <- 0
StationMeans$e05 <- 0
StationMeans$e06 <- 0
StationMeans$e07 <- 0
StationMeans$e08 <- 0
StationMeans$e09 <- 0
StationMeans$e10 <- 0
StationMeans$e11 <- 0
StationMeans$e12 <- 0



#----

for (i in 0:11){
  StationMeans$hs = acos(pmin(pmax(-tan(StationMeans$Latitude/360*2*3.141592) * tan(DaysMonth[i+1,]$declination),-1),1))
  StationMeans$Ra = 
    117.5 * (StationMeans$hs*sin(StationMeans$Latitude/360*2*3.141592)*sin(DaysMonth[i+1,]$declination) +
               cos(StationMeans$Latitude/360*2*3.141592)*cos(DaysMonth[i+1,]$declination)*sin(StationMeans$hs)) / 3.141592
  
  StationMeans[,which(colnames(StationMeans)=='e01')+i] <- 0.008404*
    216.7*exp(17.26939*
                StationMeans[,which(colnames(StationMeans)=='t01')+i]/(StationMeans[,which(colnames(StationMeans)=='t01')+i]+237.3))/
    (StationMeans[,which(colnames(StationMeans)=='t01')+i]+273.3)*
    (StationMeans$Ra)*DaysMonth[1+i,c('Days')]*
    abs((StationMeans[,which(colnames(StationMeans)=='t01')+i] - StationMeans[,which(colnames(StationMeans)=='tl01')+i])*2)^0.5 + 0.001
}


StationMeans <- subset(StationMeans, 
                       select = -c(hs,Ra))

StationMeans$a01 <- 0
StationMeans$a02 <- 0
StationMeans$a03 <- 0
StationMeans$a04 <- 0
StationMeans$a05 <- 0
StationMeans$a06 <- 0
StationMeans$a07 <- 0
StationMeans$a08 <- 0
StationMeans$a09 <- 0
StationMeans$a10 <- 0
StationMeans$a11 <- 0
StationMeans$a12 <- 0
for (i in 0:11){
  StationMeans[,which(colnames(StationMeans)=='a01')+i] <- 
    pmin(StationMeans[,which(colnames(StationMeans)=='e01')+i], StationMeans[,which(colnames(StationMeans)=='pp01')+i])
}


StationMeans$pAET <- apply(StationMeans[,which(colnames(StationMeans)=='a01'):which(colnames(StationMeans)=='a12')], MARGIN = 1, FUN='max')

StationMeans$PET <- apply(StationMeans[,which(colnames(StationMeans)=='e01'):which(colnames(StationMeans)=='e12')], MARGIN = 1, FUN='sum')

StationMeans$MAP <- apply(StationMeans[,which(colnames(StationMeans)=='pp01'):which(colnames(StationMeans)=='pp12')], MARGIN = 1, FUN='sum')

StationMeans$AET <- apply(StationMeans[,which(colnames(StationMeans)=='a01'):which(colnames(StationMeans)=='a12')], MARGIN = 1, FUN='sum')
StationMeans$MAAT <- apply(StationMeans[,which(colnames(StationMeans)=='t01'):which(colnames(StationMeans)=='t12')], MARGIN = 1, FUN='mean')

StationMeans$Deficit <- pmax(StationMeans$PET - StationMeans$AET, 0)
StationMeans$Surplus <- pmax(StationMeans$MAP - StationMeans$AET, 0)


StationMeans <- 
  StationMeans[,c("LRU","Station_ID","Station_Name","State",
                  "Latitude","Longitude","Elevation","Tg","Tc","Tcl","Tw","Twh","Tclx","MAAT","MAP","PET","AET","pAET",
                  "Deficit","Surplus")]


StationMeans$PPETRatio <- StationMeans$MAP/(StationMeans$PET +0.0001)
StationMeans$Mindex <- StationMeans$PPETRatio/(StationMeans$PPETRatio+1)

stationyearlyextremes <- ddply(Stationyears, "Station_ID", summarise,
                    d90 = quantile(Deficit, 0.9), s90 = quantile(Surplus, 0.9),
                    m10 = quantile(Mindex, 0.1), m90 = quantile(Mindex, 0.9), 
                    pr10 = quantile(PPETRatio, 0.1), pr90 = quantile(PPETRatio, 0.9)
                    )
StationMeansPlus <- merge(StationMeans, stationyearlyextremes, by='Station_ID')
#colnames(StationMeansPlus)
#Latitude +Longitude +Elevation +Tg +Tc +Tcl +Tw +Twh +Tclx +MAAT +MAP +PET +AET +pAET +Deficit +Surplus +PPETRatio +Mindex
Defmod <- lm(d90 ~ MAP +Deficit +Surplus, data = StationMeansPlus)

summary(Defmod)

Mmod <- lm(PPETRatio ~ pr10, data = StationMeansPlus)

summary(Mmod)
tab <- as.data.frame(cbind(pr10 = c(0,0.25,0.5,1,2), other = c(0,0.25,0.5,1,2)))
log(1.4543073)/log(2)

tab$prediction <- predict(Mmod,tab)
cor(StationMeansPlus[,c("Latitude","Longitude","Elevation","Tg","Tc","Tcl","Tw","Twh","Tclx","MAAT","MAP","PET","AET","pAET","Deficit","Surplus","PPETRatio","Mindex","d90","s90","m10","m90","pr10","pr90")])

library(ggplot2)
climplot <-  ggplot() +
geom_point(data=StationMeansPlus, mapping=aes(x=PPETRatio, y=pr10),alpha = 0.2, color='black')
