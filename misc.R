
library(sf)
LRUNames <- readRDS(file='data/LRUNames.RDS')
lakes <- readRDS(file='data/lakes.RDS')
states <- readRDS(file='data/states.RDS')
LRU.shptidy <- readRDS(file='data/LRU.shptidy.RDS')
ocean = data.frame(y=c(min(LRU.shptidy$lat-100000),min(LRU.shptidy$lat-100000),max(LRU.shptidy$lat+100000),max(LRU.shptidy$lat+100000)), x=c(min(LRU.shptidy$long-100000),max(LRU.shptidy$long+100000),max(LRU.shptidy$long+100000),min(LRU.shptidy$long-100000)))
selMLRAshp <- LRU.shptidy[LRU.shptidy$id %in% '98A',]

library(ggplot2)
#calculate percentiles
library(plyr)
Norms2010 <- readRDS(file='data/Norms2010.RDS')


DaysMonth <- readRDS(file='data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
MLRA <- readRDS(file='data/LRU.RDS')
NormCoordTrans <- as.data.frame(readRDS(file='data/NormCoordTrans.RDS'))
colnames(NormCoordTrans)[2:3] <- c('x','y')
NormCoordTrans$x <- as.numeric(NormCoordTrans$x)
NormCoordTrans$y <- as.numeric(NormCoordTrans$y)
Norms2010<-merge(Norms2010,MLRA, by="Station_ID")
Norms2010<- Norms2010[,c('LRU', 'Station_ID','Station_Name', 'State', 'Latitude', 'Longitude', 'Elevation','Year_',
                         "t01", "t02", "t03", "t04", "t05", "t06",
                         "t07", "t08", "t09", "t10", "t11", "t12",
                         "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                         "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                         "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                         "pp07", "pp08", "pp09", "pp10", "pp11","pp12")]

Norms2010$LRU <- paste(Norms2010$Station_Name, Norms2010$State )
Norms2010$Station_Name <- paste(Norms2010$LRU, Norms2010$Year_)





#Make Monthly Rows
selectClim <- Norms2010[Norms2010$LRU %in% 'GRAND RAPIDS MI',]


#Jan
selectMonthly <- selectClim[,c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t01","tl01","pp01")]
colnames(selectMonthly) <- c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t","tl","p")
selectMonthly$Month<- 1

#Feb-Dec
for (i in 1:11){
  
  selectMonthlyA <- selectClim[,c("LRU","Station_ID","State","Latitude","Longitude","Elevation",
                                  colnames(selectClim)[which(colnames(selectClim)=='t01')+i],
                                  colnames(selectClim)[which(colnames(selectClim)=='tl01')+i],
                                  colnames(selectClim)[which(colnames(selectClim)=='pp01')+i])]
  colnames(selectMonthlyA)<- c("LRU","Station_ID","State","Latitude","Longitude","Elevation","t","tl","p")
  selectMonthlyA$Month<- i+1
  selectMonthly <- rbind(selectMonthly, selectMonthlyA)
}
rm(selectMonthlyA)
sumMonthly <- ddply(selectMonthly, "Month", summarise,Lat = mean(Latitude),Lon = mean(Longitude),Elev = mean(Elevation),
                    t25 = quantile(t, 0.25), t75 = quantile(t, 0.75), t = mean(t),
                    p25 = quantile(p, 0.25), p75 = quantile(p, 0.75), p = mean(p), 
                    tl = mean(tl))
 sumMonthly$th <- sumMonthly$t*2 - sumMonthly$tl
 sumMonthly <- merge(sumMonthly, DaysMonth, by.x='Month',by.y='Month_')

   sumMonthly$hs = acos(pmin(pmax(-tan(sumMonthly$Lat/360*2*3.141592) * tan(sumMonthly$declination),-1),1))
  
   sumMonthly$Ra  = 
     117.5 * (sumMonthly$hs*sin(sumMonthly$Lat/360*2*3.141592)*sin(sumMonthly$declination) +
                cos(sumMonthly$Lat/360*2*3.141592)*cos(sumMonthly$declination)*sin(sumMonthly$hs)) / 3.141592
  
   sumMonthly$e <- 0.008404*
     216.7*exp(17.26939*
                 sumMonthly$t/(sumMonthly$t+237.3))/
     (sumMonthly$t+273.3)*
     (sumMonthly$Ra)*sumMonthly$Days*
     abs((sumMonthly$th - sumMonthly$tl))^0.5 + 0.001
   
sumMonthly <- subset(sumMonthly, select = -c(Day_, Days, hs, Ra, declination))
   sumMonthly$a <- pmin(sumMonthly$e, sumMonthly$p)
   sumMonthly$b <- (sumMonthly$t > 0)*sumMonthly$t 
   
   peakAET <- max(sumMonthly$a)
   AET <- sum(sumMonthly$a)
   PET <- sum(sumMonthly$e)
   MAP <- sum(sumMonthly$p)
   Surplus <- max(MAP - AET, 0)
   Deficit <- max(PET - AET, 0)
   PPETRatio<-MAP/(PET+0.0001)
   MAAT <- sum(sumMonthly$t)
   SummerBioT <- max(mean(sumMonthly[c(1:4,11:12),'b']), mean(sumMonthly[c(5:10),'b']))
   Tc <- min(sumMonthly$t)
   Tcl <- min(sumMonthly$tl)
   Tw <- max(sumMonthly$t)
   Twh <- max(sumMonthly$th)
   Lat <- mean(sumMonthly$Lat)
   Lon <- mean(sumMonthly$Lon)
   Elev <- mean(sumMonthly$Elev)
   
   #calculate extreme winter low----
   pacificsouth <- 1/((((Lat - -22.7)/13)^2 + ((Lon - -82.3)/14)^2)^2+1)
   amazon2 <- 1/((((Lat - -10.2)/5)^2 + ((Lon - -59.9)/10)^2)^2+1)
   amazon1 <- 1/((((Lat - -2.8)/14)^2 + ((Lon - -61.3)/19)^2)^2+1)
   pacificcent <- 1/((((Lat - 4.1)/21)^2 + ((Lon - -122.4)/41)^2)^2+1)
   mexico <- 1/((((Lat - 26)/6)^2 + ((Lon - -98.4)/12)^2)^2+1)
   florida <- 1/((((Lat - 27.5)/4)^2 + ((Lon - -81.1)/8)^2)^2+1)
   pacificnorth <- 1/((((Lat - 32.9)/26)^2 + ((Lon - -145)/27)^2)^2+1)
   oklahoma <- 1/((((Lat - 33.6)/4)^2 + ((Lon - -98.4)/8)^2)^2+1)
   arizona <- 1/((((Lat - 34)/12)^2 + ((Lon - -113.1)/8)^2)^2+1)
   atlantic <- 1/((((Lat - 34)/15)^2 + ((Lon - -60.7)/19)^2)^2+1)
   himalayas <- 1/((((Lat - 35.3)/6)^2 + ((Lon - 91.3)/13)^2)^2+1)
   kentucky <- 1/((((Lat - 38.5)/3)^2 + ((Lon - -87.6)/9)^2)^2+1)
   detroit <- 1/((((Lat - 41.8)/3)^2 + ((Lon - -82.6)/4)^2)^2+1)
   ontario <- 1/((((Lat - 44.6)/2)^2 + ((Lon - -79.2)/6)^2)^2+1)
   montana <- 1/((((Lat - 45.4)/5)^2 + ((Lon - -111.8)/10)^2)^2+1)
   minn <- 1/((((Lat - 47.6)/6)^2 + ((Lon - -92.6)/12)^2)^2+1)
   hudson <- 1/((((Lat - 60)/7)^2 + ((Lon - -87)/34)^2)^2+1)
   siberia <- 1/((((Lat - 61.2)/20)^2 + ((Lon - 105.7)/39)^2)^2+1)
   california <- 1/((((Lat - 34.8)/9)^2 + ((Lon - -128.2)/9)^2)^2+1)
   washington <- 1/((((Lat - 46)/5)^2 + ((Lon - -126.6)/5)^2)^2+1)
   colorado <- 1/((((Lat - 38.3)/2)^2 + ((Lon - -108.8)/3)^2)^2+1)
   hawaii <- 1/((((Lat - 21.3)/7)^2 + ((Lon - -157.5)/11)^2)^2+1)
   chess <- 1/((((Lat - 37)/3)^2 + ((Lon - -74)/3)^2)^2+1)

   Tclx<-	-9.171	+
     Tcl *	1.202	+
     Lat *	-0.04149	+
     Elev *	0.0008691	+
     Lat * Elev *	-0.00002455	+
     pacificsouth *	-1.792	+
     amazon2 *	2.573	+
     amazon1 *	-1.014	+
     pacificcent *	-0.749	+
     mexico *	-0.8227	+
     florida *	-3.557	+
     pacificnorth *	-1.246	+
     oklahoma *	0.1758	+
     arizona *	2.605	+
     chess *	0.8347	+
     atlantic *	0.2967	+
     himalayas *	-1.814	+
     kentucky *	-2.644	+
     detroit *	0	+
     ontario *	-2.314	+
     montana *	-4.415	+
     minn *	1.136	+
     hudson *	-5.154	+
     siberia *	-3.797	+
     california *	4.48	+
     washington *	3.597	+
     colorado *	1.458	+
     hawaii *	6.673	
   
   
   rm(pacificsouth,amazon1,amazon2, pacificcent, mexico, florida, pacificnorth, oklahoma, arizona, atlantic, himalayas, kentucky, detroit, ontario, montana, minn, hudson, siberia, california, washington, colorado, chess, hawaii)
   

   
   
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
  
   StationMeans <- merge(StationMeans, NormCoordTrans, by='Station_ID')
   StationMeans$PPETRatio <- StationMeans$MAP/(StationMeans$PET +0.0001)
   StationMeans$Mindex <- StationMeans$PPETRatio/(StationMeans$PPETRatio+1)
   
   graphymax = max(StationMeans$Latitude)+2
   graphymin = min(StationMeans$Latitude)-2
   graphxmax = max(StationMeans$Longitude)+2
   graphxmin = min(StationMeans$Longitude)-2
   
   
   StationMeans$SP1 <- round(ifelse(StationMeans$PPETRatio < 0.5 & StationMeans$Surplus < 25 & StationMeans$pAET < 75, pmax(StationMeans$Surplus/25, StationMeans$pAET/75)  ,1),15)
   StationMeans$SP2 <- round(ifelse(StationMeans$SP1 >= 1, ifelse(StationMeans$pAET < 75 & (StationMeans$Deficit >= 150 | StationMeans$PPETRatio < 1), pmax(StationMeans$pAET/75, 150/(StationMeans$Deficit+150)),1),0),15)
   StationMeans$SP3 <- round(ifelse(StationMeans$SP2 >= 1, ifelse(StationMeans$Deficit >= 150 | StationMeans$PPETRatio < 1, pmax(150/(StationMeans$Deficit+150)),1),0),15)
   StationMeans$SP4 <- round(ifelse(StationMeans$SP3 >= 1, pmin(1-StationMeans$Deficit/150),0),15)
   StationMeans$SPindex <- StationMeans$SP1 + StationMeans$SP2 + StationMeans$SP3 + StationMeans$SP4 + 1 #Seasonal precipitation index
   StationMeans$Cindex <- pmin(StationMeans$Tclx+15, StationMeans$Tc) #Cold index
   StationMeans$Dindex <- StationMeans$Deficit/(StationMeans$Deficit + 100)
   StationMeans$Sindex <- StationMeans$Surplus/(StationMeans$Surplus + 100)
   StationMeans$Aindex <- StationMeans$pAET/(StationMeans$pAET + 100)
   #Key to climate type_____________________________________________________
   
   
   Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                          ifelse(Surplus < 25 & PPETRatio < 0.5 & peakAET < 75, "Isoxeric",
                                 ifelse(peakAET < 75,"Xerothermic","Pluviothermic")))
   
   
   
   
   
   
   
   MRegime <- ifelse(PPETRatio>=2,"Perhumid",
                     ifelse(PPETRatio>=1.414,"Moist-Humid",
                            ifelse(PPETRatio>=1,"Dry-Humid",
                                   ifelse(PPETRatio>=0.707,"Moist-Subhumid",
                                          ifelse(PPETRatio>=0.5,"Dry-Subhumid",
                                                 ifelse(PPETRatio>=0.25,"Semiarid",
                                                        ifelse(PPETRatio>=0.125,"Arid","Perarid"
                                                        )))))))
   
   
   BioTemperatureC <- 
     ifelse(Tc >= 20 & Tclx >=5,"Meso-Tropical",
            ifelse(Tc >= 15 & Tclx >=0,"Cryo-Tropical",
                   ifelse(Tc >= 10 & Tclx >=-5,"Thermo-Sutropical",
                          ifelse(Tc >= 5 & Tclx >=-10,"Meso-Subtropical",
                                 ifelse(Tc >= 0 & Tclx >=-15,"Cryo-Subtropical",
                                        ifelse(Tc >= -5 & Tclx >=-20,"Thermo-Temperate",
                                               ifelse(Tc >= -10 & Tclx >=-25,"Meso-Temperate",
                                                      ifelse(Tc >= -25 & Tclx >=-40,"Cryo-Temperate","Polar"
                                                      ))))))))
   
   BioTemperatureW <- ifelse(SummerBioT >= 24,"Hot (Lowland)",
                             ifelse(SummerBioT >= 18,"Warm (Premontane)",
                                    ifelse(SummerBioT >= 15,"Warm-Mild (Lower-Montane)",
                                           ifelse(SummerBioT >= 12,"Cool-Mild (Upper-Montane)",
                                                  ifelse(SummerBioT >= 6,"Cool (Subalpine)","Cold (Alpine)"
                                                  )))))
   Climatetext<-paste(BioTemperatureW," ",BioTemperatureC,", ",MRegime," ",Seasonalilty, sep="" )
   my_text1 <- paste("Lat: ",round(Lat,digits=2),"  Lon:", round(Lon,digits=2),"  Elev: ",round(Elev,digits=0)," m")
   metric <- paste("Lat: ",round(Lat,digits=2),"Â°;  Lon: ", round(Lon,digits=2),"Â°;  Elev: ",round(Elev,digits=0)," m","\n",
                   "MAAT: ",round(MAAT,digits=1),"Â°C;  ","MAP: ", round(MAP,0)," mm  ","\n",
                   "Warm Month: ", round(Tw,1),"Â°C; High: ",round(Twh,1),"Â°C; ", "Cold Month: ", round(Tc,1),"Â°C; Low: ",round(Tcl,1),"Â°C","\n",
                   "Growing Season Temperature: ",round(SummerBioT,digits=1),"Â°C; Annual Extreme Low: ", round(Tclx,1),"Â°C","\n",
                   "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus,0)," mm; Deficit: ", round(Deficit,0)," mm; Peak AET: ", round(peakAET,0), " mm","\n", Climatetext,sep="")
   #, "SPindex: ",round(SPindex,2),"; Cindex: ",round(Cindex,2),"\n"
   retro <- paste("Lat: ",round(Lat,digits=2),"Â°;  Lon: ", round(Lon,digits=2),"Â°;  Elev: ",round(Elev/0.3048,digits=0)," ft","\n",
                  "Annual Temperature: ",round(MAAT*1.8+32,digits=0),"Â°F;  ","Annual Precipitation: ", round(MAP/25.4,0)," in  ","\n",
                  "Warm Month: ", round(Tw*1.8+32,0),"Â°F; High: ",round(Twh*1.8+32,0),"Â°F; ", "Cold Month: ", round(Tc*1.8+32,0),"Â°F; Low: ",round(Tcl*1.8+32,0),"Â°F","\n",
                  "Growing Season Temperature: ",round(SummerBioT*1.8+32,digits=0),"Â°F; Annual Extreme Low: ", round(Tclx*1.8+32,0),"Â°F","\n",
                  "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus/25.4,0)," in; Deficit: ", round(Deficit/25.4,0)," in; Peak AET: ", round(peakAET/25.4,0), " in","\n", Climatetext,sep="")
   my_text2 <- if(input$RadioUnits == 'USC'){retro} else {metric}
   rv$my_text2 <- my_text2
   #aggregate graph
   climplot <- ggplot(sumMonthly, aes(x=Month)) + 
     geom_bar(stat="identity",aes(fill="Precipitation", y=p/5), alpha = 0.85,  color="blue") +
     geom_bar(stat="identity", aes(fill='PET', y=e/5), alpha = 0.60,  color="red" ) +
     geom_line(stat="identity",  aes(color= "Temperature", y=t), alpha = 1) +
     geom_point(aes(shape='Mean', y=t), color="red") +
     geom_point(aes(shape='Low', y=tl), color="red") +
     geom_point(aes(shape='High', y=th), color="red") +
     geom_errorbar(aes(ymin=p25/5, ymax=p75/5), width=.2,position=position_dodge(-0.9), color="blue") +
     geom_errorbar(aes(ymin=t25, ymax=t75), width=.2,position=position_dodge(0.9), color="red") +
     
     scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c('01','02','03','04','05','06','07','08','09','10','11','12'))+
     scale_y_continuous(name= "Temperature",
                        breaks=c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45), labels=c('-20 (-4)', '-15 (  5)', '-10 (14)', '-5 (23)', '0 (32)', '5 (41)', '10 (50)', '15 (59)', '20 (68)', '25 (77)', '30 (86)', '35 (95)', '40 (104)', 'Â°C (Â°F)'),
                        sec.axis = sec_axis(trans = ~.*1,
                                            name = "Precipitation",
                                            breaks=c(0,5,10,15,20,25,30,35,40,45),
                                            labels = c('0', '25   (1)', '50   (2)', '75   (3)', '100 (4)', '125 (5)', '150 (6)', '175 (7)', '200 (8)', 'mm (in)')))+
     
     
     
     #coord_cartesian(xlim = c(1,12), ylim = c(-20, 43))+
     
     theme(legend.position="bottom") +
     scale_fill_manual("Legend", values = c("Precipitation" = "cyan", "PET" = "yellow"))+
     scale_color_manual("",values = c("Temperature" = "red", "Mean" = "red", "Low" = "red", "High"="red","Growth"="darkgreen"))+
     scale_shape_manual("",values = c("Mean" = 19, "Low" = 6, "High"=2))+
     coord_fixed(ratio = 1/9,xlim = c(1,12), ylim = c(-20, 43))+
     labs(title = paste("Climate of ",selectClim[1,]$LRU, MLRAname, sep=""))# ,  subtitle = my_text1)
  
    #Supplemental Graph1----
     a1=data.frame(x=c(-50,-50,0,0), y=c(0,6,6,0))
   a2=data.frame(x=c(-50,-50,0,0), y=c(6,12,12,6))
   a3=data.frame(x=c(-50,-50,0,0), y=c(12,36,36,12))
   a4=data.frame(x=c(0,0,6,0), y=c(0,6,6,0))
   a5=data.frame(x=c(0,0,18,6), y=c(6,18,18,6))
   a6=data.frame(x=c(0,0,15,15), y=c(18,36,36,18))
   a7=data.frame(x=c(15,15,36,18), y=c(18,36,36,18))
   
   ll1 <- data.frame(x=c(-50,6), y=c(6,6))
   ll2 <- data.frame(x=c(0,0), y=c(0,36))
   ll3 <- data.frame(x=c(15,15), y=c(18,36))
   l1 <- data.frame(x=c(-50,12), y=c(12,12))
   l2 <- data.frame(x=c(-50,0), y=c(15,15))
   l3 <- data.frame(x=c(-50,18), y=c(18,18))
   l4 <- data.frame(x=c(0,24), y=c(24,24))
   l5 <- data.frame(x=c(-10,-10), y=c(0,36))
   l6 <- data.frame(x=c(-25,-25), y=c(0,36))
   
   climplot2 <-  ggplot() +
     geom_polygon(data=a1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.5)+
     geom_polygon(data=a2, mapping=aes(x=x, y=y, fill='boreal'),alpha = 0.5)+
     geom_polygon(data=a3, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.5)+
     geom_polygon(data=a4, mapping=aes(x=x, y=y, fill='andean'),alpha = 0.5)+
     geom_polygon(data=a5, mapping=aes(x=x, y=y, fill='oceanic'),alpha = 0.5)+
     geom_polygon(data=a6, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.5)+
     geom_polygon(data=a7, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.5)+
     
     geom_line(data=ll1, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=ll2, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=ll3, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=l1, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=l2, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=l3, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=l4, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=l5, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_line(data=l6, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
     geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Tg), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(x=Cindex, y=Tg),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c("alpine" = "pink",
                                            "boreal" = "darkgreen",
                                            "temperate" = "greenyellow",
                                            "andean" = "lightblue",
                                            "oceanic" = "darkcyan",
                                            "subtropical" = "orange",
                                            "tropical" = "darkred"
                                            
     ))+
     scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", 
                        breaks=c(-45,-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                        labels=c('-45 (-60)','-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                 '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
     scale_y_continuous(name= "Growing Season", breaks=c(0,6,12,18,24,30))+
     coord_fixed(ratio = 1/1,xlim = c(-45,30), ylim = c(0, 33))+
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, MLRAname, sep=""))+
     theme_bw()+
     theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
           panel.grid.major = element_line(), panel.grid.minor = element_blank())
   #Supplemental Graph3----
   bs1=data.frame(y=c(1,1,2,2), x=c(0,0.3333,0.3333,0))
   bs2=data.frame(y=c(2,2,3,3), x=c(0,1,1,0))
   bs3=data.frame(y=c(3,3,4,4), x=c(0,1,1,0))
   bs4=data.frame(y=c(4,4,5,5), x=c(0.5,1,1,0.5))
   
   bm1=data.frame(y=c(1,1,4,4), x=c(0,0.1111,0.1111,0))
   bm2=data.frame(y=c(1,1,4,4), x=c(0.1111,0.2,0.2,0.1111))
   bm3=data.frame(y=c(1,1,4,4), x=c(0.2,0.3333,0.333,0.2))
   bm4=data.frame(y=c(2,2,4,4), x=c(0.3333,0.5,0.5,0.3333))
   bm5=data.frame(y=c(2,2,5,5), x=c(0.5,0.6667,0.6667,0.5))
   bm6=data.frame(y=c(2,2,5,5), x=c(0.6667,1,1,0.6667))
   
   climplot3 <- ggplot() +
     geom_polygon(data=bs1, mapping=aes(x=x, y=y, fill='isoxeric'),alpha = 0.2)+
     geom_polygon(data=bs2, mapping=aes(x=x, y=y, fill='xerothermic'),alpha = 0.2)+
     geom_polygon(data=bs3, mapping=aes(x=x, y=y, fill='pluviothermic'),alpha = 0.2)+
     geom_polygon(data=bs4, mapping=aes(x=x, y=y, fill='isopluvial'),alpha = 0.2)+
     geom_polygon(data=bm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.2)+
     geom_polygon(data=bm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.2)+
     geom_polygon(data=bm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.2)+
     geom_polygon(data=bm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.2)+
     geom_polygon(data=bm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.2)+
     geom_polygon(data=bm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.2)+
     geom_point(data=StationMeans, mapping=aes(y=SPindex, x=Mindex), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(y=SPindex, x=Mindex),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c("isoxeric" = "red",
                                            "xerothermic" = "blue",
                                            "pluviothermic" = "yellow",
                                            "isopluvial" = "green",
                                            "perarid" = "red",
                                            "arid" = "orange",
                                            "semiarid" = "yellow",
                                            "subhumid" = "green",
                                            "humid" = "cyan",
                                            "perhumid" = "blue"
     ))+
     scale_y_continuous(name= "Seasonality", breaks=c(1, 2,3,4),
                        labels=c('Isoxeric', 'Xerothermic', 'Pluviothermic','Isopluvial'))+
     scale_x_continuous(name= "P/PET Ratio", breaks=c(0, 0.1111, 0.2,0.3333,0.5,0.6667),
                        labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
     coord_fixed(ratio = 1/9, ylim = c(1,5), xlim = c(0, 1))+
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, sep=""))+
     theme_bw()+
     theme(legend.position='none', axis.text.x = element_text(angle = 0, vjust = 0, hjust = -0.5), axis.text.y = element_text(vjust = -2), 
           panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
   
   #moisture x temperature
   bw1=data.frame(y=c(0,0,6,6), x=c(0,1,1,0))
   bw2=data.frame(y=c(6,6,12,12), x=c(0,1,1,0))
   bw3=data.frame(y=c(12,12,18,18), x=c(0,1,1,0))
   bw4=data.frame(y=c(18,18,24,24), x=c(0,1,1,0))
   bw5=data.frame(y=c(24,24,30,30), x=c(0,1,1,0))
   
   bmm1=data.frame(y=c(0,0,30,30), x=c(0,0.1111,0.1111,0))
   bmm2=data.frame(y=c(0,0,30,30), x=c(0.1111,0.2,0.2,0.1111))
   bmm3=data.frame(y=c(0,0,30,30), x=c(0.2,0.3333,0.333,0.2))
   bmm4=data.frame(y=c(0,0,30,30), x=c(0.3333,0.5,0.5,0.3333))
   bmm5=data.frame(y=c(0,0,30,30), x=c(0.5,0.6667,0.6667,0.5))
   bmm6=data.frame(y=c(0,0,30,30), x=c(0.6667,1,1,0.6667))
   
   
   climplot4 <- ggplot() +
     geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
     geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
     geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
     geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
     geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
     geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.1)+
     geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.1)+
     geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.1)+
     geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.1)+
     geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.1)+
     geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.1)+
     geom_point(data=StationMeans, mapping=aes(x=Mindex, y=Tg), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(x=Mindex, y=Tg),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                            "cool" = "green",
                                            "mild" = "yellow",
                                            "warm" = "orange",
                                            "hot" = "red",
                                            "perarid" = "red",
                                            "arid" = "orange",
                                            "semiarid" = "yellow",
                                            "subhumid" = "green",
                                            "humid" = "cyan",
                                            "perhumid" = "blue"
     ))+
     scale_y_reverse(name= "Growing Season", breaks=c(6,12,18,24,30),
                     labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
     scale_x_continuous(name= "P/PET Ratio", breaks=c(0, .1111, .2,0.3333,0.5,0.6667),
                        labels=c('perarid', 'arid 0.125', 'semiarid 0.25','subhumid 0.5','humid 1','perhumid 2'))+
     coord_fixed(ratio = 1/30,ylim = c(0,30), xlim = c(0, 1))+
     
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, sep=""))+
     theme_bw()+
     theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1), 
           axis.text.y = element_text(vjust = 0), 
           panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
   
   
   #surplus x deficit
   b1=data.frame(y=c(0,0,0.2,0.2), x=c(0,0.6,0.6,0))
   b2=data.frame(y=c(0,0,0.2,0.2), x=c(0.6,1,1,0.6))
   
   humidline =data.frame(y=c(0,1), x=c(0,1))
   b3=data.frame(y=c(0.2,0.2,1,1), x=c(0,0.6,0.6,0))
   b4=data.frame(y=c(0.2,0.2,1,1), x=c(0.6,1,1,0.6))
   climplot5 <- ggplot() +
     geom_polygon(data=b1, mapping=aes(x=x, y=y, fill='b'),alpha = 0.2)+
     geom_polygon(data=b2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.2)+
     geom_polygon(data=b3, mapping=aes(x=x, y=y, fill='d'),alpha = 0.2)+
     geom_polygon(data=b4, mapping=aes(x=x, y=y, fill='c'),alpha = 0.2)+
     geom_line(data=humidline, mapping=aes(x=x, y=y, fill='c'),color = 'black',alpha = 0.2)+
     geom_point(data=StationMeans, mapping=aes(x=Dindex, y=Sindex), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(x=Dindex, y=Sindex),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c(
       "a" = "red",
       "b" = "yellow",
       "c" = "green",
       "d" = "blue"
       
     ))+
     scale_y_continuous(name= "Surplus", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75,0.8571),
                        labels=c('0', '25', '50','75','100','150','300','600'))+
     scale_x_continuous(name= "Deficit", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75,0.8571),
                        labels=c('0', '25', '50','75','100','150','300','600'))+
     coord_fixed(ratio = 1/1, ylim = c(0, 1), xlim = c(0, 1))+
     
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, sep=""))+
     theme_bw()+
     theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
           axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
   #----
   #growingseason x pAET
   bw1=data.frame(y=c(0,0,6,6), x=c(0,1,1,0))
   bw2=data.frame(y=c(6,6,12,12), x=c(0,1,1,0))
   bw3=data.frame(y=c(12,12,18,18), x=c(0,1,1,0))
   bw4=data.frame(y=c(18,18,24,24), x=c(0,1,1,0))
   bw5=data.frame(y=c(24,24,30,30), x=c(0,1,1,0))
   
   bmm1=data.frame(y=c(0,0,30,30), x=c(0,0.2,0.2,0))
   bmm2=data.frame(y=c(0,0,30,30), x=c(0.2,0.3333,0.3333,0.2))
   bmm3=data.frame(y=c(0,0,30,30), x=c(0.3333,0.4286,0.4286,0.333))
   bmm4=data.frame(y=c(0,0,30,30), x=c(0.4286,0.5,0.5,0.4286))
   bmm5=data.frame(y=c(0,0,30,30), x=c(0.5,0.6,0.6,0.5))
   bmm6=data.frame(y=c(0,0,30,30), x=c(0.6,0.75,0.75,0.6))
   
   climplot6 <- ggplot() +
     geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
     geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
     geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
     geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
     geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
     geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     geom_point(data=StationMeans, mapping=aes(x=Aindex, y=Tg), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(x=Aindex, y=Tg),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                            "cool" = "green",
                                            "mild" = "yellow",
                                            "warm" = "orange",
                                            "hot" = "red",
                                            "a" = "blue",
                                            "b" = "yellow"
     ))+
     scale_y_reverse(name= "Growing Season", breaks=c(6,12,18,24,30),
                     labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
     scale_x_continuous(name= "Peak Monthly Actual Evapotranspiration", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                        labels=c('0', '25', '50','75','100','150','300'))+
     coord_fixed(ratio = 1/30,ylim = c(0,30), xlim = c(0, 1))+
     
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, sep=""))+
     theme_bw()+
     theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
           axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
   #winter x pAET
   bw1=data.frame(x=c(-100,-100,-25,-25), y=c(0,1,1,0))
   bw2=data.frame(x=c(-25,-25,0,0), y=c(0,1,1,0))
   bw3=data.frame(x=c(0,0,15,15), y=c(0,1,1,0))
   bw4=data.frame(x=c(15,15,100,100), y=c(0,1,1,0))
   
   bmm1=data.frame(x=c(-100,-100,100,100), y=c(0,0.2,0.2,0))
   bmm2=data.frame(x=c(-100,-100,100,100), y=c(0.2,0.3333,0.3333,0.2))
   bmm3=data.frame(x=c(-100,-100,100,100), y=c(0.3333,0.4286,0.4286,0.333))
   bmm4=data.frame(x=c(-100,-100,100,100), y=c(0.4286,0.5,0.5,0.4286))
   bmm5=data.frame(x=c(-100,-100,100,100), y=c(0.5,0.6,0.6,0.5))
   bmm6=data.frame(x=c(-100,-100,100,100), y=c(0.6,1,1,0.6))
   
   climplot7 <- ggplot() +
     geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='polar'),alpha = 0.2)+
     geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.2)+
     geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.2)+
     geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.2)+
     
     geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Aindex), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(x=Cindex, y=Aindex),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c("polar" = "orange",
                                            "temperate" = "yellow",
                                            "mild" = "yellowgreen",
                                            "tropical" = "darkgreen",
                                            
                                            "a" = "blue",
                                            "b" = "yellow"
     ))+
     scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", breaks=c(-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                        labels=c('-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                 '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
     scale_y_continuous(name= "Peak Monthly Actual Evapotranspiration", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                        labels=c('0', '25', '50','75','100','150','300'))+
     coord_fixed(ratio = 30/1,xlim = c(-40,30), ylim = c(0,1))+
     
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, sep=""))+
     theme_bw()+
     theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
           axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
   
   #Moisture x pAET
   bmm1=data.frame(x=c(0,0,1,1), y=c(0,0.6,0.6,0))
   bmm2=data.frame(x=c(0,0,1,1), y=c(0.6,1,1,0.6))
   
   bm1=data.frame(y=c(0,0,1,1), x=c(0,0.1111,0.1111,0))
   bm2=data.frame(y=c(0,0,1,1), x=c(0.1111,0.2,0.2,0.1111))
   bm3=data.frame(y=c(0,0,1,1), x=c(0.2,0.3333,0.333,0.2))
   bm4=data.frame(y=c(0,0,1,1), x=c(0.3333,0.5,0.5,0.3333))
   bm5=data.frame(y=c(0,0,1,1), x=c(0.5,0.6667,0.6667,0.5))
   bm6=data.frame(y=c(0,0,1,1), x=c(0.6667,1,1,0.6667))
   
   climplot8 <- ggplot() +
     geom_polygon(data=bm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.2)+
     geom_polygon(data=bm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.2)+
     geom_polygon(data=bm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.2)+
     geom_polygon(data=bm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.2)+
     geom_polygon(data=bm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.2)+
     geom_polygon(data=bm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.2)+
     geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
     geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
     
     geom_point(data=StationMeans, mapping=aes(x=Mindex, y=Dindex), color = 'black', size=0.5)+
     geom_density2d(data=StationMeans, mapping=aes(x=Mindex, y=Dindex),color = 'black',alpha = 0.25)+
     scale_fill_manual("Legend", values = c("perarid" = "red",
                                            "arid" = "orange",
                                            "semiarid" = "yellow",
                                            "subhumid" = "green",
                                            "humid" = "cyan",
                                            "perhumid" = "blue",
                                            
                                            "a" = "yellow",
                                            "b" = "cyan"
     ))+
     scale_x_continuous(name= "P/PET Ratio", breaks=c(0, .1111, .2,0.3333,0.5,0.6667),
                        labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
     scale_y_continuous(name= "Deficit", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                        labels=c('0', '25', '50','75','100','150','300'))+
     coord_fixed(ratio = 1/1,xlim = c(0,1), ylim = c(0, 1))+
     
     labs(title = paste("Climate of ",StationMeans[1,]$LRU, sep=""))+
     theme_bw()+
     theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
           axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
   #assemble supplemental summary
   ocean = data.frame(y=c(-90,-90,90,90), x=c(-180,180,180,-180))

     climplot9 <-  ggplot() +
       geom_polygon(data = ocean, 
                    aes(x = x, y = y),
                    color = 'darkgray', fill = 'lightcyan', size = .2)+
       geom_sf(data = states,
               color = 'darkgray', fill = 'lightyellow', size = .2)+
       geom_sf(data = lakes,
               color = 'darkgray', fill = 'lightcyan', size = .2)+
       geom_polygon(data = selMLRAshp, 
                    aes(x=long, y=lat, group=group),
                    color = 'darkgray', fill = 'green', alpha=0.7, size = .0, linetype = 'blank')+
       geom_point(data=StationMeans, mapping=aes(x=Longitude, y=Latitude), color = 'red', size=1)+
       
       coord_sf(xlim = c(graphxmin,graphxmax), ylim = c(graphymin,graphymax)) + theme_void() 
  
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
###-----------   
   
   
Norms2010<-merge(Norms2010,MLRA, by="Station_ID")
mlraagg <- aggregate(MLRA[,c('Station_ID')], by=list(MLRA$LRU), FUN=length)
write.csv(MLRA, 'MLRA.csv')
LRU <- read.csv('MLRA.csv')
saveRDS(LRU,'data/LRU.RDS')
LRU2 <- readRDS('data/LRU.RDS')
require(sp)
require(raster)
require(rgdal)
require(rms)
require(RODBC)
require(ggplot2)
require(broom)
LRU.shp <- readOGR("data/gis/MLRA_2018.shp")
LRU.shp2 <- spTransform(LRU.shp, crs("+init=epsg:4326"))
LRU.shp2 <- spTransform(LRU.shp, crs("+init=epsg:2163"))
LRU.shptidy <- tidy(LRU.shp2, region ='LRU')
LRUNames <- unique(as.data.frame(LRU.shp[, c('MLRA', 'LRU','MLRA_NAME')]))
LRUNames <- LRUNames[!is.na(LRUNames$MLRA_NAME),]
plot(LRU.shp2)
saveRDS(LRUNames, 'LRUNames.RDS')
saveRDS(LRU.shptidy, 'LRU.shptidy.RDS')
crs(LRU.shp)

Norms2010 <- readRDS('data/Norms2010.RDS')
names(Norms2010)
NormCoord <- unique(Norms2010[,c('Station_ID', 'Latitude', 'Longitude')])

xy <- NormCoord[,c('Longitude','Latitude')]
pts <- SpatialPointsDataFrame(coords = xy, data = NormCoord,
                              proj4string = CRS("+init=epsg:4326"))
pts2 <- spTransform(pts, crs("+init=epsg:2163"))
pts3 <- tidy(pts2, region='Station_ID')
pts3 <- as.data.frame(cbind(Station_ID = as.character(pts2$Station_ID),coordinates(pts2)))
pts3$x <- as.numeric(as.character(pts$Longitude))
pts3$x <- as.numeric(levels(pts$Longitude))
plot(x = pts$Longitude, y = pts$Latitude)
point <- 
pts[1,]$Latitude
str(point)

saveRDS(pts3, 'NormCoordTrans.RDS')

CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "))
geomaxmin <- readRDS(file='data/geomaxmin.rds')
pts <- 
selectMLRA.shp <- LRU.shptidy[LRU.shptidy$id %in% '98A', ]
MLRAname <- LRUNames[LRUNames$LRU %in% '98A',c('MLRA_NAME')]
ggplot() +
 geom_polygon(data = selectMLRA.shp, aes(x = long, y = lat, group=group))
              