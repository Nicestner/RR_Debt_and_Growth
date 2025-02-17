require(ggplot2)
require(readxl)
require(plyr)
require(dplyr)
require(tidyr) # reshape wide to long formats
require(forcats) # reorder columns 

#################################### RR corrected calculation #################################### 
rm(list=ls()) # clear environment
setwd("~/") #Dokumente wd
RRcorr.dataset <- read_excel("RR_data_longtable.xlsx") #long-datensatz aus allen 'new' country werten von RR entspricht summary_Final_NZstatistics
RRcountries <- c(unique(RRcorr.dataset$country)) # create country vector
RRmaxyear <- setNames( rep(NA, 20), RRcountries) 
for (i in 1:length(RRmaxyear)){
  RRmaxyear.i <- subset(RRcorr.dataset, country=RRcountries[i])
  RRmaxyear[i] <- max(RRcorr.dataset$year)
} # check until when data goes
RRcorr.postwar <- subset(RRcorr.dataset, year > 1945 & year < 2010) # redundant here
RRcorr.postwar <- na.omit(RRcorr.postwar) 
RRmedians <- data.frame(RRcountries)
RRcolnames <- c("country", "#<30debt/gdp", "#30-60debt/gdp", "#60-90debt/gdp", "#>90debt/gdp", "<30growth", "30-60growth", "60-90growth", ">90growth", "<30inflation", "30-60inflation", "60-90inflation", ">90inflation")
boundary <- c(0, 30, 60, 90)
### check if values equal to boundary are relevant
RRboundary <- which(RRcorr.postwar$debtgdp == boundary[2] || RRcorr.postwar$debtgdp == boundary[3] || RRcorr.postwar$debtgdp == boundary[4])
# does not matter how boundaries are considered
for (i in 1:length(RRcountries)) {
  RRmedians[ i, 2] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp <= boundary[2] )))
  RRmedians[ i, 3] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[2] & RRcorr.postwar$debtgdp <= boundary[3] )))
  RRmedians[ i, 4] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[3] & RRcorr.postwar$debtgdp <= boundary[4] )))
  RRmedians[ i, 5] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp >= boundary[4] ))) 
  RRmedians[ i, 6] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp <= boundary[2] ))
  RRmedians[ i, 7] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[2] & RRcorr.postwar$debtgdp <= boundary[3] ))
  RRmedians[ i, 8] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[3] & RRcorr.postwar$debtgdp <= boundary[4] ))
  RRmedians[ i, 9] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp >= boundary[4] ))
  RRmedians[ i, 10] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp <= boundary[2] ))
  RRmedians[ i, 11] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[2] & RRcorr.postwar$debtgdp <= boundary[3] ))
  RRmedians[ i, 12] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[3] & RRcorr.postwar$debtgdp <= boundary[4] ))
  RRmedians[ i, 13] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp >= boundary[4] ))
} # how to deal with smaller or equal to or both if all is within one command?
names(RRmedians) <- RRcolnames
### calculate observation-weighted averages instead of medians of medians ###
RRweightedpostwar <- RRmedians
for (i in 1:length(RRcountries)) {
  RRweightedpostwar[ i, 2] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp <= boundary[2] ))/sum(RRmedians$`#<30debt/gdp`))
  RRweightedpostwar[ i, 3] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[2] & RRcorr.postwar$debtgdp <= boundary[3] ))/sum(RRmedians$`#30-60debt/gdp`))
  RRweightedpostwar[ i, 4] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[3] & RRcorr.postwar$debtgdp <= boundary[4] ))/sum(RRmedians$`#60-90debt/gdp`))
  RRweightedpostwar[ i, 5] <- c(length(which(RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp >= boundary[4] ))/sum(RRmedians$`#>90debt/gdp`)) 
  RRweightedpostwar[ i, 6] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp <= boundary[2] ))
  RRweightedpostwar[ i, 7] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[2] & RRcorr.postwar$debtgdp <= boundary[3] ))
  RRweightedpostwar[ i, 8] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[3] & RRcorr.postwar$debtgdp <= boundary[4] ))
  RRweightedpostwar[ i, 9] <- median(subset(RRcorr.postwar$rgdp_growthrate, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp >= boundary[4] ))
  RRweightedpostwar[ i, 10] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp <= boundary[2] ))
  RRweightedpostwar[ i, 11] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[2] & RRcorr.postwar$debtgdp <= boundary[3] ))
  RRweightedpostwar[ i, 12] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp > boundary[3] & RRcorr.postwar$debtgdp <= boundary[4] ))
  RRweightedpostwar[ i, 13] <- median(subset(RRcorr.postwar$inflation, RRcorr.postwar$country==RRcountries[i] & RRcorr.postwar$debtgdp >= boundary[4] ))
} # how to deal with smaller or equal to or both if all is within one command?
RRweightedpostwar <- RRweightedpostwar %>% mutate (`w<30growth` = `#<30debt/gdp` * `<30growth`) #wie RRweightedpostwar$counrtries[i] aufrufen?
RRweightedpostwar <- RRweightedpostwar %>% mutate (`w30-60growth` = `#30-60debt/gdp` * `30-60growth`)
RRweightedpostwar <- RRweightedpostwar %>% mutate (`w60-90growth` = `#60-90debt/gdp` * `60-90growth`)
RRweightedpostwar <- RRweightedpostwar %>% mutate (`w>90growth` = `#>90debt/gdp` * `>90growth`)
RRweightedpostwar <- RRweightedpostwar %>% mutate (`winflation<30` = `#<30debt/gdp` * `<30inflation`)
RRweightedpostwar <- RRweightedpostwar %>% mutate (`winflation30-60` = `#30-60debt/gdp` * `30-60inflation`)
RRweightedpostwar <- RRweightedpostwar %>% mutate (`winflation60-90` = `#60-90debt/gdp` * `60-90inflation`)
RRweightedpostwar <- RRweightedpostwar %>% mutate (`winflation>90` = `#>90debt/gdp` * `>90inflation`)
RRweightedresults <- colSums(RRweightedpostwar[ , 14:21],na.rm = TRUE)
RRresults <- colMeans(RRmedians[ , 6:13],na.rm = TRUE)

### create final datasets with results ###
RRresultsgrowth <- data.frame( RRcolnames[6:9], RRresults[1:4], RRweightedresults[1:4])
RRresultsgrowth <- gather(RRresultsgrowth, key="Results", value="values", 2:3)
RRresultsgrowth <- rename(RRresultsgrowth,  boundaries = RRcolnames.6.9.)
RRresultsgrowth$boundaries <- sub("growth", "", RRresultsgrowth[,1])
RRresultsgrowth$Results <- sub("RRresults.1.4.", "Means of medians", RRresultsgrowth[,2])
RRresultsgrowth$Results <- sub("RRweightedresults.1.4.", "Weighted average of medians", RRresultsgrowth[,2])

RRresultsinfl <- data.frame( RRcolnames[10:13], RRresults[5:8], RRweightedresults[5:8])
RRresultsinfl <- gather(RRresultsinfl, key="Results", value="values", 2:3)
RRresultsinfl <- rename(RRresultsinfl,  boundaries = RRcolnames.10.13.)
RRresultsinfl$boundaries <- sub("inflation", "", RRresultsinfl[,1])
RRresultsinfl$Results <- sub("RRresults.5.8.", "Means of medians", RRresultsinfl[,2])
RRresultsinfl$Results <- sub("RRweightedresults.5.8.", "Weighted average of medians", RRresultsinfl[,2])

### plots ###
noobservationsRR <- colSums(RRmedians[2:5])
png(filename = "RRgrowthmedian.png", units="cm", width=15, height=15, res=300)
ggplot(RRresultsgrowth, aes(x=fct_inorder(boundaries), y=values, fill = Results)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x="Public debt / GDP ratio in %",y="median real gdp growth rate",title="Median growth rates: Reinhart & Rogoff corrected dataset") +
  ylim(0, 9)
dev.off()

png(filename = "RRinflmedian.png", units="cm", width=15, height=15, res=300)
ggplot(RRresultsinfl, aes(x=fct_inorder(boundaries), y=values, fill = Results)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x="Public debt / GDP ratio in %",y="median inflation rate",title="Median inflation rates: Reinhart & Rogoff corrected dataset") +
  ylim(0, 9)
dev.off()


#################################### JST R5 dataset calculation #################################### 

# JST.shortwide <- dcast(JST.short, year + country + iso ~ ,  , value.var="value")
JST.dataset <- read_excel("JSTdatasetR5.xlsx",sheet = 2)
#str(JST.dataset) #long dataset!
#summary(JST.dataset$year) #data ranges from 19th cent. until 2017
JST.short <- JST.dataset %>% select(c(year, country, iso, pop, rgdppc, debtgdp, cpi ))
JST.short$rgdp <- JST.short$rgdppc*JST.short$pop*1000 # create real gdp column
JSTcountries <- c(unique(JST.short$country)) # create country vector
maxyear <- setNames( rep(NA, 18), JSTcountries) 
JSTmedians <- data.frame(JSTcountries)
for (i in 1:18){
  JST.i <- subset(JST.short, country=JSTcountries[i])
  maxyear[i] <- max(JST.i$year)
} 
# maxyear #data until 2017 for all countries
### 1. post-war replication RR: 1946 - 2009 ###
#create growth rates so that growth rates are only calculated within countries, put 0 for first value 
JST.short <- JST.short %>% mutate (rgdp_growthrate = if_else(country==lag(country),(rgdp - lag(rgdp))*100/lag(rgdp),0))
JST.short <- JST.short %>% group_by(country) %>% mutate (inflation = (cpi - lag(cpi))*100/lag(cpi))
#vor mutate nach country filtern 
#create subset with data only after 1945
JST.postwar <- subset(JST.short, year > 1945 & year < 2010)
# question: what to do with NA's?
#columns needed: debt/gdp: <30, 30-60, 60-90, >90, gdp growth IF debt/gdp: <30, 30-60, 60-90, >90 
colnames <- c("country", "#<30debt/gdp", "#30-60debt/gdp", "#60-90debt/gdp", "#>90debt/gdp", "<30growth", "30-60growth", "60-90growth", ">90growth", "<30inflation", "30-60inflation", "60-90inflation", ">90inflation")
boundary <- c(0, 0.3, 0.6, 0.9)
### check if values equal to boundary are relevant ###
JSTboundary <- which(JST.postwar$debtgdp == boundary[2] || JST.postwar$debtgdp == boundary[3] || JST.postwar$debtgdp == boundary[4])
# does not matter how boundaries are considered
for (i in 1:length(JSTcountries)) {
  JSTmedians[ i, 2] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp <= boundary[2] )))
  JSTmedians[ i, 3] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[2] & JST.postwar$debtgdp <= boundary[3] )))
  JSTmedians[ i, 4] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[3] & JST.postwar$debtgdp <= boundary[4] )))
  JSTmedians[ i, 5] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp >= boundary[4] ))) 
  JSTmedians[ i, 6] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp <= boundary[2] ))
  JSTmedians[ i, 7] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[2] & JST.postwar$debtgdp <= boundary[3] ))
  JSTmedians[ i, 8] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[3] & JST.postwar$debtgdp <= boundary[4] ))
  JSTmedians[ i, 9] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp >= boundary[4] ))
  JSTmedians[ i, 10] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp <= boundary[2] ))
  JSTmedians[ i, 11] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[2] & JST.postwar$debtgdp <= boundary[3] ))
  JSTmedians[ i, 12] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[3] & JST.postwar$debtgdp <= boundary[4] ))
  JSTmedians[ i, 13] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp >= boundary[4] ))
} # how to deal with smaller or equal to or both if all is within one command?
names(JSTmedians) <- colnames



### calculate observation-weighted averages instead of medians of medians ###
JSTweighted <- JSTmedians
for (i in 1:length(JSTcountries)) {
  JSTweighted[ i, 2] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp <= boundary[2] ))/sum(JSTmedians$`#<30debt/gdp`))
  JSTweighted[ i, 3] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[2] & JST.postwar$debtgdp <= boundary[3] ))/sum(JSTmedians$`#30-60debt/gdp`))
  JSTweighted[ i, 4] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[3] & JST.postwar$debtgdp <= boundary[4] ))/sum(JSTmedians$`#60-90debt/gdp`))
  JSTweighted[ i, 5] <- c(length(which(JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp >= boundary[4] ))/sum(JSTmedians$`#>90debt/gdp`)) 
  JSTweighted[ i, 6] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp <= boundary[2] ))
  JSTweighted[ i, 7] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[2] & JST.postwar$debtgdp <= boundary[3] ))
  JSTweighted[ i, 8] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[3] & JST.postwar$debtgdp <= boundary[4] ))
  JSTweighted[ i, 9] <- median(subset(JST.postwar$rgdp_growthrate, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp >= boundary[4] ))
  JSTweighted[ i, 10] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp <= boundary[2] ))
  JSTweighted[ i, 11] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[2] & JST.postwar$debtgdp <= boundary[3] ))
  JSTweighted[ i, 12] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp > boundary[3] & JST.postwar$debtgdp <= boundary[4] ))
  JSTweighted[ i, 13] <- median(subset(JST.postwar$inflation, JST.postwar$country==JSTcountries[i] & JST.postwar$debtgdp >= boundary[4] ))
} # how to deal with smaller or equal to or both if all is within one command?

JSTweighted <- JSTweighted %>% mutate (`w<30growth` = `#<30debt/gdp` * `<30growth`) #wie JSTweighted$counrtries[i] aufrufen?
JSTweighted <- JSTweighted %>% mutate (`w30-60growth` = `#30-60debt/gdp` * `30-60growth`)
JSTweighted <- JSTweighted %>% mutate (`w60-90growth` = `#60-90debt/gdp` * `60-90growth`)
JSTweighted <- JSTweighted %>% mutate (`w>90growth` = `#>90debt/gdp` * `>90growth`)
JSTweighted <- JSTweighted %>% mutate (`winflation<30` = `#<30debt/gdp` * `<30inflation`)
JSTweighted <- JSTweighted %>% mutate (`winflation30-60` = `#30-60debt/gdp` * `30-60inflation`)
JSTweighted <- JSTweighted %>% mutate (`winflation60-90` = `#60-90debt/gdp` * `60-90inflation`)
JSTweighted <- JSTweighted %>% mutate (`winflation>90` = `#>90debt/gdp` * `>90inflation`)


### create final datasets with results
summary(JSTmedians, na.rm = TRUE) # �berblick
JSTweightedresults <- colSums(JSTweighted[ , 14:21],na.rm = TRUE) #create subset with weighted results for inflation and growth
JSTresults <- colMeans(JSTmedians[ , 6:13],na.rm = TRUE) #create subset with MoM results for inflation and growth

JSTresultsgrowth <- data.frame( colnames[6:9], JSTresults[1:4], JSTweightedresults[1:4])
JSTresultsgrowth <- gather(JSTresultsgrowth, key="Results", value="values", 2:3)
JSTresultsgrowth <- rename(JSTresultsgrowth,  boundaries = colnames.6.9.)
JSTresultsgrowth$boundaries <- sub("growth", "", JSTresultsgrowth[,1])
JSTresultsgrowth$Results <- sub("JSTresults.1.4.", "Means of medians", JSTresultsgrowth[,2])
JSTresultsgrowth$Results <- sub("JSTweightedresults.1.4.", "Weighted average of medians", JSTresultsgrowth[,2])

JSTresultsinfl <- data.frame( colnames[10:13], JSTresults[5:8], JSTweightedresults[5:8])
JSTresultsinfl <- gather(JSTresultsinfl, key="Results", value="values", 2:3)
JSTresultsinfl <- rename(JSTresultsinfl,  boundaries = colnames.10.13.)
JSTresultsinfl$boundaries <- sub("inflation", "", JSTresultsinfl[,1])
JSTresultsinfl$Results <- sub("JSTresults.5.8.", "Means of medians", JSTresultsinfl[,2])
JSTresultsinfl$Results <- sub("JSTweightedresults.5.8.", "Weighted average of medians", JSTresultsinfl[,2])

noobservationsJST <- colSums(JSTmedians[2:5])

png(filename = "JSTgrowthmedian.png", units="cm", width=15, height=15, res=300)
ggplot(JSTresultsgrowth, aes(x=fct_inorder(boundaries), y=values, fill = Results)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x="Public debt / GDP ratio in %",y="median real gdp growth rate",title="Median growth rates: Macrohistory dataset") +
  ylim(0, 9)
dev.off()

png(filename = "JSTinflmedian.png", units="cm", width=15, height=15, res=300)
ggplot(JSTresultsinfl, aes(x=fct_inorder(boundaries), y=values, fill = Results)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x="Public debt / GDP ratio in %",y="median inflation rate",title="Median inflation rates: Macrohistory dataset") +
  ylim(0, 9)
dev.off()
### other comparisons between dataesets

noobservationsJST
noobservationsRR
