  require(ggplot2)
  require(readxl)
  require(plyr)
  require(dplyr)
  require(tidyr) # reshape wide to long formats
  require(forcats) # reorder columns 
  
  #################################### 3b) other JST variables #################################### 
  rm(list=ls()) # clear environment
  setwd("~/") #Dokumente wd
  JST.dataset <- read_excel("JSTdatasetR5.xlsx",sheet = 2)
  JSTcountries <- c(unique(JST.dataset$country)) # create country vector
  #summary(JST.dataset$year) #data ranges from 19th cent. until 2017
  JST.dataset <- JST.dataset %>% select(c(year, country, iso, pop, rgdppc, debtgdp, cpi, gdp, bond_rate, ltrate, expenditure, rconpc, 
                                          hpnom, crisisJST, capital_tr, risky_tr, safe_tr ))
  
  JST.dataset$rgdp <- JST.dataset$rgdppc*JST.dataset$pop*1000 # create real gdp column
  JST.dataset <- JST.dataset %>% mutate (rgdp_growthrate = if_else(country==lag(country),(rgdp - lag(rgdp))*100/lag(rgdp), 0))
  JST.dataset <- JST.dataset %>% group_by(country) %>% mutate (inflation = (cpi - lag(cpi))*100/lag(cpi))
  JST.dataset$realrate <- JST.dataset$ltrate - JST.dataset$inflation # real interest rate estimate
  JST.dataset$expshare <- JST.dataset$expenditure*10 / JST.dataset$gdp # share of public expenditure on nominal GDP
  # JST.dataset$rcongrowth <- JST.dataset$rconpc/ JST.dataset$rgdppc # real consumption share of real gdp per capita - times pop or not?
  JST.dataset <- JST.dataset %>% mutate (rcongrowth = if_else(country==lag(country),(rconpc - lag(rconpc))*100/lag(rconpc), 0)) # growth rate of per capita consumption
  JST.dataset <- JST.dataset %>% mutate (hpgrowth = if_else(country==lag(country),(hpnom - lag(hpnom))*100/lag(hpnom), 0)) # housing price growth rate
  JST.dataset$weightedinvreturns <- (1/3*JST.dataset$capital_tr + 1/3*JST.dataset$risky_tr + 1/3*JST.dataset$safe_tr)*100
  #Dummies mit hilfstabellen
  JST.datahelp2 <- JST.dataset
  JST.datahelp2$rgdp_growthrate[JST.datahelp2$crisisJST == 0] <- NA;
  JST.dataset$crisisgrowth <- JST.datahelp2$rgdp_growthrate #growth rate for crisis years
  JST.datahelp3 <- JST.dataset
  JST.datahelp3$rgdp_growthrate[JST.datahelp3$crisisJST == 1] <- NA;
  JST.dataset$noncrisisgrowth <- JST.datahelp3$rgdp_growthrate #growth rate for non-crisis years
  
  
  JST.dataset <- subset(JST.dataset, year > 1945 & year < 2010)
  variables <- c("year", "country", "iso", "pop" , "debtgdp", "rgdp_growthrate", "inflation", "realrate", "expshare", "rcongrowth", "hpgrowth", "crisisgrowth", "noncrisisgrowth", "weightedinvreturns")
  JST.short2 <- JST.dataset %>% select(c(variables))
  
  # JST.short2 <- na.omit(JST.short2) # remove NA rows  
  # summary(JST.short2)
  JSTmeans <- data.frame(JSTcountries)
  # colnames <- c("country", "#<30debt/gdp", "#30-60debt/gdp", "#60-90debt/gdp", "#>90debt/gdp", "<30growth", "30-60growth", "60-90growth", ">90growth", "<30realrate", "30-60realrate", "60-90realrate", ">90realrate", "<30bondrate", "30-60bondrate", "60-90bondrate", ">90bondrate" )
  boundary <- c(0, 0.3, 0.6, 0.9)
  colnames <- c(rep(NA, 41))
  colnames[2:5]<-c("#<30debt/gdp", "#30-60debt/gdp", "#60-90debt/gdp", "#>90debt/gdp")
  JSThelp <- data.frame(JSTcountries)
  for (i in 1:length(JSTcountries)){
    JSThelp[ i, 2] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp <= boundary[2] )))
    JSThelp[ i, 3] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp > boundary[2] & JST.short2$debtgdp <= boundary[3] )))
    JSThelp[ i, 4] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp > boundary[3] & JST.short2$debtgdp <= boundary[4] )))
    JSThelp[ i, 5] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp >= boundary[4] ))) 
  }
  names(JSThelp)<-c("countries", colnames[2:5])
  for (i in 1:length(JSTcountries)) {
    JSTmeans[ i, 2] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp <= boundary[2] ))/sum(JSThelp$`#<30debt/gdp`))
    JSTmeans[ i, 3] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp > boundary[2] & JST.short2$debtgdp <= boundary[3] ))/sum(JSThelp$`#30-60debt/gdp`))
    JSTmeans[ i, 4] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp > boundary[3] & JST.short2$debtgdp <= boundary[4] ))/sum(JSThelp$`#60-90debt/gdp`))
    JSTmeans[ i, 5] <- c(length(which(JST.short2$country==JSTcountries[i] & JST.short2$debtgdp >= boundary[4] ))/sum(JSThelp$`#>90debt/gdp`)) 
        for (j in 6:length(variables)) {
    JSTmeans[i , 6+4*(j-6)]  <-  mean(subset(pull(JST.short2, variables[j]), JST.short2$country==JSTcountries[i] & JST.short2$debtgdp <= boundary[2] ))
    JSTmeans[i , 7+4*(j-6)]  <-  mean(subset(pull(JST.short2, variables[j]), JST.short2$country==JSTcountries[i] & JST.short2$debtgdp > boundary[2] & JST.short2$debtgdp <= boundary[3] )) 
    JSTmeans[i , 8+4*(j-6)]  <-  mean(subset(pull(JST.short2, variables[j]), JST.short2$country==JSTcountries[i] & JST.short2$debtgdp > boundary[3] & JST.short2$debtgdp <= boundary[4] )) 
    JSTmeans[i , 9+4*(j-6)]  <-  mean(subset(pull(JST.short2, variables[j]), JST.short2$country==JSTcountries[i] & JST.short2$debtgdp >= boundary[4] )) 
    colnames[6+4*(j-6)] <- paste0(variables[j], " <", boundary[2])
    colnames[7+4*(j-6)] <- paste0(variables[j], " >", boundary[2], "&<", boundary[3])
    colnames[8+4*(j-6)] <- paste0(variables[j], " >", boundary[3], "&<", boundary[4])
    colnames[9+4*(j-6)] <- paste0(variables[j], " >", boundary[4])
      }
  }
  names(JSTmeans)<- colnames
  JSTweightedmeans <- JSTmeans
  for (i in 1:length(JSTcountries)){
    for (j in 6:length(variables)) {
    JSTweightedmeans[i , 6+4*(j-6)] <- JSTweightedmeans[i , 6+4*(j-6)]* JSTweightedmeans[i , 2]
    JSTweightedmeans[i , 7+4*(j-6)] <- JSTweightedmeans[i , 7+4*(j-6)]* JSTweightedmeans[i , 3]
    JSTweightedmeans[i , 8+4*(j-6)] <- JSTweightedmeans[i , 8+4*(j-6)]* JSTweightedmeans[i , 4]
    JSTweightedmeans[i , 9+4*(j-6)] <- JSTweightedmeans[i , 9+4*(j-6)]* JSTweightedmeans[i , 5]
    }
  }
  
  ### create final datasets with results ###
  JSTfinal <- JSTweightedmeans[, 2:41] 
  JSTfinal <- colSums(JSTfinal, na.rm=TRUE)
  # summary(JSTmeans, na.rm=TRUE)
  
  JSTfinal <- data.frame( colnames[6:41], JSTfinal[5:40])
  JSTfinal <- rename(JSTfinal,  indicator = colnames.6.41.) # rename column 
  JSTfinal <- rename(JSTfinal,  value = JSTfinal.5.40.) # rename column 
  # JSTresultsgrowth$Results <- sub("JSTweightedresults.1.4.", "Weighted average", JSTresultsgrowth[,2])
  boundaries <- rep(c("<30", "30-60", "60-90", ">90"), 9)
  JSTfinal <- data.frame(JSTfinal, boundaries)
  JSTfinal$indicator <- sub(" <0.3", "", JSTfinal$indicator)
  JSTfinal$indicator <- sub(" >0.3&<0.6", "", JSTfinal$indicator)
  JSTfinal$indicator <- sub(" >0.6&<0.9", "", JSTfinal$indicator)
  JSTfinal$indicator <- sub(" >0.9", "", JSTfinal$indicator)
  
  dfrgdp_growthrate <- subset(JSTfinal, JSTfinal$indicator=="rgdp_growthrate")
  # dfinflation <- subset(JSTfinal, JSTfinal$indicator=="inflation")
  # dfrealrate <- subset(JSTfinal, JSTfinal$indicator=="realrate")
  # dfexpshare <- subset(JSTfinal, JSTfinal$indicator=="expshare")
  # dfrcongrowth <- subset(JSTfinal, JSTfinal$indicator=="rcongrowth")
  # dfhpgrowth <- subset(JSTfinal, JSTfinal$indicator=="hpgrowth")
  # dfcrisisgrowth <- subset(JSTfinal, JSTfinal$indicator=="crisisgrowth")
  # dfnoncrisisgrowth <- subset(JSTfinal, JSTfinal$indicator=="noncrisisgrowth")
  # dfweightedinvreturns <- subset(JSTfinal, JSTfinal$indicator=="weightedinvreturns")
  ### plotting all variables in one image ###
  # ggplot(data=NULL, aes(x=fct_inorder(boundaries), y=value, group=indicator )) + 
  #   geom_bar(data=dfrgdp_growthrate, stat="identity", color = "black", fill="lightgrey") + 
  #   geom_line(data=dfinflation, size = 1, color = "Inflation" ) +   geom_point(data=dfinflation, size = 3, color = "Inflation") +
  #   geom_line(data=dfrealrate, size = 1, color = "#E69F00") +   geom_point(data=dfrealrate, size = 3, color = "#E69F00") +
  #   geom_line(data=dfexpshare, size = 1, color = "#56B4E9") +   geom_point(data=dfexpshare, size = 3, color = "#56B4E9") +
  #   geom_line(data=dfrcongrowth, size = 1, color = "#009E73") +   geom_point(data=dfrcongrowth, size = 3, color = "#009E73") +
  #   geom_line(data=dfhpgrowth, size = 1, color = "#F0E442") +   geom_point(data=dfhpgrowth, size = 3, color = "#F0E442") +
  #   geom_line(data=dfcrisisgrowth, size = 1, color = "#0072B2") +   geom_point(data=dfcrisisgrowth, size = 3, color = "#0072B2") +
  #   geom_line(data=dfnoncrisisgrowth, size = 1, color = "#D55E00") +   geom_point(data=dfnoncrisisgrowth, size = 3, color = "#D55E00") +
  #   geom_line(data=dfweightedinvreturns, size = 1, color = "#CC79A7") +   geom_point(data=dfweightedinvreturns, size = 3, color = "#CC79A7") +
  # # p + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))
  #   labs(x="Debt ratio",y="variable values", title="Other variables Macrohistory dataset") + 
  #   ylim(0, 10) +
  #   scale_colour_manual("Legend", 
  #                       breaks = c("Inflation", "Real interest rate", "Expenditure Share", "Inflation", "Real interest rate", "Expenditure Share", "Inflation", "Inflation"),
  #                       values = c("#999999", "#E69F00","#56B4E9" , "#009E73", "#F0E442","#0072B2","#D55E00", "#CC79A7")) 
  #LOL
  
  colorblindcolors <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  JSTlines <- subset(JSTfinal, indicator != "rgdp_growthrate" & indicator != "crisisgrowth" & indicator != "noncrisisgrowth")
  
  png(filename = "JSTalternativevariables.png", units="cm", width=15, height=15, res=300)
  
  ggplot(data=JSTlines, aes(x=fct_inorder(boundaries), y=value, color=indicator, group=indicator )) + 
   geom_bar(data=dfrgdp_growthrate, stat="identity", color = "black", fill="darkgrey") + 
    geom_point(size = 4) + geom_line(size = 2) +
    scale_colour_manual(name = NULL, values=colorblindcolors, labels = c("Expenditure share","House prices growth rate","Inflation rate","Real consumption growth rate","Real interest rate", "Investment return growth rate")) + 
    scale_fill_manual( values=colorblindcolors) +
    labs(x="Public debt / GDP ratio in %",y="Growth rates in %", title="Relation Public debt and GDP growth: Additional variables") +
    # ylim(-0.5, 10) +
    scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Expenditure share in %")) +
    theme(legend.position="bottom")
  
  dev.off()
   # wichtig: säulen im text beschreiben
  
  
 summary(JST.short2, na.rm=TRUE) # 1152 Datenpunkte gesamt, 175 NA für weighted inv. returns measure
