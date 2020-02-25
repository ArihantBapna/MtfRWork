#Analysing trends between indemnity and anomaly values

#Get the annual Indeminities compiled
df <- data.frame("PLoss/A" = c(), "Year" = c())
for(i in levels(factor(CauseOfLoss$`Year`))){
  varSumDb <- select(filter(CauseOfLoss, `Year` == i), c(Indemnity, NetPlantedAcres, Liability))
  indem <- sum(varSumDb$Indemnity)
  lib <- sum(varSumDb$Liability)
  plant <- sum(varSumDb$NetPlantedAcres)
  
  #Get the percent losses for each year factoring in expected loss, loss incurred and adjusting for variance in acres planted
  pLoss <- (indem)/(plant)
  
  df <- rbind(df, data.frame("PLoss/A" = pLoss,"Year" = i))
}
remove(i,indem,lib,plant,varSumDb,pLoss)
AnnualClimate <- df
remove(df)

#Get Losses on a monthly basis
GlobSpring <- data.frame("Year" = c(), "Loss/Acre" = c())
GlobSummer <- data.frame("Year" = c(), "Loss/Acre" = c())
GlobAutumn <- data.frame("Year" = c(), "Loss/Acre" = c())
GlobWinter <- data.frame("Year" = c(), "Loss/Acre" = c())
for(i in levels(factor(CauseOfLoss$`Year`))){
  subDb <- select(filter(CauseOfLoss, `Year` == i), c(Indemnity, NetPlantedAcres, Liability, MonthOfLoss))
  
  Summer <- select(filter(subDb, `MonthOfLoss` >= 6, `MonthOfLoss` <=8), c(Indemnity, NetPlantedAcres, Liability))
  Autumn <- select(filter(subDb, `MonthOfLoss` >= 9, `MonthOfLoss` <=11), c(Indemnity, NetPlantedAcres, Liability))
  Winter <- select(filter(subDb, `MonthOfLoss` == 12 | `MonthOfLoss` == 1 | `MonthOfLoss` == 2), c(Indemnity, NetPlantedAcres, Liability))
  Spring <- select(filter(subDb, `MonthOfLoss` >= 3, `MonthOfLoss` <=5), c(Indemnity, NetPlantedAcres, Liability))
  
  #Winter and autumn have zeros, so beat the zeroes out of them
  lossWinter <- sum(Winter$Indemnity)/sum(Winter$NetPlantedAcres)
  if(sum(Winter$Indemnity) <= 0){
    lossWinter = -1
  }
  
  if(sum(Autumn$Indemnity) > 0){
    lossAutumn <- sum(Autumn$Indemnity)/sum(Autumn$NetPlantedAcres)
  }else{
    lossAutumn = -1
  }
  
  GlobSummer <- rbind(GlobSummer,data.frame("Year"=i,"Loss/Acre"=(sum(Summer$Indemnity)/sum(Summer$NetPlantedAcres))))
  GlobSpring <- rbind(GlobSpring,data.frame("Year"=i,"Loss/Acre"=(sum(Spring$Indemnity)/sum(Spring$NetPlantedAcres))))
  GlobAutumn <- rbind(GlobAutumn,data.frame("Year"=i,"Loss/Acre"=lossAutumn))
  GlobWinter <- rbind(GlobWinter,data.frame("Year"=i,"Loss/Acre"=lossWinter))
}

remove(i,subDb,Summer,Autumn,Winter,Spring, lossWinter,lossAutumn)

#Run this to clear
remove(GlobSpring,GlobSummer,GlobAutumn,GlobWinter)


#Seperate the temperature data seasonally
Sum <- data.frame("MinAnom" = c(), "MaxAnom" = c(), "AvgVal" = c())
Spr <- data.frame("MinAnom" = c(), "MaxAnom" = c(), "AvgVal" = c())
Aut <- data.frame("MinAnom" = c(), "MaxAnom" = c(), "AvgVal" = c())
Win <- data.frame("MinAnom" = c(), "MaxAnom" = c(), "AvgVal" = c())

for(i in levels(factor(TempData$Date))){
  yearDat <-  select(filter(TempData, `Date` == i), c(Value, Month, Anomaly))
  print(i)
  SumTemp<- select(filter(yearDat, `Month` >= 6, `Month` <=8), c(Value,Anomaly))
  minSumAnom <- min(SumTemp$Anomaly)
  maxSumAnom <- max(SumTemp$Anomaly)
  avgSumVal <- mean(SumTemp$Value)
  
  Sum <- rbind(Sum,data.frame(minSumAnom,maxSumAnom,avgSumVal))
  
  SprTemp<- select(filter(yearDat, `Month` >= 9, `Month` <=11), c(Value,Anomaly))
  minSprAnom <- min(SprTemp$Anomaly)
  maxSprAnom <- max(SprTemp$Anomaly)
  avgSprVal <- mean(SprTemp$Value)

  Spr <- rbind(Spr,data.frame(minSprAnom,maxSprAnom,avgSprVal))
  
  AutTemp<- select(filter(yearDat, `Month` == 12 | `Month` == 1 | `Month` == 2), c(Value,Anomaly))
  minAutAnom <- min(AutTemp$Anomaly)
  maxAutAnom <- max(AutTemp$Anomaly)
  avgAutVal <- mean(AutTemp$Value)
  
  Aut <- rbind(Aut,data.frame(minAutAnom,maxAutAnom,avgAutVal))
  
  
  WinTemp<- select(filter(yearDat, `Month` >= 3, `Month` <=5), c(Value,Anomaly))
  minWinAnom <- min(WinTemp$Anomaly)
  maxWinAnom <- max(WinTemp$Anomaly)
  avgWinVal <- mean(WinTemp$Value)
  
  Win <- rbind(Win,data.frame(minWinAnom,maxWinAnom,avgWinVal))
  
  

}

remove(avgWinVal,maxSumAnom,minSumAnom,SumTemp,yearDat,i,avgSumVal,maxSprAnom,SprTemp,minSprAnom,maxWinAnom,minWinAnom,WinTemp,avgAutVal,maxAutAnom,minAutAnom,AutTemp,avgSprVal)
remove(Aut,Spr,Win,Sum)
