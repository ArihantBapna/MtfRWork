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

#Get Losses on a monthly basis




remove(i,indem,lib,plant,varSumDb,pLoss)
AnnualClimate <- df
remove(df)
