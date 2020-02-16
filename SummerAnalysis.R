summer <- select(filter(CauseOfLoss, MonthOfLoss >= 6, MonthOfLoss <= 8),c(Year,MonthOfLoss, MonthOfLossName, CauseOfLossDescription, NetPlantedAcres,PoliciesIndemnified,Liability, Indemnity))
v <- c()
y <- c()
for(i in levels(factor(summer$CauseOfLossDescription))){
  varSumDb <- select(filter(summer, CauseOfLossDescription == i), c(Indemnity, NetPlantedAcres, Year))
  varSumLoss <- sum(varSumDb$NetPlantedAcres)
  varSum <- sum(varSumDb$Indemnity)
  if(varSumLoss > 0){
    x <- varSum / varSumLoss
  }else{
    x <- 0
  }
  v <- c(v,x)
  y <- c(y, varSumDb$Year)
}

df <- data.frame("CauseOfLoss" = levels(factor(summer$CauseOfLossDescription)), "Loss/Acre" = v, "Year" = levels(factor(y)))
summerPlot <- plot_ly(df,x=~Loss.Acre, y=~CauseOfLoss, orientation='h', fill = ~Year)
summerPlot

remove(df,summerPlot,summer,i,v,varSum,varSumDb,varSumLoss,x, y)
