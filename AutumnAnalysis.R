autumn  <- select(filter(CauseOfLoss, MonthOfLoss == 9 | MonthOfLoss == 4 | MonthOfLoss == 5),c(Year,MonthOfLoss, MonthOfLossName, CauseOfLossDescription, NetPlantedAcres,PoliciesIndemnified,Liability, Indemnity))
v <- c()
y <- c()
for(i in levels(factor(autumn$CauseOfLossDescription))){
  varSumDb <- select(filter(autumn, CauseOfLossDescription == i), c(Indemnity, NetPlantedAcres, Year))
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
df <- data.frame("CauseOfLoss" = levels(factor(autumn$CauseOfLossDescription)), "Loss/Acre" = v, title="For autumns")
plot <- plot_ly(df,x=~Loss.Acre, y=~CauseOfLoss, orientation='h')
plot
remove(autumn,v,varSum,varSumLoss,df,varSumDb)
remove(df,plot,autumn,i,v,varSum,varSumDb,varSumLoss,x, y)
