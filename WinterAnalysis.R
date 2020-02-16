winter  <- select(filter(CauseOfLoss, MonthOfLoss == 1 | MonthOfLoss == 2 | MonthOfLoss == 12),c(Year,MonthOfLoss, MonthOfLossName, CauseOfLossDescription, NetPlantedAcres,PoliciesIndemnified,Liability, Indemnity))
v <- c()
y <- c()
for(i in levels(factor(winter$CauseOfLossDescription))){
  varSumDb <- select(filter(winter, CauseOfLossDescription == i), c(Indemnity, NetPlantedAcres, Year))
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
df <- data.frame("CauseOfLoss" = levels(factor(winter$CauseOfLossDescription)), "Loss/Acre" = v, title="For Winters")
plot <- plot_ly(df,x=~Loss.Acre, y=~CauseOfLoss, orientation='h')
plot

remove(df,plot,winter,i,v,varSum,varSumDb,varSumLoss,x, y)
