spring  <- select(filter(CauseOfLoss, MonthOfLoss == 3 | MonthOfLoss == 4 | MonthOfLoss == 5 & Year >= 2010),c(Year,MonthOfLoss, MonthOfLossName, CauseOfLossDescription, NetPlantedAcres,PoliciesIndemnified,Liability, Indemnity))
v <- c()
y <- c()
for(i in levels(factor(spring$CauseOfLossDescription))){
  varSumDb <- select(filter(spring, CauseOfLossDescription == i), c(Indemnity, NetPlantedAcres, Year))
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
df <- data.frame("CauseOfLoss" = levels(factor(spring$CauseOfLossDescription)), "Loss/Acre" = v, title="For springs")
plot <- plot_ly(df,x=~Loss.Acre, y=~CauseOfLoss, orientation='h')
plot

remove(df,plot,spring,i,v,varSum,varSumDb,varSumLoss,x, y)
